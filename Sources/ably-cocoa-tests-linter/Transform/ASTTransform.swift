import SwiftSyntax

struct ASTTransform {
    var options: TransformQuickSpec.Options

    func transformClassDeclaration(_ classDeclaration: AST.ClassDeclaration)
        -> ClassTransformationResult
    {
        let className = classDeclaration.syntax.identifier.text
        let transformationResults = classDeclaration.items
            .map { transformClassDeclarationItem($0, className: className) }

        return ClassTransformationResult(
            globalDeclarations: transformationResults.flatMap(\.globalDeclarations),
            classDeclaration: classDeclaration
                .replacingItems(with: transformationResults.flatMap(\.replacementItems))
        )
    }

    private func transformClassDeclarationItem(_ item: AST.ClassDeclaration.Item, className: String)
        -> ClassDeclarationItemTransformationResult
    {
        // I think the only class-level thing we want to manipulate is the `spec` function – everything else
        // we can pass through

        switch item {
        case .member:
            return .init(items: [.replacementItem(item)])
        case let .spec(spec):
            let contentsTransformationResult = transformContents(
                spec.contents,
                immediatelyInsideScope: .init(className: className, topLevel: .spec(spec))
            )

            if !options.rewriteTestCode {
                let newSpec = spec
                    .replacingContents(with: contentsTransformationResult.replacementContents)
                let classItems = contentsTransformationResult.items
                    .compactMap { item -> ClassDeclarationItemTransformationResult.Item? in
                        switch item {
                        case .replacementItem: return nil // handled in replacingContents above
                        case let .classDeclarationItem(item): return .replacementItem(item)
                        case let .globalDeclaration(decl): return .globalDeclaration(decl)
                        }
                    }
                return .init(items: classItems + [.replacementItem(.spec(newSpec))])
            } else {
                let classItems = contentsTransformationResult.items
                    .compactMap { item -> ClassDeclarationItemTransformationResult.Item? in
                        switch item {
                        case let .replacementItem(item):
                            if let classLevelFallback = item.classLevelFallback {
                                return .replacementItem(classLevelFallback)
                            }
                            fatalError(
                                "Transformation of `spec` gave replacementItem without a classLevelFallback; don’t know what to do with it"
                            )
                        case let .classDeclarationItem(item): return .replacementItem(item)
                        case let .globalDeclaration(decl): return .globalDeclaration(decl)
                        }
                    }
                return .init(items: classItems)
            }
        }
    }

    private func transformContents(
        _ contents: [AST.ScopeLevel.Item],
        immediatelyInsideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        return contents.map { item -> ScopeLevelItemTransformationResult in
            // TODO: what if there's stuff that clashes? especially once we shift things around in scope
            // e.g. `name` property on test case

            switch item {
            case .variableDeclaration where scope.isReusableTests:
                // it's not a function call's closure we're inside, it's a function body with local variables, which will remain local
                return .init(replacementItem: item)
            case .functionDeclaration where scope.isReusableTests:
                // it's not a function call's closure we're inside, it's a function body with local functions, which will remain local
                // TODO: see if we actually have any functions like this in our codebase
                return .init(replacementItem: item)
            case let .variableDeclaration(variableDecl):
                if !options.rewriteLocalsToGlobals {
                    return .init(replacementItem: item)
                }
                // Variable declarations in the body of a trailing closure passed to spec / describe etc
                // get hoisted to private global variables
                let transformedVariableDeclaration = SyntaxManipulationHelpers
                    .transformToPrivateGlobal(variableDecl)
                return .init(globalDeclaration: transformedVariableDeclaration)
            case let .functionDeclaration(functionDecl):
                if !options.rewriteLocalsToGlobals {
                    return .init(replacementItem: item)
                }
                // Function declarations in the body of a trailing closure passed to spec / describe etc
                // get hoisted to private global functions
                let transformedFunctionDeclaration = SyntaxManipulationHelpers
                    .transformToPrivateGlobal(functionDecl)
                return .init(globalDeclaration: transformedFunctionDeclaration)
            case let .structDeclaration(structDecl):
                if !options.rewriteLocalsToGlobals {
                    return .init(replacementItem: item)
                }
                // Struct declarations just get hoisted outside of spec()
                // We only have one of these in Ably at time of writing
                return .init(classLevelDeclaration: structDecl)
            case let .reusableTestsDeclaration(reusableTestsDecl):
                return transformReusableTestsDeclaration(
                    reusableTestsDecl,
                    className: scope.className
                )
            case let .describeOrContext(describeOrContext):
                return transformDescribeOrContext(describeOrContext, insideScope: scope)
            case let .it(it):
                return transformIt(it, insideScope: scope)
            case let .reusableTestsCall(reusableTestsCall):
                return transformReusableTestsCall(reusableTestsCall, insideScope: scope)
            case let .hook(hook):
                return transformHook(hook, insideScope: scope)
            case .arbitrarySyntax:
                preconditionFailure("Not expecting to receive .arbitrarySyntax in input")
            }
        }
        .reduce(.empty) { $0.appending($1) }
    }

    private func transformReusableTestsDeclaration(
        _ reusableTestsDecl: AST.ScopeLevel
            .ReusableTestsDeclaration,
        className: String
    ) -> ScopeLevelItemTransformationResult {
        // TODO: this method is a huge mess, let's tidy it up
        // TODO: let's emit a warning when this returns no test cases? probably means we unrolled a loop incorrectly
        // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
        // but we preserve the containing function and make it also invoke all of the test cases

        let transformationResult = transformContents(
            reusableTestsDecl.contents,
            immediatelyInsideScope: .init(
                className: className,
                topLevel: .reusableTestsDeclaration(reusableTestsDecl)
            )
        )

        // We now add the switch statement which invokes one
        // of these functions depending on which `testCase` argument was passed.
        let reusableTestCaseEnum = transformationResult.reusableTestCaseEnum(for: reusableTestsDecl)
        let switchStatement = SyntaxManipulationHelpers
            .makeReusableTestCaseInvocationSwitchStatement(fromEnum: reusableTestCaseEnum)
        let switchStatementCodeBlockItem = SyntaxFactory.makeCodeBlockItem(
            item: Syntax(switchStatement),
            semicolon: nil,
            errorTokens: nil
        )

        let newReusableTestsDecl = reusableTestsDecl
            .replacingContents(with: transformationResult.replacementContents)
        var newFunctionDeclaration = newReusableTestsDecl.syntax

        if options.rewriteTestCode {
            newFunctionDeclaration.body!.statements = newFunctionDeclaration.body!.statements
                .appending(switchStatementCodeBlockItem)
        }

        if options.rewriteTestCode {
            newFunctionDeclaration = SyntaxManipulationHelpers
                .addingTestCaseArgumentToReusableTestsFunctionDeclaration(
                    newFunctionDeclaration,
                    testCaseEnum: reusableTestCaseEnum
                )
            newFunctionDeclaration =
                SyntaxManipulationHelpers
                    .addingContextToReusableTestsFunctionDeclaration(newFunctionDeclaration)
        }

        var items: [ScopeLevelItemTransformationResult.Item] = [
            .replacementItem(.init(
                item: .functionDeclaration(newFunctionDeclaration),
                canLiftToHigherScope: true,
                classLevelFallback: .init(decl: newFunctionDeclaration)
            )),
        ]

        if options.rewriteTestCode {
            let enumDeclaration = SyntaxManipulationHelpers
                .makeEnumDeclaration(fromEnum: reusableTestCaseEnum)
            let item = ScopeLevelItemTransformationResult.Item.replacementItem(.init(
                item: .arbitrarySyntax(Syntax(enumDeclaration)),
                canLiftToHigherScope: true,
                classLevelFallback: .init(decl: enumDeclaration)
            ))
            items.insert(item, at: 0)
        }

        // TODO: we should probably still make use of the transformationResult, in case it spat out globals/members
        // (although I know it didn't)
        return .init(
            items: items
        )
    }

    private func transformDescribeOrContext(
        _ describeOrContext: AST.ScopeLevel.DescribeOrContext,
        insideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        var transformationResult = transformContents(
            describeOrContext.contents,
            immediatelyInsideScope: scope
                .appending(AST.ScopeLevel.describeOrContext(describeOrContext))
        )

        if !options.rewriteTestCode {
            let itemsWithoutReplacementContents = { (result: ScopeLevelItemTransformationResult) in
                result.items.filter { item in
                    if case .replacementItem = item {
                        return false
                    } else {
                        return true
                    }
                }
            }

            let newDescribeOrContext = describeOrContext
                .replacingContents(with: transformationResult.replacementContents)
            // TODO: this could be neater; we probably should have kept globalVariables separate from the other two

            transformationResult
                .items = itemsWithoutReplacementContents(transformationResult) +
                [.replacementItem(.init(item: .describeOrContext(newDescribeOrContext),
                                        canLiftToHigherScope: false))]
        } else {
            // TODO: This is so clunky, interacting with this transformationResult.items
            let firstClassDeclarationItemResults = transformationResult.items.enumerated()
                .first { item in
                    if case .classDeclarationItem = item.element {
                        return true
                    }
                    return false
                }

            if let firstClassDeclarationItemResults = firstClassDeclarationItemResults {
                if case let .classDeclarationItem /* I already checked this first bit above, sigh */ (.member(syntax)) =
                    firstClassDeclarationItemResults.element
                {
                    var newSyntax = syntax
                    newSyntax.leadingTrivia = describeOrContext.syntax.leadingTrivia! + newSyntax
                        .leadingTrivia!
                    transformationResult
                        .items[firstClassDeclarationItemResults.offset] =
                        .classDeclarationItem(.member(newSyntax))
                }

                let unliftableReplacementContents = transformationResult.replacementContents
                    .filter { !$0.canLiftToHigherScope }

                precondition(
                    unliftableReplacementContents.isEmpty,
                    "I expect unliftableReplacementContents to be empty when replacing a describeOrContext, but it contains \(unliftableReplacementContents)"
                )
            }
        }

        return transformationResult
    }

    // TODO: DRY up with transformIt
    private func transformReusableTestsCall(
        _ reusableTestsCall: AST.ScopeLevel.Item.ReusableTestsCall,
        insideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        if !options.rewriteTestCode {
            return .init(replacementItem: .reusableTestsCall(reusableTestsCall))
        }

        guard let reusableTestsDeclaration = scope
            .findReusableTestsDeclaration(forCall: reusableTestsCall)
        else {
            preconditionFailure(
                "Failed to find reusable tests declaration for call \(reusableTestsCall)"
            )
        }
        let transformationResult = transformContents(
            reusableTestsDeclaration.contents,
            immediatelyInsideScope: .init(
                className: scope.className,
                topLevel: .reusableTestsDeclaration(reusableTestsDeclaration)
            )
        )

        let reusableTestCaseEnum = transformationResult
            .reusableTestCaseEnum(for: reusableTestsDeclaration)

        // this reusableTests* function call gets turned into a method
        let reusableTestsFunctionCallWrapperFunctionDeclaration = SyntaxManipulationHelpers
            .makeReusableTestsFunctionCallWrapperFunctionDeclaration(
                forCall: reusableTestsCall,
                insideScope: scope,
                reusableTestCaseEnum: reusableTestCaseEnum
            )

        let caseInvocationFunctionDeclarations = reusableTestCaseEnum.cases
            .map { enumCase -> FunctionDeclSyntax in
                SyntaxManipulationHelpers.makeCaseInvocationFunctionDeclaration(
                    forCase: enumCase,
                    call: reusableTestsCall,
                    insideScope: scope
                )
            }

        // TODO: I guess technically it could be nested inside another reusable tests
        return .init(items: [.classDeclarationItem(.init(decl: reusableTestsFunctionCallWrapperFunctionDeclaration))] +
            caseInvocationFunctionDeclarations.map { decl in
                .classDeclarationItem(.init(decl: decl))
            })
    }

    private func transformIt(
        _ it: AST.ScopeLevel.Item.It,
        insideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        if !options.rewriteTestCode {
            return .init(replacementItem: .it(it))
        }

        // `it` gets turned into a method

        let testDescription = QuickSpecMethodCall.getFunctionArgument(it.syntax)

        let methodName = QuickSpecMethodCall.it(
            testDescription: testDescription,
            skipped: it.skipped
        )
        .outputFunctionName(inScope: scope)

        // Now we grab the trailing closure from the call to `it` and use that as the new test method's body

        guard let trailingClosure = it.syntax.trailingClosure else {
            preconditionFailure("I expect a call to `it` to have a trailing closure")
        }

        guard trailingClosure.signature == nil else {
            preconditionFailure(
                "I don't expect the trailing closure to have any signature, but got \(trailingClosure)"
            )
        }

        // Insert a call to the before/afterEach of the scope this `it` is contained within.
        let newStatements: CodeBlockItemListSyntax = {
            var newStatements = trailingClosure.statements

            // TODO: DRY these up with the beforeEach / afterEach ancestor-calling code

            // beforeEach
            if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                ofType: .beforeEach,
                includeSelf: true
            ) {
                let functionCall = SyntaxFactory.makeFunctionCallExpr(
                    calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                        identifier: SyntaxFactory
                            .makeToken(
                                .identifier(hookSource.outputFunctionName),
                                presence: .present
                            ),
                        declNameArguments: nil
                    )),
                    leftParen: SyntaxFactory.makeLeftParenToken(),
                    argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                    rightParen: SyntaxFactory.makeRightParenToken(),
                    trailingClosure: nil,
                    additionalTrailingClosures: nil
                ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                newStatements = newStatements
                    .prepending(SyntaxFactory
                        .makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil,
                                           errorTokens: nil))
            }

            // afterEach
            if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                ofType: .afterEach,
                includeSelf: true
            ) {
                let functionCall = SyntaxFactory.makeFunctionCallExpr(
                    calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                        identifier: SyntaxFactory
                            .makeToken(
                                .identifier(hookSource.outputFunctionName),
                                presence: .present
                            ),
                        declNameArguments: nil
                    )),
                    leftParen: SyntaxFactory.makeLeftParenToken(),
                    argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                    rightParen: SyntaxFactory.makeRightParenToken(),
                    trailingClosure: nil,
                    additionalTrailingClosures: nil
                ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                newStatements = newStatements
                    .appending(SyntaxFactory
                        .makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil,
                                           errorTokens: nil))
            }

            return newStatements
        }()

        let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
            attributes: nil,
            modifiers: nil,
            funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(methodName),
            genericParameterClause: nil,
            signature: SyntaxFactory.makeFunctionSignature(
                input: SyntaxFactory
                    .makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(),
                                         parameterList: SyntaxFactory
                                             .makeBlankFunctionParameterList(
                                             ),
                                         rightParen: SyntaxFactory.makeRightParenToken()),
                asyncOrReasyncKeyword: nil,
                throwsOrRethrowsKeyword: nil,
                output: nil
            ),
            genericWhereClause: nil,
            body: SyntaxFactory.makeCodeBlock(
                leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)),
                statements: newStatements,
                rightBrace: trailingClosure.rightBrace
            )
        ).withLeadingTrivia(it.syntax.leadingTrivia!)
            .withTrailingTrivia(it.syntax.trailingTrivia!)

        if scope.isReusableTests {
            return .init(replacementItem: .functionDeclaration(testFunctionDeclaration))
        } else {
            return .init(classLevelDeclaration: testFunctionDeclaration)
        }
    }

    private func transformHook(
        _ hook: AST.ScopeLevel.Item.Hook,
        insideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        var newHook = hook

        if options.addLogging {
            // To be able to compare before & after code, the logging needs to only surround
            // the original hook (i.e. exclude calls to parent context hooks)
            newHook = addingLogging(toHook: newHook, insideScope: scope)
        }

        if !options.rewriteTestCode {
            // TODO: sort this out for the rewrite case (we want the logging to be outmost)
            return .init(replacementItem: .hook(newHook))
        }

        // `beforeEach` or `afterEach` gets turned into a method

        let methodCall = QuickSpecMethodCall(functionCallExpr: newHook.syntax)
        let methodName = methodCall.outputFunctionName(inScope: scope)

        // Now we grab the trailing closure from the call to `before/afterEach` and use that as the new test method's body
        // TODO: we can probably DRY this up with the `it` equivalent

        guard let trailingClosure = newHook.syntax.trailingClosure else {
            preconditionFailure("I expect a call to `before/afterEach` to have a trailing closure")
        }

        guard trailingClosure.signature == nil else {
            preconditionFailure(
                "I don't expect the trailing closure to have any signature, but got \(trailingClosure)"
            )
        }

        // Insert a call to the before/afterEach of the scope this one is nested within.
        let newStatements: CodeBlockItemListSyntax = {
            switch methodCall {
            // TODO: double-check the ordering of the before / after in relation to parents
            case .hook(.beforeEach):
                var newStatements: CodeBlockItemListSyntax = trailingClosure.statements

                if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                    ofType: .beforeEach,
                    includeSelf: false
                ) {
                    let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                        calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                            identifier: SyntaxFactory
                                .makeToken(
                                    .identifier(hookSource.outputFunctionName),
                                    presence: .present
                                ),
                            declNameArguments: nil
                        )),
                        leftParen: SyntaxFactory.makeLeftParenToken(),
                        argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                        rightParen: SyntaxFactory.makeRightParenToken(),
                        trailingClosure: nil,
                        additionalTrailingClosures: nil
                    ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                    newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(
                        item: Syntax(ancestorFunctionCall),
                        semicolon: nil,
                        errorTokens: nil
                    ))
                }

                if scope.isReusableTests {
                    let functionName = "context.beforeEach?"

                    let contextFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                        calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                            identifier: SyntaxFactory
                                .makeToken(.identifier(functionName), presence: .present),
                            declNameArguments: nil
                        )),
                        leftParen: SyntaxFactory.makeLeftParenToken(),
                        argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                        rightParen: SyntaxFactory.makeRightParenToken(),
                        trailingClosure: nil,
                        additionalTrailingClosures: nil
                    ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                    newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(
                        item: Syntax(contextFunctionCall),
                        semicolon: nil,
                        errorTokens: nil
                    ))
                }

                return newStatements
            case .hook(.afterEach):
                var newStatements: CodeBlockItemListSyntax = trailingClosure.statements

                if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                    ofType: .afterEach,
                    includeSelf: false
                ) {
                    let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                        calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                            identifier: SyntaxFactory
                                .makeToken(
                                    .identifier(hookSource.outputFunctionName),
                                    presence: .present
                                ),
                            declNameArguments: nil
                        )),
                        leftParen: SyntaxFactory.makeLeftParenToken(),
                        argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                        rightParen: SyntaxFactory.makeRightParenToken(),
                        trailingClosure: nil,
                        additionalTrailingClosures: nil
                    ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                    newStatements = trailingClosure.statements
                        .appending(SyntaxFactory.makeCodeBlockItem(
                            item: Syntax(ancestorFunctionCall),
                            semicolon: nil,
                            errorTokens: nil
                        ))
                }

                if scope.isReusableTests {
                    let functionName = "context.afterEach?"

                    let contextFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                        calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                            identifier: SyntaxFactory
                                .makeToken(.identifier(functionName), presence: .present),
                            declNameArguments: nil
                        )),
                        leftParen: SyntaxFactory.makeLeftParenToken(),
                        argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                        rightParen: SyntaxFactory.makeRightParenToken(),
                        trailingClosure: nil,
                        additionalTrailingClosures: nil
                    ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                    newStatements = newStatements.appending(SyntaxFactory.makeCodeBlockItem(
                        item: Syntax(contextFunctionCall),
                        semicolon: nil,
                        errorTokens: nil
                    ))
                }

                return newStatements
            default: fatalError("unexpected methodCall")
            }
        }()

        // we do actually have one example of a propertly nested beforeEach – see "State WaitingForDeregistration" in PushActivationStateMachine tests. not sure we have any afterEach

        let hookFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
            attributes: nil,
            modifiers: nil,
            funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(methodName),
            genericParameterClause: nil,
            signature: SyntaxFactory.makeFunctionSignature(
                input: SyntaxFactory
                    .makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(),
                                         parameterList: SyntaxFactory
                                             .makeBlankFunctionParameterList(
                                             ),
                                         rightParen: SyntaxFactory.makeRightParenToken()),
                asyncOrReasyncKeyword: nil,
                throwsOrRethrowsKeyword: nil,
                output: nil
            ),
            genericWhereClause: nil,
            body: SyntaxFactory.makeCodeBlock(
                leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)),
                statements: newStatements,
                rightBrace: trailingClosure.rightBrace
            )
        ).withLeadingTrivia(newHook.syntax.leadingTrivia!)
            .withTrailingTrivia(newHook.syntax.trailingTrivia!)

        if scope.isReusableTests {
            return .init(replacementItem: .functionDeclaration(hookFunctionDeclaration))
        } else {
            return .init(classLevelDeclaration: hookFunctionDeclaration)
        }
    }

    private func addingLogging(
        toHook hook: AST.ScopeLevel.Item.Hook,
        insideScope scope: AST.Scope
    ) -> AST.ScopeLevel.Item.Hook {
        var newHook = hook

        let startFunctionCallExpr = createLoggingFunctionCallExpr(
            forHookType: hook.hookType,
            insideScope: scope,
            disposition: .atStart
        )
        let endFunctionCallExpr = createLoggingFunctionCallExpr(
            forHookType: hook.hookType,
            insideScope: scope,
            disposition: .atEnd
        )

        var newStatements = newHook.syntax.trailingClosure!.statements
        newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(
            item: Syntax(startFunctionCallExpr),
            semicolon: nil,
            errorTokens: nil
        ))
        newStatements = newStatements.appending(SyntaxFactory.makeCodeBlockItem(
            item: Syntax(endFunctionCallExpr),
            semicolon: nil,
            errorTokens: nil
        ))

        newHook.syntax.trailingClosure!.statements = newStatements

        return newHook
    }

    private enum LogStatementDisposition {
        case atStart
        case atEnd

        var loggingDescription: String {
            switch self {
            case .atStart: return "START"
            case .atEnd: return "END"
            }
        }
    }

    private func createLoggingFunctionCallExpr(
        forHookType hookType: HookType,
        insideScope scope: AST.Scope,
        disposition: LogStatementDisposition
    ) -> FunctionCallExprSyntax {
        let functionName = QuickSpecMethodCall.hook(hookType).outputFunctionName(inScope: scope)

        let argumentExpr = ExprSyntax(SyntaxFactory
            .makeStringLiteralExpr(
                "\(disposition.loggingDescription) HOOK: \(scope.className).\(functionName)"
            ))

        let argumentList = SyntaxFactory.makeTupleExprElementList([
            SyntaxFactory.makeTupleExprElement(
                label: nil,
                colon: nil,
                expression: argumentExpr,
                trailingComma: nil
            ),
        ])

        return SyntaxFactory.makeFunctionCallExpr(
            calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                identifier: SyntaxFactory.makeIdentifier("print"),
                declNameArguments: nil
            )),
            leftParen: SyntaxFactory.makeLeftParenToken(),
            argumentList: argumentList,
            rightParen: SyntaxFactory.makeRightParenToken(),
            trailingClosure: nil,
            additionalTrailingClosures: nil
        ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
    }
}
