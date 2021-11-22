import SwiftSyntax

struct ASTTransform {
    var options: TransformQuickSpec.Options

    func transformClassDeclaration(_ classDeclaration: AST.ClassDeclaration)
        -> ClassTransformationResult
    {
        let transformationResults = classDeclaration.items.map(transformClassDeclarationItem)

        return ClassTransformationResult(
            globalDeclarations: transformationResults.flatMap(\.globalDeclarations),
            classDeclaration: classDeclaration
                .replacingItems(with: transformationResults.flatMap(\.replacementItems))
        )
    }

    private func transformClassDeclarationItem(_ item: AST.ClassDeclaration.Item)
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
                immediatelyInsideScope: .init(topLevel: .spec(spec))
            )

            if options.onlyLocalsToGlobals {
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
                // Variable declarations in the body of a trailing closure passed to spec / describe etc
                // get hoisted to private global variables
                let transformedVariableDeclaration = SyntaxManipulationHelpers
                    .transformToPrivateGlobal(variableDecl)
                return .init(globalDeclaration: transformedVariableDeclaration)
            case let .functionDeclaration(functionDecl):
                // Function declarations in the body of a trailing closure passed to spec / describe etc
                // get hoisted to private global functions
                let transformedFunctionDeclaration = SyntaxManipulationHelpers
                    .transformToPrivateGlobal(functionDecl)
                return .init(globalDeclaration: transformedFunctionDeclaration)
            case let .structDeclaration(structDecl):
                // Struct declarations just get hoisted outside of spec()
                // We only have one of these in Ably at time of writing
                return .init(classLevelDeclaration: structDecl)
            case let .reusableTestsDeclaration(reusableTestsDecl):
                return transformReusableTestsDeclaration(reusableTestsDecl)
            case let .describeOrContext(describeOrContext):
                return transformDescribeOrContext(describeOrContext, insideScope: scope)
            case let .it(it):
                return transformIt(it, insideScope: scope)
            case let .reusableTestsCall(reusableTestsCall):
                return transformReusableTestsCall(reusableTestsCall, insideScope: scope)
            case let .hook(hook):
                return transformHook(hook, insideScope: scope)
            }
        }
        .reduce(.empty) { $0.appending($1) }
    }

    private func transformReusableTestsDeclaration(_ reusableTestsDecl: AST.ScopeLevel
        .ReusableTestsDeclaration) -> ScopeLevelItemTransformationResult
    {
        // TODO: this method is a huge mess, let's tidy it up
        // TODO: let's emit a warning when this returns no test cases? probably means we unrolled a loop incorrectly
        // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
        // but we preserve the containing function and make it also invoke all of the test cases

        let transformationResult = transformContents(
            reusableTestsDecl.contents,
            immediatelyInsideScope: .init(topLevel: .reusableTestsDeclaration(reusableTestsDecl))
        )

        // TODO: this is a bit hacky – we're essentially figuring out which
        // test methods were created by transformSpecFunctionDeclarationIntoClassLevelDeclarations
        // but we could probably just improve things to make it tell us that
        let testFunctionDeclarationsWithIdentifier = transformationResult.replacementContents
            .compactMap { item -> (identifier: String, declaration: FunctionDeclSyntax)? in
                guard case let .functionDeclaration(functionDeclaration) = item.item else {
                    return nil
                }
                let identifier = functionDeclaration.identifier.text
                if !identifier.starts(with: "test") {
                    // TODO: should we handle skipped functions in some nicer way?
                    return nil
                }
                return (identifier, functionDeclaration)
            }
        
        // TODO more Swifty way of doing this? using a "comparator"?
        let sortedTestFunctionDeclarations = testFunctionDeclarationsWithIdentifier.sorted { lhs, rhs in
            lhs.identifier.caseInsensitiveCompare(rhs.identifier) == .orderedAscending
        }.map(\.declaration)

        // We now invoke all of these functions.
        // TODO: how will we represent that in our AST? They're just random function calls. That's a pain. Maybe we'll just not represent them in the AST and only in the syntax... OK, yeah, we'll do that for now. But it's a bit dodgy
        let testFunctionInvocationCodeBlockItems = sortedTestFunctionDeclarations
            .map { declaration -> CodeBlockItemSyntax in
                let testFunctionInvocationExpression = SyntaxFactory
                    .makeFunctionCallExpr(
                        calledExpression: ExprSyntax(SyntaxFactory
                            .makeIdentifierExpr(
                                identifier: declaration
                                    .identifier,
                                declNameArguments: nil
                            )),
                        leftParen: SyntaxFactory.makeLeftParenToken(),
                        argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                        rightParen: SyntaxFactory.makeRightParenToken(),
                        trailingClosure: nil,
                        additionalTrailingClosures: nil
                    ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                return SyntaxFactory.makeCodeBlockItem(
                    item: Syntax(testFunctionInvocationExpression),
                    semicolon: nil,
                    errorTokens: nil
                )
            }

        // Now we update the function declaration of newReusableTestsDecl
        // to insert calls to the test functions.
        let newReusableTestsDecl = reusableTestsDecl
            .replacingContents(with: transformationResult.replacementContents)
        var newFunctionDeclaration = newReusableTestsDecl.syntax

        // TODO: should we stick some logging into these test functions or something?

        testFunctionInvocationCodeBlockItems.forEach { item in
            newFunctionDeclaration.body!.statements = newFunctionDeclaration.body!.statements
                .appending(item)
        }

        if !options.onlyLocalsToGlobals {
            newFunctionDeclaration =
                SyntaxManipulationHelpers
                    .addingContextToReusableTestsFunctionDeclaration(newFunctionDeclaration)
        }

        // TODO: we should probably still make use of the transformationResult, in case it spat out globals/members
        // (although I know it didn't)
        return .init(
            replacementItem: .functionDeclaration(newFunctionDeclaration),
            canLiftToHigherScope: true,
            classLevelFallback: .init(decl: newFunctionDeclaration)
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

        if options.onlyLocalsToGlobals {
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
        if options.onlyLocalsToGlobals {
            return .init(replacementItem: .reusableTestsCall(reusableTestsCall))
        }

        // this reusableTests* function call gets turned into a method
        let methodName = QuickSpecMethodCall.it(
            testDescription: reusableTestsCall.calledFunctionName,
            skipped: false
        )
        .outputFunctionName(inScope: scope)

        let newFunctionCallExpr = SyntaxManipulationHelpers.addContextToReusableTestsFunctionCall(
            reusableTestsCall.syntax,
            insideScope: scope
        )

        let codeBlockItem = SyntaxFactory.makeCodeBlockItem(
            item: Syntax(newFunctionCallExpr),
            semicolon: nil,
            errorTokens: nil
        )
        let statements = SyntaxFactory.makeCodeBlockItemList([codeBlockItem])

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
                leftBrace: SyntaxFactory.makeLeftBraceToken().withLeadingTrivia(.spaces(1)),
                statements: statements,
                rightBrace: SyntaxFactory.makeRightBraceToken()
            )
        ).withLeadingTrivia(newFunctionCallExpr.leadingTrivia!)
            .withTrailingTrivia(newFunctionCallExpr.trailingTrivia!)

        // TODO: I guess technically it could be nested inside another reusable tests
        return .init(classLevelDeclaration: testFunctionDeclaration)
    }

    private func transformIt(
        _ it: AST.ScopeLevel.Item.It,
        insideScope scope: AST.Scope
    ) -> ScopeLevelItemTransformationResult {
        if options.onlyLocalsToGlobals {
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
        if options.onlyLocalsToGlobals {
            return .init(replacementItem: .hook(hook))
        }

        // `beforeEach` or `afterEach` gets turned into a method

        let methodCall = QuickSpecMethodCall(functionCallExpr: hook.syntax)
        let methodName = methodCall.outputFunctionName(inScope: scope)

        // Now we grab the trailing closure from the call to `before/afterEach` and use that as the new test method's body
        // TODO: we can probably DRY this up with the `it` equivalent

        guard let trailingClosure = hook.syntax.trailingClosure else {
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
        ).withLeadingTrivia(hook.syntax.leadingTrivia!)
            .withTrailingTrivia(hook.syntax.trailingTrivia!)

        if scope.isReusableTests {
            return .init(replacementItem: .functionDeclaration(hookFunctionDeclaration))
        } else {
            return .init(classLevelDeclaration: hookFunctionDeclaration)
        }
    }
}
