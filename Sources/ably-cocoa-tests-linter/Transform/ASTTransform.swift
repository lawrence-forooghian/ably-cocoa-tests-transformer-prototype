import SwiftSyntax

struct ASTTransform {
    struct ClassTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclaration: AST.ClassDeclaration
    }

    var options: TransformQuickSpec.Options

    func transformClassDeclaration(_ classDeclaration: AST.ClassDeclaration)
        -> ClassTransformationResult
    {
        let transformationResults = classDeclaration.items.map(transformClassDeclarationItem)

        return ClassTransformationResult(
            globalDeclarations: transformationResults.flatMap(\.globalDeclarations),
            classDeclaration: classDeclaration
                .replacingItems(transformationResults.flatMap(\.classDeclarationItems))
        )
    }

    private func transformClassDeclarationItem(_ item: AST.ClassDeclaration.Item)
        -> ItemTransformationResult
    {
        // I think the only class-level thing we want to manipulate is the `spec` function – everything else
        // we can pass through

        switch item {
        case .member:
            return .init(item)
        case let .spec(spec):
            return transformContents(
                spec.contents,
                immediatelyInsideScope: .init(topLevel: .spec(spec))
            )
        }
    }

    private func transformContents(
        _ contents: [AST.ScopeLevel.Item],
        immediatelyInsideScope scope: AST.Scope
    ) -> ItemTransformationResult {
        return contents.map { item -> ItemTransformationResult in
            // TODO: what if there's stuff that clashes? especially once we shift things around in scope
            // e.g. `name` property on test case

            switch item {
            case let .variableDeclaration(variableDecl) where scope.isReusableTests:
                // it's not a function call's closure we're inside, it's a function body with local variables, which will remain a function, so can keep its variables intact
                // TODO: this is wildly misleading - that it says class level declaration when it's not
                return .init(classLevelDeclaration: variableDecl)
            case let .functionDeclaration(functionDecl) where scope.isReusableTests:
                // it's not a function call's closure we're inside, it's a function body with local functions, which will remain a function, so can keep its functions intact
                // TODO: see if we actually have any of this in our codebase
                return .init(classLevelDeclaration: functionDecl)
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
        .ReusableTestsDeclaration) -> ItemTransformationResult
    {
        // TODO: this method is a huge mess, let's tidy it up
        // TODO: let's emit a warning when this returns no test cases? probably means we unrolled a loop incorrectly
        // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
        // but we preserve the containing function and make it also invoke all of the test cases

        let classDeclarationItems = transformContents(
            reusableTestsDecl.contents,
            immediatelyInsideScope: .init(topLevel: .reusableTestsDeclaration(reusableTestsDecl))
        ).classDeclarationItems

        // wait, no, we'll keep it as a function call

        var newFunctionDeclaration = reusableTestsDecl.syntax

        // TODO: this is also probably hacky - we're taking a bunch of class member items and turning them back into just declarations. Not necessary
        let codeBlockItemsFromFunction = classDeclarationItems.map { classDeclarationItem in
            SyntaxFactory.makeCodeBlockItem(
                item: Syntax(classDeclarationItem.syntax.decl),
                semicolon: nil,
                errorTokens: nil
            )
        }

        // TODO: this is a bit hacky – we're essentially figuring out which
        // test methods were created by transformSpecFunctionDeclarationIntoClassLevelDeclarations
        // but we could probably just improve things to make it tell us that
        let testFunctionDeclarations = classDeclarationItems
            .compactMap { classDeclarationItem -> FunctionDeclSyntax? in
                guard let functionDeclaration = classDeclarationItem.syntax.decl
                    .as(FunctionDeclSyntax.self)
                else {
                    return nil
                }
                if !functionDeclaration.identifier.text.starts(with: "test") {
                    // TODO: should we handle skipped functions in some nicer way?
                    return nil
                }
                return functionDeclaration
            }

        // We now invoke all of these functions.
        let testFunctionInvocationCodeBlockItems = testFunctionDeclarations
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

        // TODO: should we stick some logging into these test functions or something?

        newFunctionDeclaration.body!.statements = SyntaxFactory
            .makeCodeBlockItemList(codeBlockItemsFromFunction +
                testFunctionInvocationCodeBlockItems)

        newFunctionDeclaration =
            SyntaxManipulationHelpers
                .addingContextToReusableTestsFunctionDeclaration(newFunctionDeclaration)

        return .init(classLevelDeclaration: newFunctionDeclaration)
    }

    private func transformDescribeOrContext(
        _ describeOrContext: AST.ScopeLevel.DescribeOrContext,
        insideScope scope: AST.Scope
    ) -> ItemTransformationResult {
        var transformationResult = transformContents(
            describeOrContext.contents,
            immediatelyInsideScope: scope
                .appending(AST.ScopeLevel.describeOrContext(describeOrContext))
        )
        if !transformationResult.classDeclarationItems.isEmpty {
            // preserve any comments that came alongside the function call
            // TODO: it's a bit messed up though, see e.g. "32 bytes" comment
            // and a bunch of unwanted whitespace
            if case let .member(syntax) = transformationResult.classDeclarationItems[0] {
                var newSyntax = syntax
                newSyntax.leadingTrivia = describeOrContext.syntax.leadingTrivia! + newSyntax
                    .leadingTrivia!
                transformationResult.classDeclarationItems[0] = .member(newSyntax)
            }
        }
        return transformationResult
    }

    // TODO: DRY up with transformIt
    private func transformReusableTestsCall(
        _ reusableTestsCall: AST.ScopeLevel.Item.ReusableTestsCall,
        insideScope scope: AST.Scope
    ) -> ItemTransformationResult {
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

        return .init(classLevelDeclaration: testFunctionDeclaration)
    }

    private func transformIt(
        _ it: AST.ScopeLevel.Item.It,
        insideScope scope: AST.Scope
    ) -> ItemTransformationResult {
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

        return .init(classLevelDeclaration: testFunctionDeclaration)
    }

    private func transformHook(
        _ hook: AST.ScopeLevel.Item.Hook,
        insideScope scope: AST.Scope
    ) -> ItemTransformationResult {
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

        return .init(classLevelDeclaration: hookFunctionDeclaration)
    }
}
