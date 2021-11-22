import SwiftSyntax

enum SyntaxManipulationHelpers {
    static func transformToPrivateGlobal(_ variableDeclaration: VariableDeclSyntax)
        -> VariableDeclSyntax
    {
        let leadingTrivia = variableDeclaration.leadingTrivia!
        var modifiedToPrivateVariableDeclaration = variableDeclaration
        modifiedToPrivateVariableDeclaration.leadingTrivia = .zero

        let oldModifiers = modifiedToPrivateVariableDeclaration.modifiers ?? SyntaxFactory
            .makeModifierList([])

        let newModifiers = oldModifiers.prepending(SyntaxFactory.makeDeclModifier(
            name: SyntaxFactory.makePrivateKeyword(),
            detailLeftParen: nil,
            detail: nil,
            detailRightParen: nil
        ).withLeadingTrivia(leadingTrivia).withTrailingTrivia(.spaces(1)))

        return modifiedToPrivateVariableDeclaration.withModifiers(newModifiers)
    }

    static func transformToPrivateGlobal(_ functionDeclaration: FunctionDeclSyntax)
        -> FunctionDeclSyntax
    {
        let leadingTrivia = functionDeclaration.leadingTrivia!
        var modifiedToPrivateFunctionDeclaration = functionDeclaration
        modifiedToPrivateFunctionDeclaration.leadingTrivia = .zero

        // TODO: DRY up this with the variable decl version of this method
        let oldModifiers = modifiedToPrivateFunctionDeclaration.modifiers ?? SyntaxFactory
            .makeModifierList([])

        let newModifiers = oldModifiers.prepending(SyntaxFactory.makeDeclModifier(
            name: SyntaxFactory.makePrivateKeyword(),
            detailLeftParen: nil,
            detail: nil,
            detailRightParen: nil
        ).withLeadingTrivia(leadingTrivia).withTrailingTrivia(.spaces(1)))

        return modifiedToPrivateFunctionDeclaration.withModifiers(newModifiers)
    }

    static func addingParameterToReusableTestsFunctionDeclaration(
        param: FunctionParameterSyntax,
        decl: FunctionDeclSyntax
    ) -> FunctionDeclSyntax {
        var parameterList = decl.signature.input.parameterList

        var hasTrailingClosure = false

        if !parameterList.isEmpty {
            let finalParam = parameterList.last!

            let finalParamBaseType: TypeSyntax?

            if let attributedType = finalParam.type?.as(AttributedTypeSyntax.self) {
                finalParamBaseType = attributedType.baseType
            } else if let _ = finalParam.type?.as(SimpleTypeIdentifierSyntax.self) {
                // we only care whether it's a closure and I suppose it's not
                finalParamBaseType = nil
            } else if let _ = finalParam.type?.as(TupleTypeSyntax.self) {
                // we only care whether it's a closure and I suppose it's not
                finalParamBaseType = nil
            } else {
                preconditionFailure("I don't know how to handle \(finalParam.type!.syntaxNodeType)")
            }

            hasTrailingClosure = finalParamBaseType?.is(FunctionTypeSyntax.self) == true
        }

        if (!hasTrailingClosure && !parameterList.isEmpty) ||
            (hasTrailingClosure && parameterList.count > 1)
        {
            // This seems like a faff, no doubt I don't know enough about
            // Swift collections
            let finalParam =
                parameterList[parameterList
                    .index(parameterList.endIndex, offsetBy: hasTrailingClosure ? -2 : -1)]

            // Add trailing comma to final param (or penultimate if has trailing closure)
            var newFinalParam = finalParam
            newFinalParam = newFinalParam.withTrailingComma(SyntaxFactory.makeCommaToken())
            parameterList = parameterList.replacing(
                childAt: parameterList.count - (hasTrailingClosure ? 2 : 1),
                with: newFinalParam
            )
        }

        var newParam = param
        newParam.trailingComma = hasTrailingClosure ? SyntaxFactory.makeCommaToken() : nil
        parameterList = parameterList.inserting(
            newParam,
            at: hasTrailingClosure ? parameterList.count - 1 : parameterList.count
        )

        var newDecl = decl
        newDecl.signature.input.parameterList = parameterList

        return newDecl
    }

    static func addingTestCaseArgumentToReusableTestsFunctionDeclaration(
        _ decl: FunctionDeclSyntax,
        testCaseEnum: ASTTransform.ScopeLevelItemTransformationResult.ReusableTestCaseEnum
    ) -> FunctionDeclSyntax {
        // We add a `testCase: TestCase_ReusableTestsTestTokenRequestFromJson` param to the reusable tests
        // declaration

        let parameterType = SyntaxFactory.makeTypeIdentifier(testCaseEnum.name)
        let parameter = SyntaxFactory.makeFunctionParameter(
            attributes: nil,
            firstName: SyntaxFactory.makeIdentifier("testCase"),
            secondName: nil,
            colon: SyntaxFactory.makeColonToken(),
            type: parameterType,
            ellipsis: nil,
            defaultArgument: nil,
            trailingComma: nil
        )

        return addingParameterToReusableTestsFunctionDeclaration(param: parameter, decl: decl)
    }

    static func addingContextToReusableTestsFunctionDeclaration(_ decl: FunctionDeclSyntax)
        -> FunctionDeclSyntax
    {
        // We add a `context: (beforeEach: () -> (), afterEach: () -> ())` arg to all these functions
        let parameterType = SyntaxFactory
            .makeTypeIdentifier("(beforeEach: (() -> ())?, afterEach: (() -> ())?)")
        let parameter = SyntaxFactory.makeFunctionParameter(
            attributes: nil,
            firstName: SyntaxFactory.makeIdentifier("context"),
            secondName: nil,
            colon: SyntaxFactory.makeColonToken(),
            type: parameterType,
            ellipsis: nil,
            defaultArgument: nil,
            trailingComma: nil
        )

        return addingParameterToReusableTestsFunctionDeclaration(param: parameter, decl: decl)
    }

    // TODO: these two that work with scope should be split between here and ASTTransform; just wanted them out of that class for now

    static func addContextToReusableTestsFunctionCall(
        _ functionCallExpr: FunctionCallExprSyntax,
        insideScope scope: AST.Scope
    )
        -> FunctionCallExprSyntax
    {
        var newFunctionCallExpr = functionCallExpr

        if newFunctionCallExpr.leftParen == nil {
            // build parens if doesn't already have them (e.g. if function call has a trailing closure and no other args)
            newFunctionCallExpr.leftParen = SyntaxFactory.makeLeftParenToken()
            newFunctionCallExpr.rightParen = SyntaxFactory.makeRightParenToken()
        }

        // add a trailing comma to current final arg
        var newArgumentList = newFunctionCallExpr.argumentList
        if !newArgumentList.isEmpty {
            let index = newArgumentList.index(newArgumentList.endIndex, offsetBy: -1)
            var newArgument = newArgumentList[index]
            newArgument = newArgument.withTrailingComma(SyntaxFactory.makeCommaToken())
            newArgumentList = newArgumentList.replacing(
                childAt: newArgumentList.count - 1,
                with: newArgument
            )
            newFunctionCallExpr.argumentList = newArgumentList
        }

        newFunctionCallExpr = newFunctionCallExpr.addArgument(SyntaxFactory.makeTupleExprElement(
            label: SyntaxFactory.makeIdentifier("context"),
            colon: SyntaxFactory.makeColonToken(),
            expression: ExprSyntax(makeContextTupleExpr(insideScope: scope)),
            trailingComma: nil
        ))
        return newFunctionCallExpr
    }

    static func makeContextTupleExpr(insideScope scope: AST.Scope) -> TupleExprSyntax {
        let beforeEachExpr: ExprSyntax
        if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
            ofType: .beforeEach,
            includeSelf: true
        ) {
            beforeEachExpr = ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                identifier: SyntaxFactory.makeIdentifier(hookSource.outputFunctionName),
                declNameArguments: nil
            ))
        } else {
            beforeEachExpr = ExprSyntax(SyntaxFactory
                .makeNilLiteralExpr(nilKeyword: SyntaxFactory.makeNilKeyword()))
        }

        let afterEachExpr: ExprSyntax
        if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
            ofType: .afterEach,
            includeSelf: true
        ) {
            afterEachExpr = ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                identifier: SyntaxFactory.makeIdentifier(hookSource.outputFunctionName),
                declNameArguments: nil
            ))
        } else {
            afterEachExpr = ExprSyntax(SyntaxFactory
                .makeNilLiteralExpr(nilKeyword: SyntaxFactory.makeNilKeyword()))
        }

        let elementList = SyntaxFactory.makeTupleExprElementList([
            SyntaxFactory.makeTupleExprElement(
                label: SyntaxFactory.makeIdentifier("beforeEach"),
                colon: SyntaxFactory.makeColonToken(),
                expression: beforeEachExpr,
                trailingComma: SyntaxFactory.makeCommaToken()
            ),
            SyntaxFactory.makeTupleExprElement(
                label: SyntaxFactory.makeIdentifier("afterEach"),
                colon: SyntaxFactory.makeColonToken(),
                expression: afterEachExpr,
                trailingComma: nil
            ),
        ])

        return SyntaxFactory.makeTupleExpr(
            leftParen: SyntaxFactory.makeLeftParenToken(),
            elementList: elementList,
            rightParen: SyntaxFactory.makeRightParenToken()
        )
    }

    static func makeReusableTestCaseInvocationSwitchStatement(fromEnum reusableTestCaseEnum: ASTTransform
        .ScopeLevelItemTransformationResult.ReusableTestCaseEnum) -> SwitchStmtSyntax
    {
        // TODO: how will we represent that in our AST? It's just a random switch statement. That's a pain. Maybe we'll just not represent them in the AST and only in the syntax... OK, yeah, we'll do that for now. But it's a bit dodgy

        // switch-case → case-label statements
        let caseListItems = reusableTestCaseEnum.cases.map { enumCase -> SwitchCaseSyntax in
            let caseItem = SyntaxFactory.makeCaseItem(
                pattern: PatternSyntax(SyntaxFactory.makeEnumCasePattern(
                    type: nil,
                    period: SyntaxFactory.makePeriodToken(),
                    caseName: SyntaxFactory.makeIdentifier(enumCase.name),
                    associatedTuple: nil
                )),
                whereClause: nil,
                trailingComma: nil
            )
            let functionCallExpression = SyntaxFactory.makeFunctionCallExpr(
                calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                    identifier: SyntaxFactory.makeIdentifier(enumCase.functionName),
                    declNameArguments: nil
                )),
                leftParen: SyntaxFactory.makeLeftParenToken(),
                argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                rightParen: SyntaxFactory.makeRightParenToken(),
                trailingClosure: nil,
                additionalTrailingClosures: nil
            ).withTrailingTrivia(.newlines(1))
            return SyntaxFactory.makeSwitchCase(
                unknownAttr: nil,
                label: Syntax(SyntaxFactory
                    .makeSwitchCaseLabel(caseKeyword: SyntaxFactory.makeCaseKeyword()
                        .withTrailingTrivia(.spaces(1)),
                        caseItems: SyntaxFactory.makeCaseItemList([caseItem]),
                        colon: SyntaxFactory.makeColonToken())),
                statements: SyntaxFactory.makeCodeBlockItemList([SyntaxFactory.makeCodeBlockItem(
                    item: Syntax(functionCallExpression),
                    semicolon: nil,
                    errorTokens: nil
                )])
            )
        }
        let caseList = SyntaxFactory.makeSwitchCaseList(caseListItems.map { Syntax($0) })

        return SyntaxFactory.makeSwitchStmt(
            labelName: nil,
            labelColon: nil,
            switchKeyword: SyntaxFactory.makeSwitchKeyword().withLeadingTrivia(.newlines(1))
                .withTrailingTrivia(.spaces(1)),
            expression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                identifier: SyntaxFactory.makeIdentifier("testCase"),
                declNameArguments: nil
            )),
            leftBrace: SyntaxFactory.makeLeftBraceToken().withTrailingTrivia(.newlines(1)),
            cases: caseList,
            rightBrace: SyntaxFactory.makeRightBraceToken().withTrailingTrivia(.newlines(1))
        )
    }

    static func makeEnumCaseDeclaration(fromCase reusableTestCaseEnumCase: ASTTransform
        .ScopeLevelItemTransformationResult.ReusableTestCaseEnum.Case) -> EnumCaseDeclSyntax
    {
        return SyntaxFactory.makeEnumCaseDecl(
            attributes: nil,
            modifiers: nil,
            caseKeyword: SyntaxFactory.makeCaseKeyword().withTrailingTrivia(.spaces(1)),
            elements: SyntaxFactory.makeEnumCaseElementList([SyntaxFactory.makeEnumCaseElement(
                identifier: SyntaxFactory.makeIdentifier(reusableTestCaseEnumCase.name),
                associatedValue: nil,
                rawValue: nil,
                trailingComma: nil
            )])
        ).withTrailingTrivia(.newlines(1))
    }

    static func makeEnumDeclaration(fromEnum reusableTestCaseEnum: ASTTransform
        .ScopeLevelItemTransformationResult.ReusableTestCaseEnum) -> EnumDeclSyntax
    {
        let members = SyntaxFactory.makeMemberDeclList(reusableTestCaseEnum.cases.map { enumCase in
            let caseDecl = makeEnumCaseDeclaration(fromCase: enumCase)
            return SyntaxFactory.makeMemberDeclListItem(decl: DeclSyntax(caseDecl), semicolon: nil)
        })
        let memberDeclBlock = SyntaxFactory.makeMemberDeclBlock(
            leftBrace: SyntaxFactory.makeLeftBraceToken().withTrailingTrivia(.newlines(1)),
            members: members,
            rightBrace: SyntaxFactory.makeRightBraceToken().withTrailingTrivia(.newlines(1))
        )
        return SyntaxFactory.makeEnumDecl(
            attributes: nil,
            modifiers: nil,
            enumKeyword: SyntaxFactory.makeEnumKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(reusableTestCaseEnum.name),
            genericParameters: nil,
            inheritanceClause: nil,
            genericWhereClause: nil,
            members: memberDeclBlock
        ).withLeadingTrivia(.newlines(1))
    }
}
