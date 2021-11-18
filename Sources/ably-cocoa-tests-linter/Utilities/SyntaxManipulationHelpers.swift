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

    static func addingContextToReusableTestsFunctionDeclaration(_ decl: FunctionDeclSyntax)
        -> FunctionDeclSyntax
    {
        // We add a `context: (beforeEach: () -> (), afterEach: () -> ())` arg to all these functions

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
            trailingComma: hasTrailingClosure ? SyntaxFactory.makeCommaToken() : nil
        )
        parameterList = parameterList.inserting(
            parameter,
            at: hasTrailingClosure ? parameterList.count - 1 : parameterList.count
        )

        var newDecl = decl
        newDecl.signature.input.parameterList = parameterList

        return newDecl
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
}
