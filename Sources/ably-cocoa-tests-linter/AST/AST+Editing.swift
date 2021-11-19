import SwiftSyntax

extension AST.ClassDeclaration {
    func replacingItems(with newItems: [Item]) -> Self {
        var result = self

        result.items = newItems

        // Fix up the syntax
        let originalMembersBlock = syntax.members

        // This feels a bit clumsy, maybe doing something wrong
        // TODO: move this into some black box where we do all the SwiftSyntax-manipulation-faffing-around
        let newMembersBlock = MemberDeclBlockSyntax { builder in
            builder.useLeftBrace(originalMembersBlock.leftBrace)
            builder.useRightBrace(originalMembersBlock.rightBrace)

            newItems.forEach { item in
                builder.addMember(item.syntax)
            }
        }

        var newSyntax = syntax
        newSyntax.members = newMembersBlock

        result.syntax = newSyntax

        return result
    }
}

private func makeCodeBlockItemList(forContents contents: [ASTTransform
        .ScopeLevelItemTransformationResult.Item.ReplacementItem])
    -> CodeBlockItemListSyntax
{
    return SyntaxFactory.makeCodeBlockItemList(contents.map { item in
        SyntaxFactory.makeCodeBlockItem(item: item.item.syntax, semicolon: nil, errorTokens: nil)
    })
}

private func replaceContents(
    ofFunctionDecl functionDecl: FunctionDeclSyntax,
    with newContents: [ASTTransform.ScopeLevelItemTransformationResult.Item.ReplacementItem]
) -> FunctionDeclSyntax {
    // Fix up the syntax
    let newStatements = makeCodeBlockItemList(forContents: newContents)

    var newFunctionDeclaration = functionDecl
    // TODO: I've already unwrapped this once when parsing, would be nice to not again
    newFunctionDeclaration.body!.statements = newStatements

    return newFunctionDeclaration
}

extension AST.ScopeLevel.Spec {
    func replacingContents(with newContents: [ASTTransform.ScopeLevelItemTransformationResult.Item
            .ReplacementItem]) -> Self
    {
        var result = self

        result.contents = newContents.map(\.item)
        result.syntax
            .decl =
            DeclSyntax(replaceContents(ofFunctionDecl: functionDeclaration, with: newContents))

        return result
    }
}

extension AST.ScopeLevel.DescribeOrContext {
    func replacingContents(with newContents: [ASTTransform.ScopeLevelItemTransformationResult.Item
            .ReplacementItem]) -> Self
    {
        var result = self

        result.contents = newContents.map(\.item)

        // Fix up the syntax
        // TODO: I've already unwrapped this once when parsing, would be nice to not again
        result.syntax.trailingClosure!.statements = makeCodeBlockItemList(forContents: newContents)

        return result
    }
}

extension AST.ScopeLevel.ReusableTestsDeclaration {
    func replacingContents(with newContents: [ASTTransform.ScopeLevelItemTransformationResult.Item
            .ReplacementItem]) -> Self
    {
        var result = self

        result.contents = newContents.map(\.item)
        result.syntax = replaceContents(ofFunctionDecl: syntax, with: newContents)

        return result
    }
}
