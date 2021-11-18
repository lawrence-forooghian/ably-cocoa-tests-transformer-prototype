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

private func makeCodeBlockItemList(forContents contents: [AST.ScopeLevel.Item])
    -> CodeBlockItemListSyntax
{
    return SyntaxFactory.makeCodeBlockItemList(contents.map { item in
        SyntaxFactory.makeCodeBlockItem(item: item.syntax, semicolon: nil, errorTokens: nil)
    })
}

extension AST.ScopeLevel.Spec {
    func replacingContents(with newContents: [AST.ScopeLevel.Item]) -> Self {
        var result = self

        result.contents = newContents

        // Fix up the syntax
        let newStatements = makeCodeBlockItemList(forContents: newContents)

        var newFunctionDeclaration = functionDeclaration
        // TODO: I've already unwrapped this once, why again?
        newFunctionDeclaration.body!.statements = newStatements

        result.functionDeclaration = newFunctionDeclaration
        result.syntax.decl = DeclSyntax(newFunctionDeclaration)

        return result
    }
}

extension AST.ScopeLevel.DescribeOrContext {
    func replacingContents(with newContents: [AST.ScopeLevel.Item]) -> Self {
        var result = self

        result.contents = newContents

        // Fix up the syntax
        // TODO: I've already unwrapped this once, why again?
        result.syntax.trailingClosure!.statements = makeCodeBlockItemList(forContents: newContents)

        return result
    }
}
