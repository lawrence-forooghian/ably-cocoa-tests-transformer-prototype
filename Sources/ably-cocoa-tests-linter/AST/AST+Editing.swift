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
