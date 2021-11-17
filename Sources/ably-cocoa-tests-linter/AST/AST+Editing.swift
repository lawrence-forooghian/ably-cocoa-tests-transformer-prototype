import SwiftSyntax

extension AST.ClassDeclaration {
    func replacingItems(_ newItems: [Item]) -> Self {
        var result = self

        result.items = newItems

        // Fix up the syntax
        let originalMembersBlock = syntax.members

        // This feels a bit clumsy, maybe doing something wrong
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
