import SwiftSyntax

class ASTSerializer {
    static func serializeClassContents(
        _ classContents: ClassContents,
        originalClassDeclaration: ClassDeclSyntax
    ) -> ClassDeclSyntax {
        let originalMembersBlock = originalClassDeclaration.members

        // This feels a bit clumsy, maybe doing something wrong
        let newMembersBlock = MemberDeclBlockSyntax { builder in
            builder.useLeftBrace(originalMembersBlock.leftBrace)
            builder.useRightBrace(originalMembersBlock.rightBrace)

            classContents.contents.forEach { item in
                builder.addMember(serializeClassContentsItem(item))
            }
        }

        var result = originalClassDeclaration
        result.members = newMembersBlock

        return result
    }

    private static func serializeClassContentsItem(_ item: ClassContents
        .Item) -> MemberDeclListItemSyntax
    {
        switch item {
        case let .member(member):
            return member
        case let .scope(scope):
            return serializeScope(scope)
        }
    }

    private static func serializeScope(_ scope: ASTScope) -> MemberDeclListItemSyntax {
        switch scope.type {
        case let .spec(functionDecl), let .reusableTests(functionDecl, functionName: _):
            return MemberDeclListItemSyntax { builder in
                builder.useDecl(DeclSyntax(functionDecl))
            }
        case .describeOrContext:
            fatalError("Can’t serialize describeOrContext into MemberDeclListItemSyntax")
        }
    }
}
