import SwiftSyntax

extension ASTTransform {
    struct ClassDeclarationItemTransformationResult {
        enum Item {
            case replacementItem(AST.ClassDeclaration.Item)
            case globalDeclaration(DeclSyntax)
        }

        var items: [Item]

        var replacementItems: [AST.ClassDeclaration.Item] {
            return items.compactMap { item in
                if case let .replacementItem(replacementItem) = item {
                    return replacementItem
                } else {
                    return nil
                }
            }
        }

        var globalDeclarations: [DeclSyntax] {
            return items.compactMap { item in
                if case let .globalDeclaration(decl) = item {
                    return decl
                } else {
                    return nil
                }
            }
        }
    }
}
