import SwiftSyntax

extension ASTTransform {
    struct ItemTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclarationItems: [AST.ClassDeclaration.Item]
        // non-nil implies that the scope level containing the item that was transformed
        // should be preserved and the items here should be inserted in that item's
        // place
        var scopeContents: [AST.ScopeLevel.Item]?

        static let empty = Self(globalDeclarations: [], classDeclarationItems: [])

        func appending(_ other: ItemTransformationResult) -> ItemTransformationResult {
            var result = self
            result.globalDeclarations += other.globalDeclarations
            result.classDeclarationItems += other.classDeclarationItems

            var resultScopeContents: [AST.ScopeLevel.Item]?
            if let scopeContents = scopeContents {
                resultScopeContents = scopeContents
            }
            if let otherScopeContents = other.scopeContents {
                resultScopeContents = (resultScopeContents ?? []) + otherScopeContents
            }
            result.scopeContents = resultScopeContents

            return result
        }
    }
}

extension ASTTransform.ItemTransformationResult {
    init(_ classDeclarationItem: AST.ClassDeclaration.Item) {
        self.init(globalDeclarations: [], classDeclarationItems: [classDeclarationItem])
    }

    init<T: DeclSyntaxProtocol>(classLevelDeclaration: T) {
        let member = MemberDeclListItemSyntax { builder in
            let syntax = DeclSyntax(classLevelDeclaration)
            builder.useDecl(syntax)
        }
        self.init(.member(member))
    }

    init<T: DeclSyntaxProtocol>(globalDeclaration: T) {
        self.init(globalDeclarations: [DeclSyntax(globalDeclaration)], classDeclarationItems: [])
    }
}
