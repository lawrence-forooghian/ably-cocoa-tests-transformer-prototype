import SwiftSyntax

extension ASTTransform {
    struct ItemTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclarationItems: [AST.ClassDeclaration.Item]

        static let empty = Self(globalDeclarations: [], classDeclarationItems: [])

        func appending(_ other: ItemTransformationResult) -> ItemTransformationResult {
            var result = self
            result.globalDeclarations += other.globalDeclarations
            result.classDeclarationItems += other.classDeclarationItems
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
