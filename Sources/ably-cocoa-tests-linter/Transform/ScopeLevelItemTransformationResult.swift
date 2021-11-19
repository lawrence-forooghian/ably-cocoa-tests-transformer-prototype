import SwiftSyntax

extension ASTTransform {
    struct ScopeLevelItemTransformationResult {
        var replacementContents: [AST.ScopeLevel.Item]
        var globalDeclarations: [DeclSyntax]
        var classDeclarationItems: [AST.ClassDeclaration.Item]

        static let empty = Self(
            replacementContents: [],
            globalDeclarations: [],
            classDeclarationItems: []
        )

        func appending(_ other: ScopeLevelItemTransformationResult)
            -> ScopeLevelItemTransformationResult
        {
            var result = self
            result.replacementContents += other.replacementContents
            result.globalDeclarations += other.globalDeclarations
            result.classDeclarationItems += other.classDeclarationItems

            return result
        }
    }
}

extension ASTTransform.ScopeLevelItemTransformationResult {
    init(classDeclarationItem: AST.ClassDeclaration.Item) {
        self.init(
            replacementContents: [],
            globalDeclarations: [],
            classDeclarationItems: [classDeclarationItem]
        )
    }

    init(replacementItem: AST.ScopeLevel.Item) {
        self.init(
            replacementContents: [replacementItem],
            globalDeclarations: [],
            classDeclarationItems: []
        )
    }

    init<T: DeclSyntaxProtocol>(classLevelDeclaration: T) {
        let member = MemberDeclListItemSyntax { builder in
            let syntax = DeclSyntax(classLevelDeclaration)
            builder.useDecl(syntax)
        }
        self.init(classDeclarationItem: .member(member))
    }

    init<T: DeclSyntaxProtocol>(globalDeclaration: T) {
        self.init(
            replacementContents: [],
            globalDeclarations: [DeclSyntax(globalDeclaration)],
            classDeclarationItems: []
        )
    }
}
