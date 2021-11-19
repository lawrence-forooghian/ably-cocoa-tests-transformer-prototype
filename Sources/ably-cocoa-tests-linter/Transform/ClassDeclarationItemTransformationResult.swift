import SwiftSyntax

extension ASTTransform {
    struct ClassDeclarationItemTransformationResult {
        var replacementItems: [AST.ClassDeclaration.Item]
        var globalDeclarations: [DeclSyntax]
    }
}
