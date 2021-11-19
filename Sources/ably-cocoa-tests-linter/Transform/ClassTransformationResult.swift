import SwiftSyntax

extension ASTTransform {
    struct ClassTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclaration: AST.ClassDeclaration
    }
}
