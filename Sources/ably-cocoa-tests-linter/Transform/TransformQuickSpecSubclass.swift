import SwiftSyntax

class TransformQuickSpecSubclass {
    private let classDeclaration: ClassDeclSyntax
    private let options: TransformQuickSpec.Options

    init(classDeclaration: ClassDeclSyntax, options: TransformQuickSpec.Options) {
        self.classDeclaration = classDeclaration
        self.options = options
    }

    var containingClassName: String {
        classDeclaration.identifier.text
    }

    struct ClassTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDecl: ClassDeclSyntax
    }

    func transformed() -> ASTTransform.ClassTransformationResult {
        let classContents = AST.Parser.parseClassDeclaration(classDeclaration)
        return ASTTransform(options: options).transformClassDeclaration(classContents)
    }
}
