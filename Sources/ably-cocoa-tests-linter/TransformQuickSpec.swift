import SwiftSyntax

// Transforms subclasses of QuickSpec to XCTestCase
class TransformQuickSpec: SyntaxRewriter {
    struct Options {
        var onlyLocalsToGlobals = false
    }

    let options: Options

    init(options: Options) {
        self.options = options
        super.init()
    }

    override func visitAny(_ node: Syntax) -> Syntax? {
        guard let classDecl = node.as(ClassDeclSyntax.self) else {
            return nil
        }

        guard let inheritanceClause = classDecl.inheritanceClause else {
            // If it doesn't inherit from anything, pass it through untouched.
            return nil
        }

        print("Processing class \(classDecl.identifier)")

        let inheritedTypeCollection = inheritanceClause.inheritedTypeCollection

        let quickSpecInheritedType = inheritedTypeCollection.first { inheritedType in
            let typeName = inheritedType.typeName
            let token = typeName.firstToken!

            // Not sure if this is the best way to check (can we keep going further syntactically or are we at a terminal?)
            guard case let .identifier(inheritedFromName) = token.tokenKind else {
                fatalError("I expected an identifier to be the token here")
            }

            return inheritedFromName == "QuickSpec"
        }

        guard let quickSpecInheritedType = quickSpecInheritedType else {
            // If it's not a subclass of QuickSpec, pass it through untouched.
            return nil
        }

        precondition(
            inheritedTypeCollection.count == 1,
            "I’m only equipped to handle things that inherit from one thing"
        )

        //        let typeName = quickSpecInheritedType.typeName
        //        let token = typeName.firstToken!

        let newToken = SyntaxFactory.makeToken(.identifier("XCTestCase"), presence: .present)
        //        let newToken = token.withKind(.identifier("XCTestCase")) // I'm keeping this around in case it helps with getting whitespace right

        let typeIdentifier = SimpleTypeIdentifierSyntax { builder in builder.useName(newToken) }

        let newTypeName = TypeSyntax(typeIdentifier) // No idea if this is right

        // The editing interface is a bit of a mystery to me – ah, these builders
        // I found out about "simple type identifier" by looking at the TypeSyntax initializer’s source code

        let newInheritedType = quickSpecInheritedType.withTypeName(newTypeName)
        let newInheritedTypes = inheritedTypeCollection.replacing(
            childAt: 0,
            with: newInheritedType
        )
        let newInheritanceClause = inheritanceClause.withInheritedTypeCollection(newInheritedTypes)
        let newNode = classDecl.withInheritanceClause(newInheritanceClause)

        let transformed = TransformQuickSpecSubclass(classDeclaration: newNode, options: options)
            .transformed()

        let codeBlockItemList = SyntaxFactory.makeCodeBlockItemList([
            transformed.globalDeclarations
                .map { decl in
                    SyntaxFactory
                        .makeCodeBlockItem(item: Syntax(decl), semicolon: nil, errorTokens: nil)
                },
            [SyntaxFactory.makeCodeBlockItem(
                item: Syntax(transformed.classDecl),
                semicolon: nil,
                errorTokens: nil
            )],
        ].flatMap { $0 })

        return Syntax(codeBlockItemList)
    }
}
