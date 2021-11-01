import SwiftSyntax
import Foundation 

// rewrites subclasses of QuickSpec to XCTestCase
class TransformQuickSpec: SyntaxRewriter {
    override func visit(_ node: ClassDeclSyntax) -> DeclSyntax {
        guard let inheritanceClause = node.inheritanceClause else {
            // If it doesn't inherit from anything, pass it through untouched.
            return DeclSyntax(node)
        }
        
        print("Processing class \(node.identifier)")
        
        let inheritedTypeCollection = inheritanceClause.inheritedTypeCollection
        
        let quickSpecInheritedType = inheritedTypeCollection.first { inheritedType in
            let typeName = inheritedType.typeName
            let token = typeName.firstToken!
                                    
            // Not sure if this is the best way to check (can we keep going further syntactically or are we at a terminal?)
            guard case .identifier(let inheritedFromName) = token.tokenKind else {
                fatalError("I expected an identifier to be the token here")
            }
            
            return inheritedFromName == "QuickSpec"
        }
        
        guard let quickSpecInheritedType = quickSpecInheritedType else {
            // If it's not a subclass of QuickSpec, pass it through untouched.
            return DeclSyntax(node)
        }
        
        precondition(inheritedTypeCollection.count == 1, "I’m only equipped to handle things that inherit from one thing")
        
        let typeName = quickSpecInheritedType.typeName
        let token = typeName.firstToken!

        let newToken = token.withKind(.identifier("XCTestCase")) // TODO how to create this? Seems weird that I need an existing token to create a new one – isn't there a "token builder"?
                
        let typeIdentifier = SimpleTypeIdentifierSyntax({ builder in builder.useName(newToken) })
        
        let newTypeName = TypeSyntax(typeIdentifier) // No idea if this is right
        
        // The editing interface is a bit of a mystery to me – ah, these builders
        // I found out about "simple type identifier" by looking at the TypeSyntax initializer’s source code
                
        let newInheritedType = quickSpecInheritedType.withTypeName(newTypeName)
        let newInheritedTypes = inheritedTypeCollection.replacing(childAt: 0, with: newInheritedType)
        let newInheritanceClause = inheritanceClause.withInheritedTypeCollection(newInheritedTypes)
        let newNode = node.withInheritanceClause(newInheritanceClause)
        
        return DeclSyntax(newNode)
    }
}

let directory = CommandLine.arguments[1]
let directoryContents = try FileManager.default.contentsOfDirectory(atPath: directory)
let swiftFiles = directoryContents.filter { $0.hasSuffix(".swift") }

try swiftFiles.forEach { fileName in
    print("Processing file \(fileName)")
    let url = URL(fileURLWithPath: directory).appendingPathComponent(fileName)
    
    let sourceFile = try SyntaxParser.parse(url)
    let transformed = TransformQuickSpec().visit(sourceFile)
    
    try String(describing: transformed).write(to: url, atomically: true, encoding: .utf8)
    
    print(transformed)
}

// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?
// what should we do with the xit test cases?
