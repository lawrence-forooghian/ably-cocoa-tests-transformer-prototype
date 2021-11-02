import SwiftSyntax
import Foundation

// TODO what about stuff like comments – do they come through?
// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?
// what should we do with the xit test cases?

// Transforms the `spec` method of a QuickSpec subclass.
class TransformQuickSpecMethods: SyntaxRewriter {
    private let containingClassName: String
    
    init(containingClassName: String) {
        self.containingClassName = containingClassName
    }
    
    private func isAuditedForPassthrough(_ node: FunctionDeclSyntax) -> Bool {
        let allowList = [["RealtimeClient", "checkError"]]
        
        return allowList.contains([containingClassName, node.identifier.text])
    }
    
    // TODO get rid of this and directly modify the class's members instead of spitting out a struct
    override func visitAny(_ node: Syntax) -> Syntax? {
        // I'm trying to figure out how to return something other than a DeclSyntax from a visit(FunctionDeclSyntax), I think it's using this visitAny
                
        guard let functionDecl = FunctionDeclSyntax(node) else {
            return nil // I think this means "do your usual thing, I won't give you anything different here"
        }
        
        return visit(functionDecl)
    }
    
    private func visit(_ node: FunctionDeclSyntax) -> Syntax {
        switch (node.identifier.text) {
        case "setUp", "tearDown":
            print("TODO handle class-level `setUp` and `tearDown`")
            return Syntax(node)
        case "spec":
            let classLevelDeclarations = transformSpecFunctionDeclarationIntoClassLevelDeclarations(node)
            return embedDeclarationsInStruct(classLevelDeclarations)
        default:
            if (isAuditedForPassthrough(node)) {
                return Syntax(node)
            }
            fatalError("Don't know how to handle class-level function \(node.identifier) in \(containingClassName)")
        }
    }
    
    private func embedDeclarationsInStruct(_ declarations: [MemberDeclListItemSyntax]) -> Syntax {
        // So, what we've learned here is that you can grab all of the variable declarations from inside the spec() function, and spit them out as the memebr variables of a new struct. But that for whatever reason it's not easy (/ possible?) So what we'll do is return a list of items and then add those into the member list of the class (and if that doesn't work then we'll just create a new class).
        
        let structKeyword = SyntaxFactory.makeStructKeyword(trailingTrivia: .spaces(1))

        let identifier = SyntaxFactory.makeIdentifier("PlaceholderTODORemoveAndPutInsideClass", trailingTrivia: .spaces(1))

        let leftBrace = SyntaxFactory.makeLeftBraceToken()
        let rightBrace = SyntaxFactory.makeRightBraceToken(leadingTrivia: .newlines(1))
        let members = MemberDeclBlockSyntax { builder in
            builder.useLeftBrace(leftBrace)
            builder.useRightBrace(rightBrace)
            
            declarations.forEach { item in
                builder.addMember(item)
            }
        }

        let structureDeclaration = StructDeclSyntax { builder in
            builder.useStructKeyword(structKeyword)
            builder.useIdentifier(identifier)
            builder.useMembers(members)
        }
        
        return Syntax(structureDeclaration)
    }
        
    private func transformSpecFunctionDeclarationIntoClassLevelDeclarations(_ specFunctionDeclaration: FunctionDeclSyntax) -> [MemberDeclListItemSyntax] {
        // I think we want to lift the whole body of spec() and pull it out to the class body, keeping the variable declarations etc (see e.g. RestClient's spec)
        
        // Then what do we do with the describe / it?
        
        guard let specFunctionBody = specFunctionDeclaration.body else {
            fatalError("Don’t know how to handle function declaration without a body")
        }

        let memberDeclListItems = specFunctionBody.statements.compactMap { statement -> MemberDeclListItemSyntax? in
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe

            // TODO what if there's stuff that clashes?

            if let variableDeclaration = VariableDeclSyntax(statement.item) {
                // Variable declarations just get hoisted outside of spec()
                
                let decl = DeclSyntax(variableDeclaration)
                return MemberDeclListItemSyntax { builder in builder.useDecl(decl) }
            }
            else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                guard let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression)) else {
                    preconditionFailure("Expected an identifier")
                }
                
                // Not exactly sure what .text is but it seems to not have whitespace / comments etc
                print("TODO handle spec()-level `\(identifierExpression.identifier.text)`")
                
                return nil
            }
            else if let structDeclaration = StructDeclSyntax(statement.item) {
                // Struct declarations just get hoisted outside of spec()
                // We only have one of these
                
                let decl = DeclSyntax(structDeclaration)
                return MemberDeclListItemSyntax { builder in builder.useDecl(decl) }
            }
            else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                // This is just a couple, search rsh3a2(), rsh3a2a(). Might
                // be simplest just to refactor this by hand first
                print("TODO handle spec()-level function declaration `\(functionDeclaration.identifier)`")
                return nil
            } else {
                preconditionFailure("I don't know how to handle this thing")
            }
        }

        return memberDeclListItems
    }
}

// Transforms subclasses of QuickSpec to XCTestCase
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
        
//        let typeName = quickSpecInheritedType.typeName
//        let token = typeName.firstToken!

        let newToken = SyntaxFactory.makeToken(.identifier("XCTestCase"), presence: .present)
//        let newToken = token.withKind(.identifier("XCTestCase")) // I'm keeping this around in case it helps with getting whitespace right
                
        let typeIdentifier = SimpleTypeIdentifierSyntax({ builder in builder.useName(newToken) })
        
        let newTypeName = TypeSyntax(typeIdentifier) // No idea if this is right
        
        // The editing interface is a bit of a mystery to me – ah, these builders
        // I found out about "simple type identifier" by looking at the TypeSyntax initializer’s source code
                
        let newInheritedType = quickSpecInheritedType.withTypeName(newTypeName)
        let newInheritedTypes = inheritedTypeCollection.replacing(childAt: 0, with: newInheritedType)
        let newInheritanceClause = inheritanceClause.withInheritedTypeCollection(newInheritedTypes)
        let newNode = node.withInheritanceClause(newInheritanceClause)
        
        return TransformQuickSpecMethods(containingClassName: node.identifier.text).visit(newNode)
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
    
//    try String(describing: transformed).write(to: url, atomically: true, encoding: .utf8)
}
