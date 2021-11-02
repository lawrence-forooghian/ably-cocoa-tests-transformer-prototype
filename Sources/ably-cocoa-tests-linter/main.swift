import SwiftSyntax
import Foundation

// TODO what about stuff like comments – do they come through?

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
    
    override func visitAny(_ node: Syntax) -> Syntax? {
        // I'm trying to figure out how to return something other than a DeclSyntax from a visit(FunctionDeclSyntax), I think it's using this visitAny
        
        debugPrint(node.syntaxNodeType)
        
        guard let functionDecl = FunctionDeclSyntax(node) else {
            return nil // I think this means "do your usual thing, I won't give you anything different here"
        }

        print("it's a function decl")
        
        return visit(functionDecl)
    }
    
    private func visit(_ node: FunctionDeclSyntax) -> Syntax {
        print("HERE")
        switch (node.identifier.text) {
        case "setUp", "tearDown":
            print("TODO address setUp and tearDown")
            return Syntax(node)
        case "spec":
            
            // So, what we've learned here is that you can grab all of the variable declarations from inside the spec() function, and spit them out as the memebr variables of a new struct. But that for whatever reason it's not easy (/ possible?) So what we'll do is return a list of items and then add those into the member list of the class (and if that doesn't work then we'll just create a new class).
            let memberDeclListItems = getSpecMemberDeclListItems(node)
            
            
            let structKeyword = SyntaxFactory.makeStructKeyword(trailingTrivia: .spaces(1))

            let identifier = SyntaxFactory.makeIdentifier("Example", trailingTrivia: .spaces(1))

            let leftBrace = SyntaxFactory.makeLeftBraceToken()
            let rightBrace = SyntaxFactory.makeRightBraceToken(leadingTrivia: .newlines(1))
            let members = MemberDeclBlockSyntax { builder in
                builder.useLeftBrace(leftBrace)
                builder.useRightBrace(rightBrace)
                
                memberDeclListItems.forEach { item in
                    builder.addMember(item)
                }
            }

            let structureDeclaration = StructDeclSyntax { builder in
                builder.useStructKeyword(structKeyword)
                builder.useIdentifier(identifier)
                builder.useMembers(members)
            }
            
            
            
            return Syntax(structureDeclaration)

            // this does _not_ work – gives that same assertion failure in MemberDeclListItemSyntax _validateLayout() - so there's something funny going on with attempting to grab a MemberDeclList directly
//            return Syntax(structureDeclaration.members.members)

            
//            return Syntax(transformSpec(node))
        default:
            if (isAuditedForPassthrough(node)) {
                return Syntax(node)
            }
            fatalError("Don't know how to handle function \(node.identifier) in \(containingClassName)")
        }
    }
    
    // TODO what do we do about other declarations, like variables etc?
    
    private func getSpecMemberDeclListItems(_ node: FunctionDeclSyntax) -> [MemberDeclListItemSyntax] {
//    private func transformSpec(_ node: FunctionDeclSyntax) -> Syntax {
        print("TODO address spec")
        
        // I think we want to lift the whole body of spec() and pull it out to the class body, keeping the variable declarations etc (see e.g. RestClient's spec)
        
        // Then what do we do with the describe / it?
        
        // https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?
        // what should we do with the xit test cases?
        
        guard let body = node.body else {
            fatalError("Don’t know how to handle function declaration without a body")
        }
        
        // Our aim now is to return a MemberDeclListItemSyntax
        
        // OK, it doesn't like this
        // I think we need to return a MemberDeclListItemSyntax or something like that
        
        /*
         class-members → class-member class-members opt
         class-member → declaration | compiler-control-statement
        */
        // OK, we need to build a bunch of MemberDeclListItemSyntax (I think by grabbing the calls from the body and turning them into declarations) and then figure out how to turn them into a list

        
        body.statements.forEach { statement in
            print(statement.syntaxNodeType)
            print(statement)
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe

            // TODO what if there's stuff that clashes?

            if let variableDeclaration = VariableDeclSyntax(statement.item) {
                print("it's a variable declaration")
            }
            else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                print("it's a function call")
            }
            else if let structDeclaration = StructDeclSyntax(statement.item) {
                // there's just one (ExpectedTokenParams)
                print("it's a struct declaration")
            }
            else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                // TODO I think this is just a couple, search rsh3a2a()
                print("it's a function declaration")
            } else {
                assertionFailure("I don't know how to handle this thing")
            }
        }

        let variableDeclarations = body.statements.compactMap { statement in
            VariableDeclSyntax(statement.item)
        }
        let memberDeclListItems = variableDeclarations.map { variableDeclaration -> MemberDeclListItemSyntax in
            let decl = DeclSyntax(variableDeclaration)
            print("created decl \(decl)")
            let memberDeclListItem = MemberDeclListItemSyntax { builder in builder.useDecl(decl) }
            print("created memberDeclListItem \(memberDeclListItem)")
            memberDeclListItem._validateLayout()
            print("validated")
            // Hmm, those are validating, so what's failing???
            return memberDeclListItem
        }
        
        return memberDeclListItems
    
        // no idea if this is the right way to create one
        // now something is failing on // Check child #0 child is DeclSyntax
//        let memberDeclList = SyntaxFactory.makeMemberDeclList(memberDeclListItems)
//        print("created memberDeclList \(memberDeclList)")
//        memberDeclList._validateLayout()
//        print(memberDeclList.children.count)
//        print(memberDeclList.children)
//        print("validated")
        
        // What's failing? I would love a backtrace (OK, I'm now just using swift package edit to do print-debugging)
        
        // there seems to be some weird Franken-MemberDeclListItemSyntax that has loads of declarations; where's that coming from?
        
//        let memberDeclList = SyntaxFactory.makeBlankMemberDeclList().inserting(memberDeclListItems[0], at: 0)
//
//        print("created list")
//        memberDeclList._validateLayout()
//        print("validated list")
        
        // so even with one item, this is messed up; what's happening, why is making this decl list making some messed up decl?
        // and a blank list is no better…
    
//        // copied from https://nshipster.com/swiftsyntax/
//        let leftBrace = SyntaxFactory.makeLeftBraceToken()
//        let rightBrace = SyntaxFactory.makeRightBraceToken(leadingTrivia: .newlines(1))
//        let members = MemberDeclBlockSyntax { builder in
//            builder.useLeftBrace(leftBrace)
//            builder.useRightBrace(rightBrace)
//        }
//
//        print(members)
//        // their creation of a structure declaration does work if we just dump it into a file, there's something about the way I"m trying to use it that it doesn't like
//
//        return members.members // nope, this gives the same assertion
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
        // TODO I think we can use SyntaxFactory for this
//        let newToken = token.withKind(.identifier("XCTestCase")) // TODO how to create this? Seems weird that I need an existing token to create a new one – isn't there a "token builder"?
                
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
