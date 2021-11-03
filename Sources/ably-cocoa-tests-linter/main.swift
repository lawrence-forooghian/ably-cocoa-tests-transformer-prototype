import SwiftSyntax
import Foundation

// TODO what about stuff like comments – do they come through?
// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?
// what should we do with the xit test cases?

class TransformQuickSpecSubclass {
    private let classDeclaration: ClassDeclSyntax
    
    init(classDeclaration: ClassDeclSyntax) {
        self.classDeclaration = classDeclaration
    }
    
    var containingClassName: String {
        get {
            classDeclaration.identifier.text
        }
    }
    
    func transformed() -> ClassDeclSyntax {
        var result = classDeclaration
        
        // This all feels a bit clumsy, probably doing something very wrong
        
        let originalMembersBlock = classDeclaration.members
        let newDeclListItems = originalMembersBlock.members.flatMap { member in
            return transformClassMember(member)
        }
        
        let newMembersBlock = MemberDeclBlockSyntax { builder in
            builder.useLeftBrace(originalMembersBlock.leftBrace)
            builder.useRightBrace(originalMembersBlock.rightBrace)
            
            newDeclListItems.forEach { item in
                builder.addMember(item)
            }
        }
        
        result.members = newMembersBlock
        
        return result
    }
    
    private func isAuditedForPassthrough(_ node: FunctionDeclSyntax) -> Bool {
        let allowList = [["RealtimeClient", "checkError"]]
        
        return allowList.contains([containingClassName, node.identifier.text])
    }
    
    private func transformClassMember(_ member: MemberDeclListItemSyntax) -> [MemberDeclListItemSyntax] {

        guard let functionDecl = member.decl.as(FunctionDeclSyntax.self) else {
            // TODO we need to figure out what else might be here
            return [member]
        }
        
        switch (functionDecl.identifier.text) {
        case "setUp", "tearDown":
            print("TODO handle class-level `setUp` and `tearDown`")
            return [member]
        case "spec":
            return transformSpecFunctionDeclarationIntoClassLevelDeclarations(functionDecl)
        default:
            if (isAuditedForPassthrough(functionDecl)) {
                return [member]
            }
            fatalError("Don't know how to handle class-level function \(functionDecl.identifier) in \(containingClassName)")
        }
    }
        
    private func transformSpecFunctionDeclarationIntoClassLevelDeclarations(_ specFunctionDeclaration: FunctionDeclSyntax) -> [MemberDeclListItemSyntax] {
        // I think we want to lift the whole body of spec() and pull it out to the class body, keeping the variable declarations etc (see e.g. RestClient's spec)
        
        // Then what do we do with the describe / it?
        
        // There's a bunch of other stuff we'll have to handle: contexts defined in a loop inside another context, variables inside contexts.
        
        guard let specFunctionBody = specFunctionDeclaration.body else {
            fatalError("Don’t know how to handle function declaration without a body")
        }

        let memberDeclListItems = specFunctionBody.statements.compactMap { statement -> [MemberDeclListItemSyntax]? in
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe

            // TODO what if there's stuff that clashes?

            if let variableDeclaration = VariableDeclSyntax(statement.item) {
                // Variable declarations just get hoisted outside of spec()
                
                let decl = DeclSyntax(variableDeclaration)
                return [MemberDeclListItemSyntax { builder in builder.useDecl(decl) }]
            }
            else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                return transformFunctionCallInsideSpecIntoClassLevelDeclarations(functionCallExpr)
            }
            else if let structDeclaration = StructDeclSyntax(statement.item) {
                // Struct declarations just get hoisted outside of spec()
                // We only have one of these
                
                let decl = DeclSyntax(structDeclaration)
                return [MemberDeclListItemSyntax { builder in builder.useDecl(decl) }]
            }
            else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                // This is just a couple, search rsh3a2(), rsh3a2a(). Might
                // be simplest just to refactor this by hand first
                print("TODO handle spec()-level function declaration `\(functionDeclaration.identifier)`")
                return nil
            } else {
                preconditionFailure("I don't know how to handle this thing")
            }
        }.flatMap { $0 }

        return memberDeclListItems
    }
    
    private func transformFunctionCallInsideSpecIntoClassLevelDeclarations(_ functionCallExpr: FunctionCallExprSyntax) -> [MemberDeclListItemSyntax] {
        guard let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression)) else {
            preconditionFailure("Expected an identifier")
        }
        
        // Not exactly sure what .text is but it seems to not have whitespace / comments etc
        let calledFunctionName = identifierExpression.identifier.text
        
        switch (calledFunctionName) {
        case "it":
            // `it` gets turned into a method
            
            precondition(functionCallExpr.argumentList.count == 1, "`it` should only take one argument")
            
            // OK, this is a ExprSyntax, how do I find out whether it's a string literal?
            let argumentExpression = functionCallExpr.argumentList.first!.expression

            guard let stringLiteralExpression = argumentExpression.as(StringLiteralExprSyntax.self) else {
                preconditionFailure("Expected the one argument to `it` to be a string literal describing the test")
            }
            
            precondition(stringLiteralExpression.segments.count == 1, "the argument to `it` I'm expecting to only have one segment")
            
            let firstSegment = stringLiteralExpression.segments.first!
            // TODO is this okay? Wasn't sure how to keep drilling
            let testDescription = firstSegment.firstToken!.text
            
            // Now we grab the trailing closure from the call to `it` and use that as the new test method's body
            
            guard let trailingClosure = functionCallExpr.trailingClosure else {
                preconditionFailure("I expect a call to `it` to have a trailing closure")
            }
            
            guard trailingClosure.signature == nil else {
                preconditionFailure("I don't expect the trailing closure to have any signature")
            }
                        
            let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
                attributes: nil,
                modifiers: nil,
                funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
                identifier: SyntaxFactory.makeIdentifier(testDescription) /* TODO this needs sanitizing */,
                genericParameterClause: nil,
                signature: SyntaxFactory.makeFunctionSignature(input: SyntaxFactory.makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(), parameterList: SyntaxFactory.makeBlankFunctionParameterList(), rightParen: SyntaxFactory.makeRightParenToken()), asyncOrReasyncKeyword: nil, throwsOrRethrowsKeyword: nil, output: nil),
                genericWhereClause: nil,
                body: SyntaxFactory.makeCodeBlock(leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)), statements: trailingClosure.statements, rightBrace: trailingClosure.rightBrace
                                                 )
            ).withLeadingTrivia(functionCallExpr.leadingTrivia!).withTrailingTrivia(functionCallExpr.trailingTrivia!)
            
            return [
                SyntaxFactory.makeMemberDeclListItem(
                    decl: DeclSyntax(testFunctionDeclaration),
                    semicolon: nil
                )
            ]
        default:
            print("TODO handle spec()-level `\(calledFunctionName)`")
            return []
        }
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
        
        let newClassDeclaration = TransformQuickSpecSubclass(classDeclaration: newNode).transformed()
        
        return DeclSyntax(newClassDeclaration)
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
}
