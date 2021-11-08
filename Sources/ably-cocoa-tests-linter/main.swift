import SwiftSyntax
import Foundation

// TODO what about stuff like comments – do they come through?
// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?

class TransformQuickSpecSubclass {
    enum ItAncestor: CustomStringConvertible {
        case spec
        case describeOrContext(description: String)
        
        var description: String {
            switch (self) {
            case .spec: return "spec()"
            case let .describeOrContext(description: description): return "describeOrContext(\(description))"
            }
        }
        
        var methodNameComponent: String {
            switch (self) {
            case .spec: return ""
            case let .describeOrContext(description: description): return description
            }
        }
    }
    typealias ItAncestry = [ItAncestor]
    
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
    
    private func transformClassMember(_ member: MemberDeclListItemSyntax) -> [MemberDeclListItemSyntax] {
        // I think the only class-level thing we want to manipulate is the `spec` function – everything else
        // we can pass through
        
        guard let specFunctionDecl = member.decl.as(FunctionDeclSyntax.self), specFunctionDecl.identifier.text == "spec" else {
            return [member]
        }
        
        return transformSpecFunctionDeclarationIntoClassLevelDeclarations(specFunctionDecl)
    }
    
    private func transformSpecFunctionDeclarationIntoClassLevelDeclarations(_ specFunctionDeclaration: FunctionDeclSyntax) -> [MemberDeclListItemSyntax] {
        // I think we want to lift the whole body of spec() and pull it out to the class body, keeping the variable declarations etc (see e.g. RestClient's spec)
        
        guard let specFunctionBody = specFunctionDeclaration.body else {
            fatalError("Don’t know how to handle function declaration without a body")
        }
        
        return transformItContainerBodyIntoClassLevelDeclarations(specFunctionBody.statements, ancestry: [.spec])
    }
    
    private func transformItContainerBodyIntoClassLevelDeclarations(_ statements: CodeBlockItemListSyntax, ancestry: ItAncestry) -> [MemberDeclListItemSyntax] {
        // TODO remove references to spec() here, and check they still apply
        
        let memberDeclListItems = statements.compactMap { statement -> [MemberDeclListItemSyntax]? in
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe
            
            // TODO what if there's stuff that clashes?
            
            if let variableDeclaration = VariableDeclSyntax(statement.item) {
                // Variable declarations just get hoisted outside of spec()
                // TODO revisit this now that it's not just spec — these need tidying up, probably grouping into some sort of object instead of just dumping everything at the top level
                
                let decl = DeclSyntax(variableDeclaration)
                return [MemberDeclListItemSyntax { builder in builder.useDecl(decl) }]
            }
            else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                return transformFunctionCallInsideItContainerIntoClassLevelDeclarations(functionCallExpr, ancestry: ancestry)
            }
            else if let structDeclaration = StructDeclSyntax(statement.item) {
                // Struct declarations just get hoisted outside of spec()
                // We only have one of these
                
                let decl = DeclSyntax(structDeclaration)
                return [MemberDeclListItemSyntax { builder in builder.useDecl(decl) }]
            }
            else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                // TODO The alternative here would probably be, instead of allow-listing everything,
                // to treat any function that contains calls to `context` etc as an instance of this case
                // TODO let's emit a warning when thsi returns no test cases? probably means we unrolled a loop incorrectly
                if (["rsh3a2a", "rsh3d2", "testFixture", "testAttribute", "testDirection", "testRequestType", "testStateWaitingForRegistrationSyncThrough", "testTokenRequestFromJson"].contains(functionDeclaration.identifier.text)) {
                    // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
                    // but we preserve the containing function and make it also invoke all of the test cases
                    
                    let declarations = transformSpecFunctionDeclarationIntoClassLevelDeclarations(functionDeclaration)
                    
                    // OK, we need to embed this inside a class
                    // wait, no, we'll keep it as a function call
                    
                    var newFunctionDeclaration = functionDeclaration
                    
                    // TODO this is also probably hacky - we're taking a bunch of class member items and turning them back into just declarations. Not necessary
                    let codeBlockItemsFromFunction = declarations.map { declaration in
                        SyntaxFactory.makeCodeBlockItem(item: Syntax(declaration.decl), semicolon: nil, errorTokens: nil)
                    }
                    
                    // TODO this is a bit hacky – we're essentially figuring out which
                    // test methods were created by transformSpecFunctionDeclarationIntoClassLevelDeclarations
                    // but we could probably just improve things to make it tell us that
                    let testFunctionDeclarations = declarations.compactMap { declaration -> FunctionDeclSyntax? in
                        guard let functionDeclaration = declaration.decl.as(FunctionDeclSyntax.self) else {
                            return nil
                        }
                        if (!functionDeclaration.identifier.text.starts(with: "test")) {
                            // TODO should we handle skipped functions in some nicer way?
                            return nil
                        }
                        return functionDeclaration
                    }
                    
                    // We now invoke all of these functions.
                    let testFunctionInvocationCodeBlockItems = testFunctionDeclarations.map { declaration -> CodeBlockItemSyntax in
                        let testFunctionInvocationExpression = SyntaxFactory.makeFunctionCallExpr(calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: declaration.identifier, declNameArguments: nil)), leftParen: SyntaxFactory.makeLeftParenToken(), argumentList: SyntaxFactory.makeBlankTupleExprElementList(), rightParen: SyntaxFactory.makeRightParenToken(), trailingClosure: nil, additionalTrailingClosures: nil).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                        
                        return SyntaxFactory.makeCodeBlockItem(item: Syntax(testFunctionInvocationExpression), semicolon: nil, errorTokens: nil)
                    }
                    
                    // TODO should we stick some logging into these test functions or something?
                    
                    newFunctionDeclaration.body!.statements = SyntaxFactory.makeCodeBlockItemList(codeBlockItemsFromFunction + testFunctionInvocationCodeBlockItems)
                    
                    return [MemberDeclListItemSyntax { builder in builder.useDecl(DeclSyntax(newFunctionDeclaration)) }]
                }
                
                if (["testWithUntilAttach", "testHandlesDecodingErrorInFixture", "testFakeNetworkResponse", "testSupportsAESEncryptionWithKeyLength", "testOptionsGiveDefaultAuthMethod", "testOptionsGiveBasicAuthFalse", "testRestoresDefaultPrimaryHostAfterTimeoutExpires", "testStoresSuccessfulFallbackHostAsDefaultHost", "testUsesAlternativeHost", "testUsesAnotherFallbackHost"].contains(functionDeclaration.identifier.text)) {
                    // This is a test function that directly contains assertions, we just pass it through
                    // TODO is there a neater way to do this? e.g. a special return type / method name
                    // TODO should we also namespace these, e.g. 'testHandlesDecodingErrorInFixture' in RealtimeClientChannel, which is inside a context?
                    // TODO does the XCTest framework know what to do with these?
                    return [ MemberDeclListItemSyntax { builder in builder.useDecl(DeclSyntax(functionDeclaration)) }]
                }
                
                print("TODO handle \(ancestry)-level declaration of function `\(functionDeclaration.identifier)`")
                return []
                
            }
            else {
                print("TODO handle \(ancestry)-level \(statement.item)")
                return nil
            }
        }.flatMap { $0 }
        
        return memberDeclListItems
    }
    
    private func transformFunctionCallInsideItContainerIntoClassLevelDeclarations(_ functionCallExpr: FunctionCallExprSyntax, ancestry: ItAncestry) -> [MemberDeclListItemSyntax] {
        guard let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression)) else {
            print("Expected an identifier, but got \(functionCallExpr)")
            return []
        }
        
        // Not exactly sure what .text is but it seems to not have whitespace / comments etc
        let calledFunctionName = identifierExpression.identifier.text
        
        switch (calledFunctionName) {
        case "it":
            return [transformItFunctionCallIntoClassLevelDeclaration(functionCallExpr, ancestry: ancestry, skipped: false)]
        case "xit":
            return [transformItFunctionCallIntoClassLevelDeclaration(functionCallExpr, ancestry: ancestry, skipped: true)]
        case "describe", "context":
            guard let trailingClosure = functionCallExpr.trailingClosure else {
                // TODO DRY up with `it`
                preconditionFailure("Expected a trailing closure")
            }
            
            let description = getFunctionArgument(functionCallExpr)
            
            return transformItContainerBodyIntoClassLevelDeclarations(trailingClosure.statements, ancestry: ancestry + [.describeOrContext(description: description)])
        default:
            print("TODO handle \(ancestry)-level `\(calledFunctionName)`")
            return []
        }
    }
    
    // gets the argument for `it` / `describe` / `context` etc
    private func getFunctionArgument(_ functionCallExpr: FunctionCallExprSyntax) -> String {
        // TODO update function name from `it` here
        
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
        
        return testDescription
    }

    private func transformItFunctionCallIntoClassLevelDeclaration(_ functionCallExpr: FunctionCallExprSyntax, ancestry: ItAncestry, skipped: Bool) -> MemberDeclListItemSyntax {
        // TODO do something with the ancestry
        
        // `it` gets turned into a method
        
        let testDescription = getFunctionArgument(functionCallExpr)
        
        let methodName = methodName(testDescription: testDescription, ancestry: ancestry, skipped: skipped)
        
        // Now we grab the trailing closure from the call to `it` and use that as the new test method's body
        
        guard let trailingClosure = functionCallExpr.trailingClosure else {
            preconditionFailure("I expect a call to `it` to have a trailing closure")
        }
        
        guard trailingClosure.signature == nil else {
            preconditionFailure("I don't expect the trailing closure to have any signature, but got \(trailingClosure)")
        }
        
        let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
            attributes: nil,
            modifiers: nil,
            funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(methodName),
            genericParameterClause: nil,
            signature: SyntaxFactory.makeFunctionSignature(input: SyntaxFactory.makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(), parameterList: SyntaxFactory.makeBlankFunctionParameterList(), rightParen: SyntaxFactory.makeRightParenToken()), asyncOrReasyncKeyword: nil, throwsOrRethrowsKeyword: nil, output: nil),
            genericWhereClause: nil,
            body: SyntaxFactory.makeCodeBlock(leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)), statements: trailingClosure.statements, rightBrace: trailingClosure.rightBrace
                                             )
        ).withLeadingTrivia(functionCallExpr.leadingTrivia!).withTrailingTrivia(functionCallExpr.trailingTrivia!)
        
        return SyntaxFactory.makeMemberDeclListItem(
            decl: DeclSyntax(testFunctionDeclaration),
            semicolon: nil
        )
    }
    
    private func methodName(testDescription: String, ancestry: ItAncestry, skipped: Bool) -> String {
        let unsanitisedComponents = ancestry.map { $0.methodNameComponent } + [testDescription]
        let unsantisedName = unsanitisedComponents[0].starts(with: "test") ? "" : "test" + unsanitisedComponents.joined(separator: "_")
        
        let withoutSymbols = unsantisedName.components(separatedBy: CharacterSet.symbols.union(CharacterSet.punctuationCharacters)).joined(separator: "")
        let withoutWhitespace = withoutSymbols.components(separatedBy: CharacterSet.whitespaces).joined(separator: "_")
        
        // TODO iterate on this, probably want some camelCase instead of underscores, and to be more clever when we have a `describe` that matches the test class name
        
        return (skipped ? "skipped_" : "") + withoutWhitespace
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
