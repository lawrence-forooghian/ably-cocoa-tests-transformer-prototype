import SwiftSyntax
import Foundation

// TODO what about stuff like comments – do they come through?
// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?

// Guiding principles:
// - I don't understand the test code well, and I want people to just need to review the structural changes without thinking about any behavioural ones. there seems to be a lot of state potentially shared between test cases (probably not fully intentionally, and probably not desirably) and I don't want to change that behaviour. So must behave exactly as before

// TODO tidy up this code and make it bloggable / talkable / open sourceable if we so desire

// TODO I think we can make a Scope type so that we don't have to worry about things like array length (i.e. this invariant that scope is always non-empty)
extension Array where Element == TransformQuickSpecSubclass.ScopeMember /* i.e. Scope */ {
    private var parent: Self? {
        guard self.count >= 2 else { return nil }
        var new = self
        new.removeLast()
        return new
    }
    
    func nearestAncestorHavingOwnBeforeEach(includeSelf: Bool) -> Self? {
        if (includeSelf && last!.contentsInfo.hasOwnBeforeEach) {
            return self
        }
        return parent?.nearestAncestorHavingOwnBeforeEach(includeSelf: true)
    }
    
    func nearestAncestorHavingOwnAfterEach(includeSelf: Bool) -> Self? {
        if (includeSelf && last!.contentsInfo.hasOwnAfterEach) {
            return self
        }
        return parent?.nearestAncestorHavingOwnAfterEach(includeSelf: true)
    }
    
    var isSkipped: Bool {
        if case .describeOrContext(description: _, skipped: true) = last!.type {
            return true
        }
        return parent?.isSkipped ?? false
    }
}

class TransformQuickSpecSubclass {
    struct ScopeMember: CustomStringConvertible {
        enum ScopeMemberType: CustomStringConvertible {
            case spec
            case describeOrContext(description: String, skipped: Bool)
            
            var description: String {
                switch (self) {
                case .spec: return "spec()"
                case let .describeOrContext(description: description): return "describeOrContext(\(description))"
                }
            }

        }
        
        struct ContentsInfo {
            var hasOwnBeforeEach: Bool
            var hasOwnAfterEach: Bool
            var variableDefinitions: [VariableDefinition]
            
            struct VariableDefinition {
                var originalName: String
                var isMutable: Bool
                
                func mangledNameForScope(_ scope: Scope) -> String {
                    print("TODO mangle variable name correctly")
                    return "\(originalName)_TODOmangledName"
                }
            }
        }
        
        var type: ScopeMemberType
        var contentsInfo: ContentsInfo
        
        var methodNameComponent: String {
            switch (type) {
            case .spec: return ""
            case let .describeOrContext(description: description, skipped: _): return description
            }
        }
        
        var description: String {
            return "<\(String(describing: type))" + (contentsInfo.hasOwnBeforeEach ? ", hasOwnBeforeEach" : "") + (contentsInfo.hasOwnAfterEach ? ", hasOwnAfterEach" : "") + (contentsInfo.variableDefinitions.isEmpty ? "" : " variableDefinitions: \(contentsInfo.variableDefinitions)") + ">"
        }
    }
    typealias Scope = [ScopeMember]
    
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
        
        let contentsInfo = createScopeMemberContentsInfo(specFunctionBody.statements)
        
        return transformScopeMemberBodyIntoClassLevelDeclarations(specFunctionBody.statements, scope: [ScopeMember(type: .spec, contentsInfo: contentsInfo)])
    }
    
    private func createScopeMemberContentsInfo(_ statements: CodeBlockItemListSyntax) -> ScopeMember.ContentsInfo {
        return statements.reduce(ScopeMember.ContentsInfo(hasOwnBeforeEach: false, hasOwnAfterEach: false, variableDefinitions: [])) { result, statement in
            var newResult = result
            
            // beforeEach / afterEach
            if let functionCallExpr = FunctionCallExprSyntax(statement.item), let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression)) {
                // (copied comment from elsewhere) Not exactly sure what .text is but it seems to not have whitespace / comments etc
                let calledFunctionName = identifierExpression.identifier.text
                
                newResult.hasOwnBeforeEach = newResult.hasOwnBeforeEach || (calledFunctionName == "beforeEach")
                newResult.hasOwnAfterEach = newResult.hasOwnAfterEach || (calledFunctionName == "afterEach")
            }
            
            // variable declarations
            if let variableDeclarationSyntax = VariableDeclSyntax(statement.item) {
                guard variableDeclarationSyntax.bindings.count == 1 else {
                    preconditionFailure("I don’t know how to handle variable declarations with multiple bindings")
                }
                let binding = variableDeclarationSyntax.bindings.first!
                
                let variableDeclarations: [ScopeMember.ContentsInfo.VariableDefinition]
                
                if let identifierPattern = binding.pattern.as(IdentifierPatternSyntax.self) {
                    // TODO this must be wrong!
                    let isMutable = (variableDeclarationSyntax.letOrVarKeyword.text == "var")
                    variableDeclarations = [ScopeMember.ContentsInfo.VariableDefinition(originalName: identifierPattern.identifier.text /* is this correct? */, isMutable: isMutable)]
                }
                else if let tuplePattern = binding.pattern.as(TuplePatternSyntax.self) {
                    variableDeclarations = tuplePattern.elements.map { element in
                        guard let identifierPattern = element.pattern.as(IdentifierPatternSyntax.self) else {
                            preconditionFailure("I don't know how to handle a binding in a tuple that's not an identifier: \(binding.pattern)")
                        }
                        let isMutable = (variableDeclarationSyntax.letOrVarKeyword.text == "let")

                        // TODO DRY up with the above
                        return ScopeMember.ContentsInfo.VariableDefinition(originalName: identifierPattern.identifier.text /* is this correct? */, isMutable: isMutable)
                    }
                    
                } else {
                    preconditionFailure("I don’t know how to handle bindings with patterns other than an identifier or a tuple: \(binding.pattern)")
                }
                
                newResult.variableDefinitions += variableDeclarations
            }

            return newResult
        }
    }
    
    private func transformScopeMemberBodyIntoClassLevelDeclarations(_ statements: CodeBlockItemListSyntax, scope: Scope) -> [MemberDeclListItemSyntax] {
        // TODO remove references to spec() here, and check they still apply
        
        let memberDeclListItems = statements.compactMap { statement -> [MemberDeclListItemSyntax]? in
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe
            
            // TODO what if there's stuff that clashes?
            
            /*if let variableDeclaration = VariableDeclSyntax(statement.item) {
                // Variable declarations just get hoisted outside of spec()
                // TODO revisit this now that it's not just spec — these need tidying up, probably grouping into some sort of object instead of just dumping everything at the top level
                
                let decl = DeclSyntax(variableDeclaration)
                return [MemberDeclListItemSyntax { builder in builder.useDecl(decl) }]
            }
            else */if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                return transformFunctionCallInsideScopeIntoClassLevelDeclarations(functionCallExpr, scope: scope)
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
                if (["rsh3a2a", "rsh3d2", "testFixture", "testAttribute", "testDirection", "testRequestType", "testStateWaitingForRegistrationSyncThrough", "testTokenRequestFromJson", "testPresencePerformMethod"].contains(functionDeclaration.identifier.text)) {
                    // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
                    // but we preserve the containing function and make it also invoke all of the test cases
                    
                    print("\tTODO handle \(functionDeclaration.identifier.text) distinctly from `spec` – we need a scope, and we need to handle before/afterEach")
                    
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
                
                if (["testWithUntilAttach", "testHandlesDecodingErrorInFixture", "testFakeNetworkResponse", "testSupportsAESEncryptionWithKeyLength", "testOptionsGiveDefaultAuthMethod", "testOptionsGiveBasicAuthFalse", "testRestoresDefaultPrimaryHostAfterTimeoutExpires", "testStoresSuccessfulFallbackHostAsDefaultHost", "testUsesAlternativeHost", "testUsesAnotherFallbackHost", "testMovesToDisconnectedWithNetworkingError", "testStopsClientWithOptions", "testSuspendedStateResultsInError", "testResultsInErrorWithConnectionState", "testUsesAlternativeHostOnResponse"].contains(functionDeclaration.identifier.text)) {
                    // This is a test function that directly contains assertions, we just pass it through
                    // TODO is there a neater way to do this? e.g. a special return type / method name
                    // TODO should we also namespace these, e.g. 'testHandlesDecodingErrorInFixture' in RealtimeClientChannel, which is inside a context?
                    // TODO does the XCTest framework know what to do with these?
                    // TODO what if any of these use some context-local variables and we just shove them at the top level; is that maybe a problem? Maybe we need an object to contain everything, might be easiest
                    // TODO we can make this simpler once we start introducing scope objects – then these become methods on the scope or something and we pass em through. Except for anything containing it / context / describe — those get treated like a `spec`.
                    return [ MemberDeclListItemSyntax { builder in builder.useDecl(DeclSyntax(functionDeclaration)) }]
                }
                
                print("\tTODO handle \(scope)-level declaration of function `\(functionDeclaration.identifier)`")
                return []
                
            }
            else {
                print("\tTODO handle \(scope)-level \(statement.item)")
                return nil
            }
        }.flatMap { $0 }
        
        return memberDeclListItems
    }
    
    private func transformFunctionCallInsideScopeIntoClassLevelDeclarations(_ functionCallExpr: FunctionCallExprSyntax, scope: Scope) -> [MemberDeclListItemSyntax] {
        guard let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression)) else {
            print("Expected an identifier, but got \(functionCallExpr)")
            return []
        }
        
        // Not exactly sure what .text is but it seems to not have whitespace / comments etc
        let calledFunctionName = identifierExpression.identifier.text
        
        switch (calledFunctionName) {
        case "it", "xit":
            return [transformItFunctionCallIntoClassLevelDeclaration(functionCallExpr, scope: scope, skipped: calledFunctionName == "xit")]
        case "describe", "xdescribe", "context", "xcontext":
            guard let trailingClosure = functionCallExpr.trailingClosure else {
                // TODO DRY up with `it`
                preconditionFailure("Expected a trailing closure")
            }
            
            let description = getFunctionArgument(functionCallExpr)
            
            // do a preflight to fetch some info about the scope's contents - hasOwnBeforeEach, hasOwnAfterEach
            // TODO why does this give fewer hasOwnBeforeEach than we have in the codebase? I'm sure we'll find out in time
            let contentsInfo = createScopeMemberContentsInfo(trailingClosure.statements)
            
            let scopeMember = ScopeMember(type: .describeOrContext(description: description, skipped: calledFunctionName.starts(with: "x")), contentsInfo: contentsInfo)
            
            return transformScopeMemberBodyIntoClassLevelDeclarations(trailingClosure.statements, scope: scope + [scopeMember])
        case "beforeEach", "afterEach":
            return [transformBeforeOrAfterEachFunctionCallIntoClassLevelDeclaration(functionCallExpr, scope: scope)]
        default:
            print("\tTODO handle \(scope)-level `\(calledFunctionName)`")
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

    private func transformItFunctionCallIntoClassLevelDeclaration(_ functionCallExpr: FunctionCallExprSyntax, scope: Scope, skipped: Bool) -> MemberDeclListItemSyntax {
        // `it` gets turned into a method
        
        let testDescription = getFunctionArgument(functionCallExpr)
        
        let methodName = QuickSpecMethodCall.it(testDescription: testDescription, skipped: skipped).outputFunctionName(inScope: scope)
        
        // Now we grab the trailing closure from the call to `it` and use that as the new test method's body
        
        guard let trailingClosure = functionCallExpr.trailingClosure else {
            preconditionFailure("I expect a call to `it` to have a trailing closure")
        }
        
        guard trailingClosure.signature == nil else {
            preconditionFailure("I don't expect the trailing closure to have any signature, but got \(trailingClosure)")
        }
        
        // Insert a call to the before/afterEach of the scope this `it` is contained within.
        let newStatements: CodeBlockItemListSyntax = {
            var newStatements = trailingClosure.statements

            // TODO DRY these up with the beforeEach / afterEach ancestor-calling code
            
            // beforeEach
            if let nearestScopeHavingOwnBeforeEach = scope.nearestAncestorHavingOwnBeforeEach(includeSelf: true) {
                let functionName = QuickSpecMethodCall.beforeEach.outputFunctionName(inScope: nearestScopeHavingOwnBeforeEach)
                let functionCall = SyntaxFactory.makeFunctionCallExpr(calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: SyntaxFactory.makeToken(.identifier(functionName), presence: .present), declNameArguments: nil)), leftParen: SyntaxFactory.makeLeftParenToken(), argumentList: SyntaxFactory.makeBlankTupleExprElementList(), rightParen: SyntaxFactory.makeRightParenToken(), trailingClosure: nil, additionalTrailingClosures: nil).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil, errorTokens: nil))
            }
            
            // Set up the local variables that we have access to in this scope, for use
            // in the test case.
            scope.forEach { member in
                // emit e.g. var foo = mangledFoo
                member.contentsInfo.variableDefinitions.forEach { definition in
                    print("TODO Use correct scope when printing out mangled name in `it`-derived function")
                    let mangledName = definition.mangledNameForScope([member])
                    let letOrVarKeyword = definition.isMutable ? SyntaxFactory.makeVarKeyword() : SyntaxFactory.makeLetKeyword()
                    
                    let decl = SyntaxFactory.makeVariableDecl(attributes: nil, modifiers: nil, letOrVarKeyword: letOrVarKeyword.withTrailingTrivia(.spaces(1)), bindings: SyntaxFactory.makePatternBindingList([SyntaxFactory.makePatternBinding(pattern: PatternSyntax(SyntaxFactory.makeIdentifierPattern(identifier: SyntaxFactory.makeToken(.identifier(definition.originalName), presence: .present))), typeAnnotation: nil, initializer: SyntaxFactory.makeInitializerClause(equal: SyntaxFactory.makeEqualToken(leadingTrivia: .spaces(1), trailingTrivia: .spaces(1)), value: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: SyntaxFactory.makeIdentifier(mangledName), declNameArguments: nil))), accessor: nil, trailingComma: nil)])).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                    // TODO this prepending is kinda backwards - reverses the original order
                    newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(item: Syntax(decl), semicolon: nil, errorTokens: nil))
                }
            }
            
            
            // afterEach
            if let nearestScopeHavingOwnAfterEach = scope.nearestAncestorHavingOwnAfterEach(includeSelf: true) {
                let functionName = QuickSpecMethodCall.afterEach.outputFunctionName(inScope: nearestScopeHavingOwnAfterEach)
                let functionCall = SyntaxFactory.makeFunctionCallExpr(calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: SyntaxFactory.makeToken(.identifier(functionName), presence: .present), declNameArguments: nil)), leftParen: SyntaxFactory.makeLeftParenToken(), argumentList: SyntaxFactory.makeBlankTupleExprElementList(), rightParen: SyntaxFactory.makeRightParenToken(), trailingClosure: nil, additionalTrailingClosures: nil).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                newStatements = newStatements.appending(SyntaxFactory.makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil, errorTokens: nil))
            }
            
            return newStatements
        }()
        
        let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
            attributes: nil,
            modifiers: nil,
            funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(methodName),
            genericParameterClause: nil,
            signature: SyntaxFactory.makeFunctionSignature(input: SyntaxFactory.makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(), parameterList: SyntaxFactory.makeBlankFunctionParameterList(), rightParen: SyntaxFactory.makeRightParenToken()), asyncOrReasyncKeyword: nil, throwsOrRethrowsKeyword: nil, output: nil),
            genericWhereClause: nil,
            body: SyntaxFactory.makeCodeBlock(leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)), statements: newStatements, rightBrace: trailingClosure.rightBrace
                                             )
        ).withLeadingTrivia(functionCallExpr.leadingTrivia!).withTrailingTrivia(functionCallExpr.trailingTrivia!)
        
        return SyntaxFactory.makeMemberDeclListItem(
            decl: DeclSyntax(testFunctionDeclaration),
            semicolon: nil
        )
    }
    
    private func transformBeforeOrAfterEachFunctionCallIntoClassLevelDeclaration(_ functionCallExpr: FunctionCallExprSyntax, scope: Scope) -> MemberDeclListItemSyntax {
        // `beforeEach` or `afterEach` gets turned into a method
                
        let methodCall = QuickSpecMethodCall(functionCallExpr: functionCallExpr)
        let methodName = methodCall.outputFunctionName(inScope: scope)
        
        // Now we grab the trailing closure from the call to `before/afterEach` and use that as the new test method's body
        // TODO we can probably DRY this up with the `it` equivalent
        
        guard let trailingClosure = functionCallExpr.trailingClosure else {
            preconditionFailure("I expect a call to `before/afterEach` to have a trailing closure")
        }
        
        guard trailingClosure.signature == nil else {
            preconditionFailure("I don't expect the trailing closure to have any signature, but got \(trailingClosure)")
        }
                
        // Insert a call to the before/afterEach of the scope this one is nested within.
        let newStatements: CodeBlockItemListSyntax = {
            switch methodCall {
                // TODO double-check the ordering of the before / after in relation to parents
            case .beforeEach:
                guard let nearest = scope.nearestAncestorHavingOwnBeforeEach(includeSelf: false) else {
                    return trailingClosure.statements
                }
                let ancestorFunctionName = QuickSpecMethodCall.beforeEach.outputFunctionName(inScope: nearest)
                let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: SyntaxFactory.makeToken(.identifier(ancestorFunctionName), presence: .present), declNameArguments: nil)), leftParen: SyntaxFactory.makeLeftParenToken(), argumentList: SyntaxFactory.makeBlankTupleExprElementList(), rightParen: SyntaxFactory.makeRightParenToken(), trailingClosure: nil, additionalTrailingClosures: nil).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                return trailingClosure.statements.prepending(SyntaxFactory.makeCodeBlockItem(item: Syntax(ancestorFunctionCall), semicolon: nil, errorTokens: nil))
            case .afterEach:
                guard let nearest = scope.nearestAncestorHavingOwnAfterEach(includeSelf: false) else {
                    return trailingClosure.statements
                }
                let ancestorFunctionName = QuickSpecMethodCall.afterEach.outputFunctionName(inScope: nearest)
                let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(identifier: SyntaxFactory.makeToken(.identifier(ancestorFunctionName), presence: .present), declNameArguments: nil)), leftParen: SyntaxFactory.makeLeftParenToken(), argumentList: SyntaxFactory.makeBlankTupleExprElementList(), rightParen: SyntaxFactory.makeRightParenToken(), trailingClosure: nil, additionalTrailingClosures: nil).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                return trailingClosure.statements.appending(SyntaxFactory.makeCodeBlockItem(item: Syntax(ancestorFunctionCall), semicolon: nil, errorTokens: nil))
            default: fatalError("unexpected methodCall")
            }
        }()
        
        // we do actually have one example of a propertly nested beforeEach – see "State WaitingForDeregistration" in PushActivationStateMachine tests. not sure we have any afterEach
        
        let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
            attributes: nil,
            modifiers: nil,
            funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
            identifier: SyntaxFactory.makeIdentifier(methodName),
            genericParameterClause: nil,
            signature: SyntaxFactory.makeFunctionSignature(input: SyntaxFactory.makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(), parameterList: SyntaxFactory.makeBlankFunctionParameterList(), rightParen: SyntaxFactory.makeRightParenToken()), asyncOrReasyncKeyword: nil, throwsOrRethrowsKeyword: nil, output: nil),
            genericWhereClause: nil,
            body: SyntaxFactory.makeCodeBlock(leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)), statements: newStatements, rightBrace: trailingClosure.rightBrace
                                             )
        ).withLeadingTrivia(functionCallExpr.leadingTrivia!).withTrailingTrivia(functionCallExpr.trailingTrivia!)
        
        return SyntaxFactory.makeMemberDeclListItem(
            decl: DeclSyntax(testFunctionDeclaration),
            semicolon: nil
        )
    }
    
    enum QuickSpecMethodCall {
        case beforeEach
        case afterEach
        case it(testDescription: String, skipped: Bool)
        
        private var isSkipped: Bool {
            guard case let .it(testDescription: _, skipped: skipped) = self else {
                return false
            }
            
            return skipped
        }
        
        private var description: String {
            switch (self) {
            case .beforeEach: return "beforeEach"
            case .afterEach: return "afterEach"
            case let .it(testDescription: testDescription, skipped: _): return testDescription
            }
        }
        
        private var generatesTestMethod: Bool {
            switch (self) {
            case .it: return true
            default: return false
            }
        }
        
        init(functionCallExpr: FunctionCallExprSyntax) {
            // TODO DRY up with some other places
            let identifierExpression = IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression))!
            
            switch (identifierExpression.identifier.text) {
            case "beforeEach": self = .beforeEach
            case "afterEach": self = .afterEach
            default: fatalError("this initializer isn't ready for other function names yet")
            }
        }
        
        func outputFunctionName(inScope scope: Scope) -> String {
            let unsanitisedComponents: [String] = {
                switch (self) {
                case .beforeEach, .afterEach: return [description] + scope.map { $0.methodNameComponent }
                case .it: return scope.map { $0.methodNameComponent } + [description]
                }
            }()
            
            let unsantisedName = ((!generatesTestMethod || unsanitisedComponents[0].starts(with: "test")) ? "" : "test") + unsanitisedComponents.joined(separator: "_")
            
            let withoutSymbols = unsantisedName.components(separatedBy: CharacterSet.symbols.union(CharacterSet.punctuationCharacters)).joined(separator: "_")
            let withoutWhitespace = withoutSymbols.components(separatedBy: CharacterSet.whitespaces).joined(separator: "_")
            
            // TODO iterate on this, probably want some camelCase instead of underscores, and to be more clever when we have a `describe` that matches the test class name
            
            return (isSkipped || scope.isSkipped ? "skipped_" : "") + withoutWhitespace
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
