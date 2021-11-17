import SwiftSyntax

enum ASTTransform {
    struct ClassTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclaration: AST.ClassDeclaration
    }

    static func transformClassDeclaration(_ classDeclaration: AST.ClassDeclaration)
        -> ClassTransformationResult
    {
        let transformationResults = classDeclaration.items.map(transformClassDeclarationItem)

        return ClassTransformationResult(
            globalDeclarations: transformationResults.flatMap(\.globalDeclarations),
            classDeclaration: classDeclaration
                .replacingItems(transformationResults.flatMap(\.classDeclarationItems))
        )
    }

    struct ItemTransformationResult {
        var globalDeclarations: [DeclSyntax]
        var classDeclarationItems: [AST.ClassDeclaration.Item]

        init(classDeclarationItem: AST.ClassDeclaration.Item) {
            globalDeclarations = []
            classDeclarationItems = [classDeclarationItem]
        }
    }

    private static func transformClassDeclarationItem(_ item: AST.ClassDeclaration.Item)
        -> ItemTransformationResult
    {
        // I think the only class-level thing we want to manipulate is the `spec` function – everything else
        // we can pass through

        switch item {
        case let .member(member): return ItemTransformationResult(classDeclarationItem: item)
        case let .spec(spec): return foo()
        }
    }

    /*
     private static func transformSpecOrReusableTestsFunctionDeclaration(
         _ functionDeclaration: FunctionDeclSyntax,
         createDefinedScope: (ScopeMember.ContentsInfo) -> Scope
     ) -> ItemTransformationResult {
         guard let functionBody = functionDeclaration.body else {
             fatalError("Don’t know how to handle function declaration without a body")
         }

         let contentsInfo = ScopeMember.ContentsInfo(statements: functionBody.statements)

         return transformStatements(
             functionBody.statements,
             immediatelyInsideScope: createDefinedScope(contentsInfo)
         )
     }

     private static func transformStatements(
         _ statements: CodeBlockItemListSyntax,
         immediatelyInsideScope scope: Scope
     ) -> ItemTransformationResult {
         // TODO: remove references to spec() here, and check they still apply

         let memberDeclListItems = statements.map { statement -> ItemTransformationResult in
             // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe

             // TODO: what if there's stuff that clashes?

             // TODO: if we wanted to split up the different aspects of transformation (class level, global level) best would be to create some domain-specific AST-like thing and then do whatever we want with that afterwards

             if let variableDeclaration = VariableDeclSyntax(statement.item) {
                 if scope.isReusableTests {
                     // it's not a function call's closure we're inside, it's a function body with local variables, which will remain a function, so can keep its variables intact
                     let decl = DeclSyntax(variableDeclaration)
                     return ItemTransformationResult(
                         classLevelDeclarations: [MemberDeclListItemSyntax { builder in
                             builder.useDecl(decl)
                         }],
                         globalDeclarations: []
                     )
                 }

                 // Variable declarations in the body of a trailing closure passed to spec / describe etc
                 // get hoisted to private global variables
                 let leadingTrivia = variableDeclaration.leadingTrivia!
                 var modifiedToPrivateVariableDeclaration = variableDeclaration
                 modifiedToPrivateVariableDeclaration.leadingTrivia = .zero

                 let oldModifiers = modifiedToPrivateVariableDeclaration.modifiers ?? SyntaxFactory
                     .makeModifierList([])

                 let newModifiers = oldModifiers.prepending(SyntaxFactory.makeDeclModifier(
                     name: SyntaxFactory.makePrivateKeyword(),
                     detailLeftParen: nil,
                     detail: nil,
                     detailRightParen: nil
                 ).withLeadingTrivia(leadingTrivia).withTrailingTrivia(.spaces(1)))

                 modifiedToPrivateVariableDeclaration = modifiedToPrivateVariableDeclaration
                     .withModifiers(newModifiers)

                 return ItemTransformationResult(
                     classLevelDeclarations: [],
                     globalDeclarations: [DeclSyntax(modifiedToPrivateVariableDeclaration)]
                 )
             } else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                 return transformFunctionCall(
                     functionCallExpr,
                     insideScope: scope
                 )
             } else if let structDeclaration = StructDeclSyntax(statement.item) {
                 // Struct declarations just get hoisted outside of spec()
                 // We only have one of these

                 let decl = DeclSyntax(structDeclaration)
                 return ItemTransformationResult(
                     classLevelDeclarations: [MemberDeclListItemSyntax { builder in
                         builder.useDecl(decl)
                     }],
                     globalDeclarations: []
                 )
             } else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                 // TODO: The alternative here would probably be, instead of allow-listing everything,
                 // to treat any function that contains calls to `context` etc as an instance of this case
                 // TODO: let's emit a warning when thsi returns no test cases? probably means we unrolled a loop incorrectly
                 if functionDeclaration.identifier.text.starts(with: "reusableTests") {
                     // This is a special case that defines a bunch of contexts etc, we treat it similarly to a `spec` call
                     // but we preserve the containing function and make it also invoke all of the test cases

                     let declarations = transformSpecOrReusableTestsFunctionDeclaration(
                         functionDeclaration,
                         // TODO: is there ever a reason why might want to create a nested scope here? e.g. if we have a reusableTests function nested inside a reusableTests? It's academic, doesn't apply to ably-cocoa I'm 99.9% sure
                         createDefinedScope: { [ScopeMember(
                             type: .reusableTests(functionName: functionDeclaration.identifier.text),
                             contentsInfo: $0
                         )] }
                     ).classLevelDeclarations

                     // OK, we need to embed this inside a class
                     // wait, no, we'll keep it as a function call

                     var newFunctionDeclaration = functionDeclaration

                     // TODO: this is also probably hacky - we're taking a bunch of class member items and turning them back into just declarations. Not necessary
                     let codeBlockItemsFromFunction = declarations.map { declaration in
                         SyntaxFactory.makeCodeBlockItem(
                             item: Syntax(declaration.decl),
                             semicolon: nil,
                             errorTokens: nil
                         )
                     }

                     // TODO: this is a bit hacky – we're essentially figuring out which
                     // test methods were created by transformSpecFunctionDeclarationIntoClassLevelDeclarations
                     // but we could probably just improve things to make it tell us that
                     let testFunctionDeclarations = declarations
                         .compactMap { declaration -> FunctionDeclSyntax? in
                             guard let functionDeclaration = declaration.decl
                                 .as(FunctionDeclSyntax.self)
                             else {
                                 return nil
                             }
                             if !functionDeclaration.identifier.text.starts(with: "test") {
                                 // TODO: should we handle skipped functions in some nicer way?
                                 return nil
                             }
                             return functionDeclaration
                         }

                     // We now invoke all of these functions.
                     let testFunctionInvocationCodeBlockItems = testFunctionDeclarations
                         .map { declaration -> CodeBlockItemSyntax in
                             let testFunctionInvocationExpression = SyntaxFactory
                                 .makeFunctionCallExpr(
                                     calledExpression: ExprSyntax(SyntaxFactory
                                         .makeIdentifierExpr(identifier: declaration.identifier,
                                                             declNameArguments: nil)),
                                     leftParen: SyntaxFactory.makeLeftParenToken(),
                                     argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                                     rightParen: SyntaxFactory.makeRightParenToken(),
                                     trailingClosure: nil,
                                     additionalTrailingClosures: nil
                                 ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                             return SyntaxFactory.makeCodeBlockItem(
                                 item: Syntax(testFunctionInvocationExpression),
                                 semicolon: nil,
                                 errorTokens: nil
                             )
                         }

                     // TODO: should we stick some logging into these test functions or something?

                     newFunctionDeclaration.body!.statements = SyntaxFactory
                         .makeCodeBlockItemList(codeBlockItemsFromFunction +
                             testFunctionInvocationCodeBlockItems)

                     newFunctionDeclaration =
                         addingContextToReusableTestsFunctionDeclaration(newFunctionDeclaration)

                     return ItemTransformationResult(
                         classLevelDeclarations: [MemberDeclListItemSyntax { builder in
                             builder.useDecl(DeclSyntax(newFunctionDeclaration))
                         }],
                         globalDeclarations: []
                     )
                 }

                 if scope.isReusableTests {
                     // it's not a function call's closure we're inside, it's a function body with local functions, which will remain a function, so can keep its functions intact
                     // TODO: see if we actually have any of this in our codebase
                     let decl = DeclSyntax(functionDeclaration)
                     return ItemTransformationResult(
                         classLevelDeclarations: [MemberDeclListItemSyntax { builder in
                             builder.useDecl(decl)
                         }],
                         globalDeclarations: []
                     )
                 }

                 // Function declarations in the body of a trailing closure passed to spec / describe etc
                 // get hoisted to private global functions
                 let leadingTrivia = functionDeclaration.leadingTrivia!
                 var modifiedToPrivateFunctionDeclaration = functionDeclaration
                 modifiedToPrivateFunctionDeclaration.leadingTrivia = .zero

                 // TODO: DRY up this privatising with variable declarations
                 let oldModifiers = modifiedToPrivateFunctionDeclaration.modifiers ?? SyntaxFactory
                     .makeModifierList([])

                 let newModifiers = oldModifiers.prepending(SyntaxFactory.makeDeclModifier(
                     name: SyntaxFactory.makePrivateKeyword(),
                     detailLeftParen: nil,
                     detail: nil,
                     detailRightParen: nil
                 ).withLeadingTrivia(leadingTrivia).withTrailingTrivia(.spaces(1)))

                 modifiedToPrivateFunctionDeclaration = modifiedToPrivateFunctionDeclaration
                     .withModifiers(newModifiers)

                 return ItemTransformationResult(
                     classLevelDeclarations: [],
                     globalDeclarations: [DeclSyntax(modifiedToPrivateFunctionDeclaration)]
                 )
             } else {
                 print("\tTODO handle \(scope)-level \(statement.item)")
                 return ItemTransformationResult(
                     classLevelDeclarations: [],
                     globalDeclarations: []
                 )
             }
         }
         .reduce(ItemTransformationResult(classLevelDeclarations: [],
                                            globalDeclarations: [
                                            ]) /* TODO: an `empty` method */ ) { accum, val in
             // TODO: from a Swifty point of view it would be nice to define a `+` here
             ItemTransformationResult(
                 classLevelDeclarations: accum.classLevelDeclarations + val.classLevelDeclarations,
                 globalDeclarations: accum.globalDeclarations + val.globalDeclarations
             )
         }

         return memberDeclListItems
     }

     private static func addingContextToReusableTestsFunctionDeclaration(_ decl: FunctionDeclSyntax)
         -> FunctionDeclSyntax
     {
         // We add a `context: (beforeEach: () -> (), afterEach: () -> ())` arg to all these functions

         var parameterList = decl.signature.input.parameterList

         var hasTrailingClosure = false

         if !parameterList.isEmpty {
             let finalParam = parameterList.last!

             let finalParamBaseType: TypeSyntax?

             if let attributedType = finalParam.type?.as(AttributedTypeSyntax.self) {
                 finalParamBaseType = attributedType.baseType
             } else if let _ = finalParam.type?.as(SimpleTypeIdentifierSyntax.self) {
                 // we only care whether it's a closure and I suppose it's not
                 finalParamBaseType = nil
             } else if let _ = finalParam.type?.as(TupleTypeSyntax.self) {
                 // we only care whether it's a closure and I suppose it's not
                 finalParamBaseType = nil
             } else {
                 preconditionFailure("I don't know how to handle \(finalParam.type!.syntaxNodeType)")
             }

             hasTrailingClosure = finalParamBaseType?.is(FunctionTypeSyntax.self) == true
         }

         if (!hasTrailingClosure && !parameterList.isEmpty) ||
             (hasTrailingClosure && parameterList.count > 1)
         {
             // This seems like a faff, no doubt I don't know enough about
             // Swift collections
             let finalParam =
                 parameterList[parameterList
                     .index(parameterList.endIndex, offsetBy: hasTrailingClosure ? -2 : -1)]

             // Add trailing comma to final param (or penultimate if has trailing closure)
             var newFinalParam = finalParam
             newFinalParam = newFinalParam.withTrailingComma(SyntaxFactory.makeCommaToken())
             parameterList = parameterList.replacing(
                 childAt: parameterList.count - (hasTrailingClosure ? 2 : 1),
                 with: newFinalParam
             )
         }

         let parameterType = SyntaxFactory
             .makeTypeIdentifier("(beforeEach: (() -> ())?, afterEach: (() -> ())?)")
         let parameter = SyntaxFactory.makeFunctionParameter(
             attributes: nil,
             firstName: SyntaxFactory.makeIdentifier("context"),
             secondName: nil,
             colon: SyntaxFactory.makeColonToken(),
             type: parameterType,
             ellipsis: nil,
             defaultArgument: nil,
             trailingComma: hasTrailingClosure ? SyntaxFactory.makeCommaToken() : nil
         )
         parameterList = parameterList.inserting(
             parameter,
             at: hasTrailingClosure ? parameterList.count - 1 : parameterList.count
         )

         var newDecl = decl
         newDecl.signature.input.parameterList = parameterList

         return newDecl
     }

     private static func transformFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         insideScope scope: Scope
     ) -> ItemTransformationResult {
         guard let identifierExpression =
             IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression))
         else {
             preconditionFailure("Expected an identifier, but got \(functionCallExpr)")
         }

         // Not exactly sure what .text is but it seems to not have whitespace / comments etc
         let calledFunctionName = identifierExpression.identifier.text

         switch calledFunctionName {
         case "it":
             return transformItFunctionCall(functionCallExpr, insideScope: scope, skipped: false)
         case "xit":
             return transformItFunctionCall(functionCallExpr, insideScope: scope, skipped: true)
         case "describe", "xdescribe", "context", "xcontext":
             return transformDescribeOrContextFunctionCall(
                 functionCallExpr,
                 insideScope: scope,
                 skipped: calledFunctionName.starts(with: "x")
             )
         case "beforeEach", "afterEach":
             return transformBeforeOrAfterEachFunctionCall(
                 functionCallExpr,
                 insideScope: scope
             )
         default:
             if calledFunctionName.starts(with: "reusableTests") {
                 return transformReusableTestsFunctionCall(
                     functionCallExpr,
                     calledFunctionName: calledFunctionName,
                     insideScope: scope
                 )
             }

             preconditionFailure("Unexpected \(scope)-level call to \(calledFunctionName)")
         }
     }

     private static func transformDescribeOrContextFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         insideScope scope: Scope,
         skipped: Bool
     ) -> ItemTransformationResult {
         guard let trailingClosure = functionCallExpr.trailingClosure else {
             // TODO: DRY up with `it`
             preconditionFailure("Expected a trailing closure")
         }

         let description = QuickSpecMethodCall.getFunctionArgument(functionCallExpr)

         // do a preflight to fetch some info about the scope's contents - hasOwnBeforeEach, hasOwnAfterEach
         // TODO: why does this give fewer hasOwnBeforeEach than we have in the codebase? I'm sure we'll find out in time
         let contentsInfo = ScopeMember.ContentsInfo(statements: trailingClosure.statements)

         let scopeMember = ScopeMember(
             type: .describeOrContext(description: description,
                                      skipped: skipped),
             contentsInfo: contentsInfo
         )

         var transformationResult = transformStatements(
             trailingClosure.statements,
             immediatelyInsideScope: scope + [scopeMember]
         )
         if !transformationResult.classLevelDeclarations.isEmpty {
             // preserve any comments that came alongside the function call
             // TODO: it's a bit messed up though, see e.g. "32 bytes" comment
             // and a bunch of unwanted whitespace
             transformationResult.classLevelDeclarations[0].leadingTrivia = functionCallExpr
                 .leadingTrivia! + transformationResult.classLevelDeclarations[0].leadingTrivia!
         }
         return transformationResult
     }

     private static func addContextToReusableTestsFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         insideScope scope: Scope
     )
         -> FunctionCallExprSyntax
     {
         var newFunctionCallExpr = functionCallExpr

         if newFunctionCallExpr.leftParen == nil {
             // build parens if doesn't already have them (e.g. if function call has a trailing closure and no other args)
             newFunctionCallExpr.leftParen = SyntaxFactory.makeLeftParenToken()
             newFunctionCallExpr.rightParen = SyntaxFactory.makeRightParenToken()
         }

         // add a trailing comma to current final arg
         var newArgumentList = newFunctionCallExpr.argumentList
         if !newArgumentList.isEmpty {
             let index = newArgumentList.index(newArgumentList.endIndex, offsetBy: -1)
             var newArgument = newArgumentList[index]
             newArgument = newArgument.withTrailingComma(SyntaxFactory.makeCommaToken())
             newArgumentList = newArgumentList.replacing(
                 childAt: newArgumentList.count - 1,
                 with: newArgument
             )
             newFunctionCallExpr.argumentList = newArgumentList
         }

         newFunctionCallExpr = newFunctionCallExpr.addArgument(SyntaxFactory.makeTupleExprElement(
             label: SyntaxFactory.makeIdentifier("context"),
             colon: SyntaxFactory.makeColonToken(),
             expression: ExprSyntax(makeContextTupleExpr(insideScope: scope)),
             trailingComma: nil
         ))
         return newFunctionCallExpr
     }

     private static func makeContextTupleExpr(insideScope scope: Scope) -> TupleExprSyntax {
         let beforeEachExpr: ExprSyntax
         if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
             ofType: .beforeEach,
             includeSelf: true
         ) {
             beforeEachExpr = ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                 identifier: SyntaxFactory.makeIdentifier(hookSource.outputFunctionName),
                 declNameArguments: nil
             ))
         } else {
             beforeEachExpr = ExprSyntax(SyntaxFactory
                 .makeNilLiteralExpr(nilKeyword: SyntaxFactory.makeNilKeyword()))
         }

         let afterEachExpr: ExprSyntax
         if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
             ofType: .afterEach,
             includeSelf: true
         ) {
             afterEachExpr = ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                 identifier: SyntaxFactory.makeIdentifier(hookSource.outputFunctionName),
                 declNameArguments: nil
             ))
         } else {
             afterEachExpr = ExprSyntax(SyntaxFactory
                 .makeNilLiteralExpr(nilKeyword: SyntaxFactory.makeNilKeyword()))
         }

         let elementList = SyntaxFactory.makeTupleExprElementList([
             SyntaxFactory.makeTupleExprElement(
                 label: SyntaxFactory.makeIdentifier("beforeEach"),
                 colon: SyntaxFactory.makeColonToken(),
                 expression: beforeEachExpr,
                 trailingComma: SyntaxFactory.makeCommaToken()
             ),
             SyntaxFactory.makeTupleExprElement(
                 label: SyntaxFactory.makeIdentifier("afterEach"),
                 colon: SyntaxFactory.makeColonToken(),
                 expression: afterEachExpr,
                 trailingComma: nil
             ),
         ])

         return SyntaxFactory.makeTupleExpr(
             leftParen: SyntaxFactory.makeLeftParenToken(),
             elementList: elementList,
             rightParen: SyntaxFactory.makeRightParenToken()
         )
     }

     // TODO: DRY up with transformItFunctionCallIntoClassLevelDeclaration
     private static func transformReusableTestsFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         calledFunctionName: String,
         insideScope scope: Scope
     ) -> ItemTransformationResult {
         // this reusableTests* function call gets turned into a method
         let methodName = QuickSpecMethodCall.it(testDescription: calledFunctionName, skipped: false)
             .outputFunctionName(inScope: scope)

         let newFunctionCallExpr = addContextToReusableTestsFunctionCall(
             functionCallExpr,
             insideScope: scope
         )

         let codeBlockItem = SyntaxFactory.makeCodeBlockItem(
             item: Syntax(newFunctionCallExpr),
             semicolon: nil,
             errorTokens: nil
         )
         let statements = SyntaxFactory.makeCodeBlockItemList([codeBlockItem])

         let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
             attributes: nil,
             modifiers: nil,
             funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
             identifier: SyntaxFactory.makeIdentifier(methodName),
             genericParameterClause: nil,
             signature: SyntaxFactory.makeFunctionSignature(
                 input: SyntaxFactory
                     .makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(),
                                          parameterList: SyntaxFactory
                                              .makeBlankFunctionParameterList(
                                              ),
                                          rightParen: SyntaxFactory.makeRightParenToken()),
                 asyncOrReasyncKeyword: nil,
                 throwsOrRethrowsKeyword: nil,
                 output: nil
             ),
             genericWhereClause: nil,
             body: SyntaxFactory.makeCodeBlock(
                 leftBrace: SyntaxFactory.makeLeftBraceToken().withLeadingTrivia(.spaces(1)),
                 statements: statements,
                 rightBrace: SyntaxFactory.makeRightBraceToken()
             )
         ).withLeadingTrivia(newFunctionCallExpr.leadingTrivia!)
             .withTrailingTrivia(newFunctionCallExpr.trailingTrivia!)

         return ItemTransformationResult(
             classLevelDeclarations: [SyntaxFactory.makeMemberDeclListItem(
                 decl: DeclSyntax(testFunctionDeclaration),
                 semicolon: nil
             )],
             globalDeclarations: []
         )
     }

     private static func transformItFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         insideScope scope: Scope,
         skipped: Bool
     ) -> ItemTransformationResult {
         // `it` gets turned into a method

         let testDescription = QuickSpecMethodCall.getFunctionArgument(functionCallExpr)

         let methodName = QuickSpecMethodCall.it(testDescription: testDescription, skipped: skipped)
             .outputFunctionName(inScope: scope)

         // Now we grab the trailing closure from the call to `it` and use that as the new test method's body

         guard let trailingClosure = functionCallExpr.trailingClosure else {
             preconditionFailure("I expect a call to `it` to have a trailing closure")
         }

         guard trailingClosure.signature == nil else {
             preconditionFailure(
                 "I don't expect the trailing closure to have any signature, but got \(trailingClosure)"
             )
         }

         // Insert a call to the before/afterEach of the scope this `it` is contained within.
         let newStatements: CodeBlockItemListSyntax = {
             var newStatements = trailingClosure.statements

             // TODO: DRY these up with the beforeEach / afterEach ancestor-calling code

             // beforeEach
             if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                 ofType: .beforeEach,
                 includeSelf: true
             ) {
                 let functionCall = SyntaxFactory.makeFunctionCallExpr(
                     calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                         identifier: SyntaxFactory
                             .makeToken(
                                 .identifier(hookSource.outputFunctionName),
                                 presence: .present
                             ),
                         declNameArguments: nil
                     )),
                     leftParen: SyntaxFactory.makeLeftParenToken(),
                     argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                     rightParen: SyntaxFactory.makeRightParenToken(),
                     trailingClosure: nil,
                     additionalTrailingClosures: nil
                 ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                 newStatements = newStatements
                     .prepending(SyntaxFactory
                         .makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil,
                                            errorTokens: nil))
             }

             // afterEach
             if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                 ofType: .afterEach,
                 includeSelf: true
             ) {
                 let functionCall = SyntaxFactory.makeFunctionCallExpr(
                     calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                         identifier: SyntaxFactory
                             .makeToken(
                                 .identifier(hookSource.outputFunctionName),
                                 presence: .present
                             ),
                         declNameArguments: nil
                     )),
                     leftParen: SyntaxFactory.makeLeftParenToken(),
                     argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                     rightParen: SyntaxFactory.makeRightParenToken(),
                     trailingClosure: nil,
                     additionalTrailingClosures: nil
                 ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                 newStatements = newStatements
                     .appending(SyntaxFactory
                         .makeCodeBlockItem(item: Syntax(functionCall), semicolon: nil,
                                            errorTokens: nil))
             }

             return newStatements
         }()

         let testFunctionDeclaration = SyntaxFactory.makeFunctionDecl(
             attributes: nil,
             modifiers: nil,
             funcKeyword: SyntaxFactory.makeFuncKeyword().withTrailingTrivia(.spaces(1)),
             identifier: SyntaxFactory.makeIdentifier(methodName),
             genericParameterClause: nil,
             signature: SyntaxFactory.makeFunctionSignature(
                 input: SyntaxFactory
                     .makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(),
                                          parameterList: SyntaxFactory
                                              .makeBlankFunctionParameterList(
                                              ),
                                          rightParen: SyntaxFactory.makeRightParenToken()),
                 asyncOrReasyncKeyword: nil,
                 throwsOrRethrowsKeyword: nil,
                 output: nil
             ),
             genericWhereClause: nil,
             body: SyntaxFactory.makeCodeBlock(
                 leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)),
                 statements: newStatements,
                 rightBrace: trailingClosure.rightBrace
             )
         ).withLeadingTrivia(functionCallExpr.leadingTrivia!)
             .withTrailingTrivia(functionCallExpr.trailingTrivia!)

         return ItemTransformationResult(
             classLevelDeclarations: [SyntaxFactory.makeMemberDeclListItem(
                 decl: DeclSyntax(testFunctionDeclaration),
                 semicolon: nil
             )],
             globalDeclarations: []
         )
     }

     private static func transformBeforeOrAfterEachFunctionCall(
         _ functionCallExpr: FunctionCallExprSyntax,
         insideScope scope: Scope
     ) -> ItemTransformationResult {
         // `beforeEach` or `afterEach` gets turned into a method

         let methodCall = QuickSpecMethodCall(functionCallExpr: functionCallExpr)
         let methodName = methodCall.outputFunctionName(inScope: scope)

         // Now we grab the trailing closure from the call to `before/afterEach` and use that as the new test method's body
         // TODO: we can probably DRY this up with the `it` equivalent

         guard let trailingClosure = functionCallExpr.trailingClosure else {
             preconditionFailure("I expect a call to `before/afterEach` to have a trailing closure")
         }

         guard trailingClosure.signature == nil else {
             preconditionFailure(
                 "I don't expect the trailing closure to have any signature, but got \(trailingClosure)"
             )
         }

         // Insert a call to the before/afterEach of the scope this one is nested within.
         let newStatements: CodeBlockItemListSyntax = {
             switch methodCall {
             // TODO: double-check the ordering of the before / after in relation to parents
             case .hook(.beforeEach):
                 var newStatements: CodeBlockItemListSyntax = trailingClosure.statements

                 if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                     ofType: .beforeEach,
                     includeSelf: false
                 ) {
                     let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                         calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                             identifier: SyntaxFactory
                                 .makeToken(
                                     .identifier(hookSource.outputFunctionName),
                                     presence: .present
                                 ),
                             declNameArguments: nil
                         )),
                         leftParen: SyntaxFactory.makeLeftParenToken(),
                         argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                         rightParen: SyntaxFactory.makeRightParenToken(),
                         trailingClosure: nil,
                         additionalTrailingClosures: nil
                     ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                     newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(
                         item: Syntax(ancestorFunctionCall),
                         semicolon: nil,
                         errorTokens: nil
                     ))
                 }

                 if scope.isReusableTests {
                     let functionName = "context.beforeEach?"

                     let contextFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                         calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                             identifier: SyntaxFactory
                                 .makeToken(.identifier(functionName), presence: .present),
                             declNameArguments: nil
                         )),
                         leftParen: SyntaxFactory.makeLeftParenToken(),
                         argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                         rightParen: SyntaxFactory.makeRightParenToken(),
                         trailingClosure: nil,
                         additionalTrailingClosures: nil
                     ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                     newStatements = newStatements.prepending(SyntaxFactory.makeCodeBlockItem(
                         item: Syntax(contextFunctionCall),
                         semicolon: nil,
                         errorTokens: nil
                     ))
                 }

                 return newStatements
             case .hook(.afterEach):
                 var newStatements: CodeBlockItemListSyntax = trailingClosure.statements

                 if let hookSource = scope.hookSourceOfNearestAncestorHavingOwnHookSource(
                     ofType: .afterEach,
                     includeSelf: false
                 ) {
                     let ancestorFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                         calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                             identifier: SyntaxFactory
                                 .makeToken(
                                     .identifier(hookSource.outputFunctionName),
                                     presence: .present
                                 ),
                             declNameArguments: nil
                         )),
                         leftParen: SyntaxFactory.makeLeftParenToken(),
                         argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                         rightParen: SyntaxFactory.makeRightParenToken(),
                         trailingClosure: nil,
                         additionalTrailingClosures: nil
                     ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))
                     newStatements = trailingClosure.statements
                         .appending(SyntaxFactory.makeCodeBlockItem(
                             item: Syntax(ancestorFunctionCall),
                             semicolon: nil,
                             errorTokens: nil
                         ))
                 }

                 if scope.isReusableTests {
                     let functionName = "context.afterEach?"

                     let contextFunctionCall = SyntaxFactory.makeFunctionCallExpr(
                         calledExpression: ExprSyntax(SyntaxFactory.makeIdentifierExpr(
                             identifier: SyntaxFactory
                                 .makeToken(.identifier(functionName), presence: .present),
                             declNameArguments: nil
                         )),
                         leftParen: SyntaxFactory.makeLeftParenToken(),
                         argumentList: SyntaxFactory.makeBlankTupleExprElementList(),
                         rightParen: SyntaxFactory.makeRightParenToken(),
                         trailingClosure: nil,
                         additionalTrailingClosures: nil
                     ).withLeadingTrivia(.newlines(1)).withTrailingTrivia(.newlines(1))

                     newStatements = newStatements.appending(SyntaxFactory.makeCodeBlockItem(
                         item: Syntax(contextFunctionCall),
                         semicolon: nil,
                         errorTokens: nil
                     ))
                 }

                 return newStatements
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
             signature: SyntaxFactory.makeFunctionSignature(
                 input: SyntaxFactory
                     .makeParameterClause(leftParen: SyntaxFactory.makeLeftParenToken(),
                                          parameterList: SyntaxFactory
                                              .makeBlankFunctionParameterList(
                                              ),
                                          rightParen: SyntaxFactory.makeRightParenToken()),
                 asyncOrReasyncKeyword: nil,
                 throwsOrRethrowsKeyword: nil,
                 output: nil
             ),
             genericWhereClause: nil,
             body: SyntaxFactory.makeCodeBlock(
                 leftBrace: trailingClosure.leftBrace.withLeadingTrivia(.spaces(1)),
                 statements: newStatements,
                 rightBrace: trailingClosure.rightBrace
             )
         ).withLeadingTrivia(functionCallExpr.leadingTrivia!)
             .withTrailingTrivia(functionCallExpr.trailingTrivia!)

         return ItemTransformationResult(
             classLevelDeclarations: [SyntaxFactory.makeMemberDeclListItem(
                 decl: DeclSyntax(testFunctionDeclaration),
                 semicolon: nil
             )],
             globalDeclarations: []
         )
     }
     */
}
