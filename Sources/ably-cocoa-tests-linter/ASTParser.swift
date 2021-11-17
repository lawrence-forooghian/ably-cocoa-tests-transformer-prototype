import SwiftSyntax

class ASTParser {
    static func parseClassDeclaration(_ classDeclaration: ClassDeclSyntax) -> ClassContents {
        return ClassContents(contents: classDeclaration.members.members.map { member in
            return parseClassMember(member)
        })
    }
    
    private static func parseClassMember(_ member: MemberDeclListItemSyntax) -> ClassContents.`Type` {
        guard let specFunctionDecl = member.decl.as(FunctionDeclSyntax.self), specFunctionDecl.identifier.text == "spec" else {
            return .member(member)
        }
        
        let contents = parseSpecOrReusableTestsFunctionDeclaration(
            specFunctionDecl
        )

        return .scope(ASTScope(type: .spec, contents: contents))
    }

    private static func parseSpecOrReusableTestsFunctionDeclaration(
        _ functionDeclaration: FunctionDeclSyntax
    ) -> [ASTScope.Item] {
        guard let functionBody = functionDeclaration.body else {
            fatalError("Don’t know how to handle function declaration without a body")
        }

        return parseStatements(
            functionBody.statements
        )
    }

    private static func parseStatements(
        _ statements: CodeBlockItemListSyntax
    ) -> [ASTScope.Item] {
        return statements.map { statement -> ASTScope.Item in
            // It's a load of CodeBlockItemSyntax, for the variable declarations, then the beforeEach / afterEach, then the describe

            // TODO: what if there's stuff that clashes?

            if let variableDeclaration = VariableDeclSyntax(statement.item) {
                return .variableDeclaration(variableDeclaration)
            } else if let functionCallExpr = FunctionCallExprSyntax(statement.item) {
                return parseFunctionCall(
                    functionCallExpr
                )
            } else if let structDeclaration = StructDeclSyntax(statement.item) {
                return .structDeclaration(structDeclaration)
            } else if let functionDeclaration = FunctionDeclSyntax(statement.item) {
                // TODO: let's emit a warning when this returns no test cases? probably means we unrolled a loop incorrectly
                if functionDeclaration.identifier.text.starts(with: "reusableTests") {
                    let contents = parseSpecOrReusableTestsFunctionDeclaration(functionDeclaration)
                    let scope = ASTScope(type: .reusableTests(functionName: functionDeclaration.identifier.text), contents: contents)
                    return .scope(scope)
                }
                
                return .functionDeclaration(functionDeclaration)
            } else {
//                fatalError("\tTODO handle \(scope)-level \(statement.item)")
                fatalError("\tTODO parse \(statement.item)") // TODO improve error message
            }
        }
    }

    private static func parseFunctionCall(
        _ functionCallExpr: FunctionCallExprSyntax
    ) -> ASTScope.Item {
        guard let identifierExpression =
            IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression))
        else {
            preconditionFailure("Expected an identifier, but got \(functionCallExpr)")
        }

        // Not exactly sure what .text is but it seems to not have whitespace / comments etc
        let calledFunctionName = identifierExpression.identifier.text

        switch calledFunctionName {
        case "it":
            return parseItFunctionCall(functionCallExpr, skipped: false)
        case "xit":
            return parseItFunctionCall(functionCallExpr, skipped: true)
        case "describe", "xdescribe", "context", "xcontext":
            return parseDescribeOrContextFunctionCall(
                functionCallExpr,
                skipped: calledFunctionName.starts(with: "x")
            )
        case "beforeEach":
            return .beforeEach(functionCallExpr)
        case "afterEach":
            return .afterEach(functionCallExpr)
        default:
            if calledFunctionName.starts(with: "reusableTests") {
                return .reusableTestsCall(functionCallExpr, calledFunctionName: calledFunctionName)
            }

//            preconditionFailure("Unexpected \(scope)-level call to \(calledFunctionName)") // TODO restore scope in message
            preconditionFailure("Unexpected call to \(calledFunctionName)")
        }
    }

    private static func parseDescribeOrContextFunctionCall(
        _ functionCallExpr: FunctionCallExprSyntax,
        skipped: Bool
    ) -> ASTScope.Item {
        guard let trailingClosure = functionCallExpr.trailingClosure else {
            // TODO: DRY up with `it`
            preconditionFailure("Expected a trailing closure")
        }

        let description = QuickSpecMethodCall.getFunctionArgument(functionCallExpr)
        
        let contents = parseStatements( trailingClosure.statements )
        return .scope(ASTScope(type: .describeOrContext(description: description, skipped: skipped), contents: contents))
    }

    private static func parseItFunctionCall(
        _ functionCallExpr: FunctionCallExprSyntax,
        skipped: Bool
    ) -> ASTScope.Item {
        let testDescription = QuickSpecMethodCall.getFunctionArgument(functionCallExpr)
        return .it(functionCallExpr, description: testDescription, skipped: skipped)
    }
}
