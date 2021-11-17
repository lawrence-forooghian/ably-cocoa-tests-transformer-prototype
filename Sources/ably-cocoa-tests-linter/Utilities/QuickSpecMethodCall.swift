import Foundation
import SwiftSyntax

enum QuickSpecMethodCall {
    case hook(_: HookType)
    case it(testDescription: String, skipped: Bool)

    private var isSkipped: Bool {
        guard case let .it(testDescription: _, skipped: skipped) = self else {
            return false
        }

        return skipped
    }

    private var description: String {
        switch self {
        case .hook(.beforeEach): return "beforeEach"
        case .hook(.afterEach): return "afterEach"
        case let .it(testDescription: testDescription, skipped: _): return testDescription
        }
    }

    private var generatesTestMethod: Bool {
        switch self {
        case .it: return true
        default: return false
        }
    }

    init(functionCallExpr: FunctionCallExprSyntax) {
        // TODO: DRY up with some other places
        let identifierExpression =
            IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression))!

        switch identifierExpression.identifier.text {
        case "beforeEach": self = .hook(.beforeEach)
        case "afterEach": self = .hook(.afterEach)
        default: fatalError("this initializer isn't ready for other function names yet")
        }
    }

    func outputFunctionName(inScope scope: AST.Scope) -> String {
        let unsanitisedComponents: [String] = {
            switch self {
            case .hook(.beforeEach),
                 .hook(.afterEach): return [description] + scope.levels
                .map(\.methodNameComponent)
            case .it: return scope.levels.map(\.methodNameComponent) + [description]
            }
        }()

        let unsanitisedName =
            ((!generatesTestMethod || unsanitisedComponents[0].starts(with: "test")) ? "" :
                "test") + unsanitisedComponents.joined(separator: "_")

        let withoutSymbols = unsanitisedName
            .components(separatedBy: CharacterSet.symbols
                .union(CharacterSet.punctuationCharacters)).joined(separator: "_")
        let withoutWhitespace = withoutSymbols.components(separatedBy: CharacterSet.whitespaces)
            .joined(separator: "_")

        // TODO: iterate on this, probably want some camelCase instead of underscores, and to be more clever when we have a `describe` that matches the test class name

        return (isSkipped || scope.isSkipped ? "skipped_" : "") + withoutWhitespace
    }

    // TODO: can probably integrate this better with the class
    // gets the argument for `it` / `describe` / `context` etc
    static func getFunctionArgument(_ functionCallExpr: FunctionCallExprSyntax) -> String {
        // TODO: update function name from `it` here

        precondition(
            functionCallExpr.argumentList.count == 1,
            "`it` should only take one argument"
        )

        // OK, this is a ExprSyntax, how do I find out whether it's a string literal?
        let argumentExpression = functionCallExpr.argumentList.first!.expression

        guard let stringLiteralExpression = argumentExpression.as(StringLiteralExprSyntax.self)
        else {
            preconditionFailure(
                "Expected the one argument to `it` to be a string literal describing the test"
            )
        }

        precondition(
            stringLiteralExpression.segments.count == 1,
            "the argument to `it` I'm expecting to only have one segment"
        )

        let firstSegment = stringLiteralExpression.segments.first!
        // TODO: is this okay? Wasn't sure how to keep drilling
        let testDescription = firstSegment.firstToken!.text

        return testDescription
    }
}
