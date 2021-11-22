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
        // The aim is to reproduce the naming logic used by Quick, so we
        // can easily compare test order for debugging.
        var unsanitisedComponents: [String] = {
            switch self {
            case .hook(.beforeEach), .hook(.afterEach):
                return [description].compactMap { $0 } + scope.levels.map(\.methodNameComponent)
                    .compactMap { $0 }
            case .it:
                return scope.levels.map(\.methodNameComponent).compactMap { $0 } + [description]
                    .compactMap { $0 }
            }
        }()

        if generatesTestMethod {
            unsanitisedComponents.insert("test", at: 0)
        }

        if isSkipped || scope.isSkipped {
            unsanitisedComponents.insert("skipped", at: 0)
        }

        // This the joining logic used in Quick@c81db82, Example.swift#name
        let unsanitisedName = unsanitisedComponents.joined(separator: ", ")

        return unsanitisedName.c99ExtendedIdentifier
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

// This is copied from Quick@c81db82, file String+C99ExtendedIdentifier.swift
// TODO: check attribution requirements
extension String {
    private static var invalidCharacters: CharacterSet = {
        var invalidCharacters = CharacterSet()

        let invalidCharacterSets: [CharacterSet] = [
            .whitespacesAndNewlines,
            .illegalCharacters,
            .controlCharacters,
            .punctuationCharacters,
            .nonBaseCharacters,
            .symbols,
        ]

        for invalidSet in invalidCharacterSets {
            invalidCharacters.formUnion(invalidSet)
        }

        return invalidCharacters
    }()

    internal var c99ExtendedIdentifier: String {
        let validComponents = components(separatedBy: String.invalidCharacters)
        let result = validComponents.joined(separator: "_")

        return result.isEmpty ? "_" : result
    }
}
