import SwiftSyntax

struct ScopeMember: CustomStringConvertible {
    enum `Type`: CustomStringConvertible {
        case spec
        case reusableTests(functionName: String)
        case describeOrContext(description: String, skipped: Bool)

        var description: String {
            switch self {
            case .spec: return "spec()"
            case let .reusableTests(functionName: functionName): return "reusableTests(\(functionName)"
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

            func mangledNameForScope(_: Scope) -> String {
                return "TODOmangledName"
            }
        }

        init(
            hasOwnBeforeEach: Bool,
            hasOwnAfterEach: Bool,
            variableDefinitions: [VariableDefinition]
        ) {
            self.hasOwnBeforeEach = hasOwnBeforeEach
            self.hasOwnAfterEach = hasOwnAfterEach
            self.variableDefinitions = variableDefinitions
        }

        init(statements: CodeBlockItemListSyntax) {
            self = statements.reduce(ContentsInfo(
                hasOwnBeforeEach: false,
                hasOwnAfterEach: false,
                variableDefinitions: []
            )) { result, statement -> ContentsInfo in
                var newResult = result

                // beforeEach / afterEach
                if let functionCallExpr = FunctionCallExprSyntax(statement.item),
                   let identifierExpression =
                   IdentifierExprSyntax(Syntax(functionCallExpr.calledExpression))
                {
                    // (copied comment from elsewhere) Not exactly sure what .text is but it seems to not have whitespace / comments etc
                    let calledFunctionName = identifierExpression.identifier.text

                    newResult.hasOwnBeforeEach = newResult
                        .hasOwnBeforeEach || (calledFunctionName == "beforeEach")
                    newResult.hasOwnAfterEach = newResult
                        .hasOwnAfterEach || (calledFunctionName == "afterEach")
                }

                // variable declarations
                if let variableDeclarationSyntax = VariableDeclSyntax(statement.item) {
                    guard variableDeclarationSyntax.bindings.count == 1 else {
                        preconditionFailure(
                            "I don’t know how to handle variable declarations with multiple bindings"
                        )
                    }
                    let binding = variableDeclarationSyntax.bindings.first!

                    let variableDeclarations: [ScopeMember.ContentsInfo.VariableDefinition]

                    if let identifierPattern = binding.pattern.as(IdentifierPatternSyntax.self) {
                        // TODO: this must be wrong!
                        let isMutable = (variableDeclarationSyntax.letOrVarKeyword.text == "var")
                        variableDeclarations = [ScopeMember.ContentsInfo.VariableDefinition(
                            originalName: identifierPattern.identifier.text /* is this correct? */,
                            isMutable: isMutable
                        )]
                    } else if let tuplePattern = binding.pattern.as(TuplePatternSyntax.self) {
                        variableDeclarations = tuplePattern.elements.map { element in
                            guard let identifierPattern = element.pattern
                                .as(IdentifierPatternSyntax.self)
                            else {
                                preconditionFailure(
                                    "I don't know how to handle a binding in a tuple that's not an identifier: \(binding.pattern)"
                                )
                            }
                            let isMutable = (variableDeclarationSyntax.letOrVarKeyword
                                .text == "let")

                            // TODO: DRY up with the above
                            return ScopeMember.ContentsInfo.VariableDefinition(
                                originalName: identifierPattern.identifier
                                    .text /* is this correct? */,
                                isMutable: isMutable
                            )
                        }

                    } else {
                        preconditionFailure(
                            "I don’t know how to handle bindings with patterns other than an identifier or a tuple: \(binding.pattern)"
                        )
                    }

                    newResult.variableDefinitions += variableDeclarations
                }

                return newResult
            }
        }
    }

    var type: `Type`
    var contentsInfo: ContentsInfo

    var methodNameComponent: String {
        switch type {
        case .spec: return ""
        case .reusableTests: return ""
        case let .describeOrContext(description: description, skipped: _): return description
        }
    }

    var description: String {
        return "<\(String(describing: type))" +
            (contentsInfo.hasOwnBeforeEach ? ", hasOwnBeforeEach" : "") +
            (contentsInfo.hasOwnAfterEach ? ", hasOwnAfterEach" : "") +
            (contentsInfo.variableDefinitions
                .isEmpty ? "" : " variableDefinitions: \(contentsInfo.variableDefinitions)") +
            ">"
    }

    func ownHookSourceType(ofType hookType: HookType) -> HookSource.`Type`? {
        switch hookType {
        case .beforeEach:
            if contentsInfo.hasOwnBeforeEach { return .quickSpecMethodCall }
        case .afterEach:
            if contentsInfo.hasOwnAfterEach { return .quickSpecMethodCall }
        }

        if case .reusableTests = type {
            return .contextArg
        }

        return nil
    }
}
