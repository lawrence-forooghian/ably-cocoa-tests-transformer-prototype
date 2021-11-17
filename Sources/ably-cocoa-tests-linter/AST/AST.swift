import SwiftSyntax

struct ClassContents {
    enum Item {
        case member(_: MemberDeclListItemSyntax)
        case scope(_: ASTScope)
    }

    var contents: [Item]
}

struct ASTScope {
    enum `Type`: CustomStringConvertible {
        case spec(_: FunctionDeclSyntax)
        case reusableTests(_: FunctionDeclSyntax, functionName: String)
        case describeOrContext(_: FunctionCallExprSyntax, description: String, skipped: Bool)

        var description: String {
            switch self {
            case .spec: return "spec()"
            case let .reusableTests(functionName: functionName): return "reusableTests(\(functionName)"
            case let .describeOrContext(description: description): return "describeOrContext(\(description))"
            }
        }
    }

    var type: `Type`

    var contents: [Item]

    enum Item {
        case variableDeclaration(_: VariableDeclSyntax)
        case functionDeclaration(_: FunctionDeclSyntax)
        case structDeclaration(_: StructDeclSyntax)
        case scope(_: ASTScope)
        case it(_: FunctionCallExprSyntax, description: String, skipped: Bool)
        case reusableTestsCall(_: FunctionCallExprSyntax, calledFunctionName: String)
        case hook(_: FunctionCallExprSyntax, type: HookType)
    }
}
