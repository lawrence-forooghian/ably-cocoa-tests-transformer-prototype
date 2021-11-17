import SwiftSyntax

// Each AST item contains an abstract representation of the item, as well
// as the syntax used to define it.

enum AST {
    // TODO: is there still a reason to have a Scope type tying Spec/ReusableTests/DescribeOrContext together?

    struct ClassDeclaration {
        struct Spec {
            var syntax: MemberDeclListItemSyntax
            var contents: [Scope.Item]
        }

        enum Item {
            case member(MemberDeclListItemSyntax)
            case spec(Spec)

            var syntax: MemberDeclListItemSyntax {
                switch self {
                case let .member(syntax): return syntax
                case let .spec(spec): return spec.syntax
                }
            }
        }

        var syntax: ClassDeclSyntax
        var items: [Item]
    }

    enum Scope {
        struct ReusableTests {
            var syntax: FunctionDeclSyntax
            var functionName: String
            var contents: [Scope.Item]
        }

        struct DescribeOrContext {
            var syntax: FunctionCallExprSyntax
            var description: String
            var skipped: Bool
            var contents: [Scope.Item]
        }

        enum Item {
            case variableDeclaration(VariableDeclSyntax)
            case functionDeclaration(FunctionDeclSyntax)
            case structDeclaration(StructDeclSyntax)
            case reusableTests(ReusableTests)
            case describeOrContext(DescribeOrContext)
            case it(FunctionCallExprSyntax, description: String, skipped: Bool)
            case reusableTestsCall(FunctionCallExprSyntax, calledFunctionName: String)
            case hook(FunctionCallExprSyntax, type: HookType)
        }
    }
}
