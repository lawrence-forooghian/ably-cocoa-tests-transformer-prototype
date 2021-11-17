import SwiftSyntax

// Each AST item contains an abstract representation of the item, as well
// as the syntax used to define it.

enum AST {
    struct ClassDeclaration {
        enum Item {
            case member(MemberDeclListItemSyntax)
            case spec(ScopeLevel.Spec)

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

    enum ScopeLevel {
        struct Spec {
            var syntax: MemberDeclListItemSyntax
            var contents: [Item]
        }

        struct ReusableTestsDeclaration {
            var syntax: FunctionDeclSyntax
            var functionName: String
            var contents: [Item]
        }

        struct DescribeOrContext {
            var syntax: FunctionCallExprSyntax
            var description: String
            var skipped: Bool
            var contents: [Item]
        }

        enum Item {
            case variableDeclaration(VariableDeclSyntax)
            case functionDeclaration(FunctionDeclSyntax)
            case structDeclaration(StructDeclSyntax)
            case reusableTestsDeclaration(ReusableTestsDeclaration)
            case describeOrContext(DescribeOrContext)
            case it(FunctionCallExprSyntax, description: String, skipped: Bool)
            case reusableTestsCall(FunctionCallExprSyntax, calledFunctionName: String)
            case hook(FunctionCallExprSyntax, type: HookType)
        }

        case spec(Spec)
        case reusableTestsDeclaration(ReusableTestsDeclaration)
        case describeOrContext(DescribeOrContext)
    }
}
