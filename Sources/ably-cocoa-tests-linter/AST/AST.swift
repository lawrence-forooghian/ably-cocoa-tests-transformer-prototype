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
            // This is de-normalised for easy editing
            var functionDeclaration: FunctionDeclSyntax
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
            struct It {
                var syntax: FunctionCallExprSyntax
                var description: String
                var skipped: Bool
            }

            struct ReusableTestsCall {
                var syntax: FunctionCallExprSyntax
                var calledFunctionName: String
            }

            struct Hook {
                var syntax: FunctionCallExprSyntax
                var hookType: HookType
            }

            case variableDeclaration(VariableDeclSyntax)
            case functionDeclaration(FunctionDeclSyntax)
            case structDeclaration(StructDeclSyntax)
            case reusableTestsDeclaration(ReusableTestsDeclaration)
            case describeOrContext(DescribeOrContext)
            case it(It)
            case reusableTestsCall(ReusableTestsCall)
            case hook(Hook)

            var syntax: Syntax {
                switch self {
                case let .variableDeclaration(variableDecl):
                    return Syntax(variableDecl)
                case let .functionDeclaration(functionDecl):
                    return Syntax(functionDecl)
                case let .structDeclaration(structDecl):
                    return Syntax(structDecl)
                case let .reusableTestsDeclaration(reusableTestsDeclaration):
                    return Syntax(reusableTestsDeclaration.syntax)
                case let .describeOrContext(describeOrContext):
                    return Syntax(describeOrContext.syntax)
                case let .it(it):
                    return Syntax(it.syntax)
                case let .reusableTestsCall(reusableTestsCall):
                    return Syntax(reusableTestsCall.syntax)
                case let .hook(hook):
                    return Syntax(hook.syntax)
                }
            }
        }

        case spec(Spec)
        case reusableTestsDeclaration(ReusableTestsDeclaration)
        case describeOrContext(DescribeOrContext)
    }
}
