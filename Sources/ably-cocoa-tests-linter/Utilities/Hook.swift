struct HookSource {
    enum `Type` {
        case quickSpecMethodCall
        case contextArg
    }

    var scope: AST.Scope
    var type: `Type`
    var hookType: HookType

    var outputFunctionName: String {
        switch type {
        case .quickSpecMethodCall:
            switch hookType {
            case .beforeEach:
                return QuickSpecMethodCall.hook(.beforeEach).outputFunctionName(inScope: scope)
            case .afterEach:
                return QuickSpecMethodCall.hook(.afterEach).outputFunctionName(inScope: scope)
            }
        case .contextArg:
            switch hookType {
            case .beforeEach:
                return "context.beforeEach?"
            case .afterEach:
                return "context.afterEach?"
            }
        }
    }
}

enum HookType {
    case beforeEach
    case afterEach
}
