extension AST.ScopeLevel {
    var methodNameComponent: String? {
        switch self {
        case .spec: return nil
        case .reusableTestsDeclaration: return nil
        case let .describeOrContext(describeOrContext): return describeOrContext.description
        }
    }

    func ownHookSourceType(ofType hookType: HookType) -> HookSource.`Type`? {
        switch hookType {
        case .beforeEach:
            if hasOwnHook(ofType: .beforeEach) { return .quickSpecMethodCall }
        case .afterEach:
            if hasOwnHook(ofType: .afterEach) { return .quickSpecMethodCall }
        }

        if case .reusableTestsDeclaration = self {
            return .contextArg
        }

        return nil
    }

    var contents: [Item] {
        switch self {
        case let .describeOrContext(val): return val.contents
        case let .spec(val): return val.contents
        case let .reusableTestsDeclaration(val): return val.contents
        }
    }

    func hasOwnHook(ofType hookType: HookType) -> Bool {
        return contents.contains { item in
            if case let .hook(hook) = item, hook.hookType == hookType {
                return true
            }
            return false
        }
    }
}
