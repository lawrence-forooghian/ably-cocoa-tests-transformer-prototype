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

    func findReusableTestsDeclaration(forCall reusableTestsCall: AST.ScopeLevel.Item
        .ReusableTestsCall) -> AST.ScopeLevel.ReusableTestsDeclaration?
    {
        let found = contents.compactMap { item -> AST.ScopeLevel.ReusableTestsDeclaration? in
            switch item {
            case let .reusableTestsDeclaration(reusableTestsDeclaration)
                where reusableTestsDeclaration.functionName == reusableTestsCall
                .calledFunctionName: return reusableTestsDeclaration
            default: return nil
            }
        }
        if found.count > 1 {
            fatalError("Found multiple reusable tests declarations for call, unexpected")
        }
        return found.first
    }
}
