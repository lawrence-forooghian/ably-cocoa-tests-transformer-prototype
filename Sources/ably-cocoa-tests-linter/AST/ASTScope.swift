extension AST {
    struct Scope {
        var className: String
        // Invariant: non-empty
        private(set) var levels: [ScopeLevel]

        init(className: String, topLevel: ScopeLevel) {
            self.className = className
            levels = [topLevel]
        }

        private init(className: String, levels: [ScopeLevel]) {
            self.className = className
            self.levels = levels
        }

        func appending(_ level: ScopeLevel) -> Scope {
            return Scope(className: className, levels: levels + [level])
        }

        var parent: Self? {
            guard levels.count >= 2 else { return nil }
            var newLevels = levels
            newLevels.removeLast()
            return .init(className: className, levels: newLevels)
        }

        private var peek: ScopeLevel {
            return levels.last!
        }

        func hookSourceOfNearestAncestorHavingOwnHookSource(ofType hookType: HookType,
                                                            includeSelf: Bool) -> HookSource?
        {
            if includeSelf, let hookSourceType = peek.ownHookSourceType(ofType: hookType) {
                return HookSource(scope: self, type: hookSourceType, hookType: hookType)
            }
            return parent?.hookSourceOfNearestAncestorHavingOwnHookSource(
                ofType: hookType,
                includeSelf: true
            )
        }

        var isSkipped: Bool {
            if case let .describeOrContext(describeOrContext) = peek, describeOrContext.skipped {
                return true
            }
            return parent?.isSkipped ?? false
        }

        var isReusableTests: Bool {
            levels.contains { level in
                if case .reusableTestsDeclaration = level {
                    return true
                } else {
                    return false
                }
            }
        }

        func findReusableTestsDeclaration(forCall reusableTestsCall: AST.ScopeLevel.Item
            .ReusableTestsCall) -> AST.ScopeLevel.ReusableTestsDeclaration?
        {
            if let foundAtTop = peek.findReusableTestsDeclaration(forCall: reusableTestsCall) {
                return foundAtTop
            }

            return parent?.findReusableTestsDeclaration(forCall: reusableTestsCall)
        }
    }
}
