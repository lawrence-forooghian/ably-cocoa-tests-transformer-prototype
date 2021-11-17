extension AST {
    struct Scope {
        // Invariant: non-empty
        private(set) var levels: [ScopeLevel]

        init(topLevel: ScopeLevel) {
            levels = [topLevel]
        }

        private init(levels: [ScopeLevel]) {
            self.levels = levels
        }

        func appending(_ level: ScopeLevel) -> Scope {
            return Scope(levels: levels + [level])
        }

        var parent: Self? {
            guard levels.count >= 2 else { return nil }
            var newLevels = levels
            newLevels.removeLast()
            return .init(levels: newLevels)
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
    }
}
