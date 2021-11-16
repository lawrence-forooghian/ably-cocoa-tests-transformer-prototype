typealias Scope = [ScopeMember]

// TODO: I think we can make a Scope type so that we don't have to worry about things like array length (i.e. this invariant that scope is always non-empty)
extension Array where Element == ScopeMember /* i.e. Scope */ {
    private var parent: Self? {
        guard count >= 2 else { return nil }
        var new = self
        new.removeLast()
        return new
    }

    func hookSourceOfNearestAncestorHavingOwnHookSource(ofType hookType: HookType,
                                                        includeSelf: Bool) -> HookSource?
    {
        if includeSelf, let hookSourceType = last!.ownHookSourceType(ofType: hookType) {
            return HookSource(scope: self, type: hookSourceType, hookType: hookType)
        }
        return parent?.hookSourceOfNearestAncestorHavingOwnHookSource(
            ofType: hookType,
            includeSelf: true
        )
    }

    var isSkipped: Bool {
        if case .describeOrContext(description: _, skipped: true) = last!.type {
            return true
        }
        return parent?.isSkipped ?? false
    }

    var isReusableTests: Bool {
        contains { member in
            if case .reusableTests = member.type {
                return true
            } else {
                return false
            }
        }
    }
}
