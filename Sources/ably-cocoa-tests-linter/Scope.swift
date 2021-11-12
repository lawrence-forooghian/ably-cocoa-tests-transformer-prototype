typealias Scope = [ScopeMember]

// TODO: I think we can make a Scope type so that we don't have to worry about things like array length (i.e. this invariant that scope is always non-empty)
extension Array where Element == ScopeMember /* i.e. Scope */ {
    private var parent: Self? {
        guard count >= 2 else { return nil }
        var new = self
        new.removeLast()
        return new
    }

    func nearestAncestorHavingOwnBeforeEach(includeSelf: Bool) -> Self? {
        if includeSelf, last!.contentsInfo.hasOwnBeforeEach {
            return self
        }
        return parent?.nearestAncestorHavingOwnBeforeEach(includeSelf: true)
    }

    func nearestAncestorHavingOwnAfterEach(includeSelf: Bool) -> Self? {
        if includeSelf, last!.contentsInfo.hasOwnAfterEach {
            return self
        }
        return parent?.nearestAncestorHavingOwnAfterEach(includeSelf: true)
    }

    var isSkipped: Bool {
        if case .describeOrContext(description: _, skipped: true) = last!.type {
            return true
        }
        return parent?.isSkipped ?? false
    }
}
