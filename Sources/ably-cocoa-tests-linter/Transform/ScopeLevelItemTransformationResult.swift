import SwiftSyntax

extension ASTTransform {
    struct ScopeLevelItemTransformationResult {
        enum Item {
            struct ReplacementItem {
                var item: AST.ScopeLevel.Item
                var canLiftToHigherScope: Bool
                var classLevelFallback: AST.ClassDeclaration.Item?
            }

            case replacementItem(ReplacementItem)
            case globalDeclaration(DeclSyntax)
            case classDeclarationItem(AST.ClassDeclaration.Item)
        }

        var items: [Item]

        static let empty = Self(
            items: []
        )

        func appending(_ other: ScopeLevelItemTransformationResult)
            -> ScopeLevelItemTransformationResult
        {
            var result = self
            result.items += other.items
            return result
        }

        var replacementContents: [Item.ReplacementItem] {
            return items.compactMap { item in
                if case let .replacementItem(replacementItem) = item {
                    return replacementItem
                } else {
                    return nil
                }
            }
        }

        var globalDeclarations: [DeclSyntax] {
            return items.compactMap { item in
                if case let .globalDeclaration(decl) = item {
                    return decl
                } else {
                    return nil
                }
            }
        }

        var classDeclarationItems: [AST.ClassDeclaration.Item] {
            return items.compactMap { item in
                if case let .classDeclarationItem(classDeclItem) = item {
                    return classDeclItem
                } else {
                    return nil
                }
            }
        }

        var testFunctionDeclarations: [FunctionDeclSyntax] {
            // TODO: this is a bit hacky – we're essentially figuring out which
            // test methods were created by transformSpecFunctionDeclarationIntoClassLevelDeclarations
            // but we could probably just improve things to make it tell us that
            return replacementContents
                .compactMap { item -> FunctionDeclSyntax? in
                    guard case let .functionDeclaration(functionDeclaration) = item.item else {
                        return nil
                    }
                    if !functionDeclaration.identifier.text.starts(with: "test") {
                        // TODO: should we handle skipped functions in some nicer way?
                        return nil
                    }
                    return functionDeclaration
                }
        }
    }
}

extension ASTTransform.ScopeLevelItemTransformationResult {
    init(classDeclarationItem: AST.ClassDeclaration.Item) {
        self.init(
            items: [.classDeclarationItem(classDeclarationItem)]
        )
    }

    init(
        replacementItem: AST.ScopeLevel.Item,
        canLiftToHigherScope: Bool = false,
        classLevelFallback: AST.ClassDeclaration.Item? = nil
    ) {
        self.init(
            items: [.replacementItem(.init(item: replacementItem,
                                           canLiftToHigherScope: canLiftToHigherScope,
                                           classLevelFallback: classLevelFallback))]
        )
    }

    init<T: DeclSyntaxProtocol>(classLevelDeclaration: T) {
        self.init(classDeclarationItem: .init(decl: classLevelDeclaration))
    }

    init<T: DeclSyntaxProtocol>(globalDeclaration: T) {
        self.init(
            items: [.globalDeclaration(DeclSyntax(globalDeclaration))]
        )
    }
}

extension AST.ClassDeclaration.Item {
    init<T: DeclSyntaxProtocol>(decl: T) {
        let member = MemberDeclListItemSyntax { builder in
            let syntax = DeclSyntax(decl)
            builder.useDecl(syntax)
        }
        self = .member(member)
    }
}
