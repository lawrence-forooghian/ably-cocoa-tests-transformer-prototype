import ArgumentParser
import Foundation
import SwiftSyntax

// TODO: what about stuff like comments – do they come through?
// https://forums.swift.org/t/se-0275-allow-more-characters-like-whitespaces-and-punctuations-for-escaped-identifiers/32538/50 - what should we use as the new method names?

// Guiding principles:
// - I don't understand the test code well, and I want people to just need to review the structural changes without thinking about any behavioural ones. there seems to be a lot of state potentially shared between test cases (probably not fully intentionally, and probably not desirably) and I don't want to change that behaviour. So must behave exactly as before

// TODO: tidy up this code and make it bloggable / talkable / open sourceable if we so desire
@main
struct RunTransformation: ParsableCommand {
    @Flag(help: "Extract local variables to global variables.")
    var rewriteLocalsToGlobals = false

    @Flag(inversion: .prefixedNo, help: "Rewrite test code. Implies rewriteLocalsToGlobals.")
    var rewriteTestCode = true

    @Flag(
        help: "Add logging to lifecycle events. For debugging, to compare Quick tests with migrated tests."
    )
    var addLogging = false

    @Argument(help: "The directory containing the Swift source files to transform.")
    var directory: String

    private var options: TransformQuickSpec.Options {
        return TransformQuickSpec.Options(
            rewriteLocalsToGlobals: rewriteTestCode || rewriteLocalsToGlobals,
            rewriteTestCode: rewriteTestCode,
            addLogging: addLogging
        )
    }

    mutating func run() throws {
        let directoryContents = try FileManager.default.contentsOfDirectory(atPath: directory)
        let swiftFiles = directoryContents.filter { $0.hasSuffix(".swift") }

        try swiftFiles.forEach { fileName in
            print("Processing file \(fileName)")
            let url = URL(fileURLWithPath: directory).appendingPathComponent(fileName)

            let sourceFile = try SyntaxParser.parse(url)
            let transformed = TransformQuickSpec(options: options).visit(sourceFile)

            try String(describing: transformed).write(to: url, atomically: true, encoding: .utf8)
        }
    }
}
