import ArgumentParser
import Foundation

@main
struct RunComparison: ParsableCommand {
    // TODO: is there a way to make variable name different from flag? so I can have (non-weird capitalisation and the word "path") in the variable name
    @Option(help: "The path to the .xcresult file emitted by the Quick tests.")
    var quickTestsXcresultFile: String

    @Option(help: "The path to the .xcresult file emitted by the migrated tests.")
    var migratedTestsXcresultFile: String

    mutating func run() throws {
        try LogsComparison(
            quickTestsXCResultFilePath: quickTestsXcresultFile,
            migratedTestsXCResultFilePath: migratedTestsXcresultFile
        ).runComparison()
    }
}
