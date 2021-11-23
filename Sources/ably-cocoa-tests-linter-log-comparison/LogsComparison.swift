import Foundation

struct LogsComparison {
    var quickTestsXCResultFilePath: String
    var migratedTestsXCResultFilePath: String

    func runComparison() throws {
        let quickTestsLogs = try loadTestLog(forXCResultFilePath: quickTestsXCResultFilePath)
            .filteredAndNormalised
        let migratedTestsLogs = try loadTestLog(forXCResultFilePath: migratedTestsXCResultFilePath)
            .filteredAndNormalised

        let quickTestsLogsOutputPath = createTemporaryFilePath(prefix: "quick-")
        try quickTestsLogs.write(to: URL(fileURLWithPath: quickTestsLogsOutputPath))
        print("Filtered Quick tests output written to \(quickTestsLogsOutputPath)")
        let migratedTestsLogsOutputPath = createTemporaryFilePath(prefix: "migrated-")
        try migratedTestsLogs.write(to: URL(fileURLWithPath: migratedTestsLogsOutputPath))
        print("Filtered migrated tests output written to \(quickTestsLogsOutputPath)")

        if quickTestsLogs.lines != migratedTestsLogs.lines {
            print("Quick tests and migrated tests output don’t match. Opening diff.")
            try runOpendiff(
                quickTestsLogsOutputPath: quickTestsLogsOutputPath,
                migratedTestsLogsOutputPath: migratedTestsLogsOutputPath
            )
        } else {
            print("Quick tests and migrated tests output match.")
        }
    }

    private func runOpendiff(
        quickTestsLogsOutputPath: String,
        migratedTestsLogsOutputPath: String
    ) throws {
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/opendiff")
        process.arguments = [quickTestsLogsOutputPath, migratedTestsLogsOutputPath]
        try process.run()
        process.waitUntilExit()
    }

    struct TestLog {
        var lines: [String]

        private init(lines: [String]) {
            self.lines = lines
        }

        init(logFileContents: String) {
            lines = logFileContents.components(separatedBy: .newlines)
        }

        func write(to fileURL: URL) throws {
            let data = lines.joined(separator: "\n").data(using: .utf8)!
            try data.write(to: fileURL)
        }

        var filteredAndNormalised: TestLog {
            var newLines = lines.compactMap { line -> String? in
                // Some lines are preceded by some extraneous metadata e.g.
                // ]K6:result[S6:StringK2:_vV9:succeeded]K9:startTime[S4:DateK2:_vV28:2021-11-23T12:49:25.477+0000]K9:suiteName[S6:StringK2:_vV9:Utilities]K7:summary[S6:StringK2:_vV502:START HOOK: Utilities.beforeEach__Utilities__JSON_Encoder
                // so we want to kill that.

                // TODO: double check this normalising is working
                let patterns = [
                    "^.*(Test Case .* started.)$",
                    "^.*(START HOOK: .*)",
                    "^.*(END HOOK: .*)",
                ]

                let captures = patterns.compactMap { pattern -> String? in
                    let regex = try! NSRegularExpression(pattern: pattern, options: [])
                    let firstMatch = regex.firstMatch(
                        in: line,
                        options: [],
                        range: NSRange(location: 0, length: line.count)
                    )
                    guard let firstMatch = firstMatch else {
                        return nil
                    }
                    return (line as NSString).substring(with: firstMatch.range(at: 1))
                }

                return captures.first
            }

            newLines = newLines.map { line in
                // tests in Quick don't have the "test__" prefix
                line.replacingOccurrences(of: "test__", with: "")
            }

            return .init(lines: newLines)
        }
    }

    private func loadTestLog(forXCResultFilePath xcResultFilePath: String) throws -> TestLog {
        // Taken from https://stackoverflow.com/questions/58176544/how-do-i-collect-the-stdout-and-std-error-from-the-xcresult-bundle-generated-by

        // "find the id that points to the location of the encoded file in the .xcresult bundle"
        let xcResultId = try getXCResultID(forXCResultFilePath: xcResultFilePath)

        return try loadLogs(forXCResultFilePath: xcResultFilePath, id: xcResultId)
    }

    private func getXCResultID /* name deliberately vague because I have no idea what this ID is */ (
        forXCResultFilePath xcResultFilePath: String
    ) throws
        -> String
    {
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/xcrun")

        let pipe = Pipe()
        process.standardOutput = pipe

        process.arguments = ["xcresulttool", "get", "--format", "json", "--path", xcResultFilePath]

        try process.run()

        let data = try pipe.fileHandleForReading.readToEnd()

        guard let data = data else {
            fatalError("No data returned from process’s stdout")
        }

        process.waitUntilExit()

        guard process.terminationStatus == 0 else {
            fatalError("getting ID failed")
        }

        let json = try JSONSerialization.jsonObject(with: data, options: []) as! [String: Any]

        // get .actions._values[]
        let actionsValues = (json["actions"] as? [String: Any])?["_values"] as! [[String: Any]]
        let ids = actionsValues
            .compactMap {
                (
                    (($0["actionResult"] as? [String: Any])?["logRef"] as? [String: Any])?["id"] as? [String: Any]
                )?["_value"] as? String
            }
        if ids.count != 1 {
            fatalError("Got unexpected number \(ids.count) of IDs, expected 1")
        }

        return ids.first!
    }

    private func createTemporaryFilePath(prefix: String) -> String {
        return NSTemporaryDirectory().appending("\(prefix)\(UUID().uuidString)")
    }

    private func loadLogs(
        forXCResultFilePath xcResultFilePath: String,
        id: String
    ) throws
        -> TestLog
    {
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/xcrun")

        let pipe = Pipe()
        process.standardOutput = pipe

        let tempFilePath = createTemporaryFilePath(prefix: "logs-")

        process.arguments = [
            "xcresulttool",
            "export",
            "--path",
            xcResultFilePath,
            "--id",
            id,
            "--output-path",
            tempFilePath,
            "--type",
            "file",
        ]

        try process.run()
        process.waitUntilExit()

        guard process.terminationStatus == 0 else {
            fatalError("getting logs failed")
        }

        let data = try Data(contentsOf: URL(fileURLWithPath: tempFilePath))
        return TestLog(logFileContents: String(data: data, encoding: .utf8)!)
    }
}
