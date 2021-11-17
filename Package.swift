// swift-tools-version:5.5
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "ably-cocoa-tests-linter",
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        .package(
            name: "SwiftSyntax",
            url: "https://github.com/apple/swift-syntax.git",
            .exact("0.50500.0")
        ),
        .package(url: "https://github.com/apple/swift-argument-parser", from: "1.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .executableTarget(
            name: "ably-cocoa-tests-linter",
            dependencies: [
                "SwiftSyntax",
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
            ]
        ),
        .testTarget(
            name: "ably-cocoa-tests-linterTests",
            dependencies: ["ably-cocoa-tests-linter"]
        ),
    ]
)
