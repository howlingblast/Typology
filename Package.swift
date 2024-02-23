// swift-tools-version:5.7

import PackageDescription

let package = Package(
  name: "Typology",
  platforms: [
    .macOS(.v10_15)
  ],
  products: [
    .library(
      name: "TypologyCore",
      targets: ["TypologyCore"]
    ),
    .executable(name: "typology", targets: ["TypologyCLI"]),
  ],
  dependencies: [
    .package(url: "https://github.com/apple/swift-syntax", branch: "main"),
    .package(url: "https://github.com/jakeheis/SwiftCLI", from: "5.0.0"),
    .package(url: "https://github.com/onevcat/Rainbow", from: "3.0.0"),
  ],
  targets: [
    // Targets are the basic building blocks of a package. A target can define a module or a test suite.
    // Targets can depend on other targets in this package, and on products in packages which this package depends on.
    .target(
      name: "TypologyCore",
      dependencies: [
        .product(name: "SwiftOperators", package: "swift-syntax"),
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftSyntaxParser", package: "swift-syntax"),
        "Rainbow",
        "SwiftCLI",
      ]
    ),
    .executableTarget(
      name: "TypologyCLI",
      dependencies: [
        "SwiftCLI",
        "TypologyCore",
      ]
    ),
    .testTarget(
      name: "TypologyTests",
      dependencies: [
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        "TypologyCore",
      ]
    ),
  ]
)
