//
//  ASTTests.swift
//  TypologyTests
//
//  Created by Max Desiatov on 01/06/2019.
//

import XCTest

@testable import TypologyCore

let root = URL(fileURLWithPath: #file)
  .deletingLastPathComponent()
  .deletingLastPathComponent()
  .deletingLastPathComponent()
  .appendingPathComponent("ValidationTests/AST/")

final class ASTTests: XCTestCase {
  func testTernary() throws {
    let string =
      try #"true ? "then" : "else""#.parseExpr()
    let int =
      try "false ? 0 : 42".parseExpr()
    let error =
      try #"true ? "then" : 42"#.parseExpr()

    XCTAssertEqual(try string.infer(), .string)
    XCTAssertEqual(try int.infer(), .int)
    XCTAssertThrowsError(try error.infer())
  }

  func testBinary() throws {
    let binary1 = try #"1 + 2 * 3"#.parseExpr()
    let binary2 = try #"1 * 2 + 3"#.parseExpr()

    XCTAssertEqualIgnoringRange(
      binary1, Expr.binary(.literal(1), "+", .binary(.literal(2), "*", .literal(3))))
    XCTAssertEqualIgnoringRange(
      binary2, Expr.binary(.binary(.literal(1), "*", .literal(2)), "+", .literal(3)))
  }

  func testInterpolatedStringExpr() throws {
    let string = try #""\(foo, 1) + \(bar)""#.parseExpr()

    XCTAssertEqualIgnoringRange(
      string,
      Expr.interpolatedString(
        "", [(.tuple([.identifier("foo"), .literal(1)]), " + "), (.identifier("bar"), "")]))
  }

  func testFunc() throws {
    let f =
      try "func x(_ x: String, y: [Int]) -> Int { return 42 }"
      .parseStatement() as? FunctionDecl

    XCTAssertEqual(
      f?.scheme,
      Scheme(
        [.string, .array(of: .int)] --> .int
      ))
  }

  func testGenericFunc() throws {
    let f =
      try "func x<T>(_ x: T, _ y: T) -> T { return x }"
      .parseStatement() as? FunctionDecl

    let tVar = "T"
    let t = Type.constructor(TypeIdentifier(value: tVar), [])

    XCTAssertEqual(
      f?.scheme,
      Scheme(
        [t, t] --> t,
        variables: [TypeVariable(value: tVar)]
      ))
  }

  func testFuncPosition() throws {
    let functions = try """
          // declare function #commentsForComments
          //This is also a comment
          //    but is written over multiple lines.
          func first(_ x: String) -> String {
              var x: String {
                return "Hello"
              }
              var y: String {
                get {
                  return "Hello, "
                }
                set {
                  print("world!")
                }
              }
              dynamic private(set) let a: Double = 3.14, b: Int
              let z = 5
              let (x, y) = z

              return x
          }

          /* This is also a comment
              but is written over multiple lines. */
          // declare another function with double offset #commentsForComments
              func second(_ x: String) -> String {
                  return x
              }
      """.parseAST()

    let firstFunc = try XCTUnwrap(functions.statements.first)
    let secondFunc = try XCTUnwrap(functions.statements.last)

    XCTAssertEqual(firstFunc.range.start.line, 4)
    XCTAssertEqual(firstFunc.range.start.column, 5)
    XCTAssertEqual(firstFunc.range.end.line, 21)
    XCTAssertEqual(firstFunc.range.end.column, 6)

    XCTAssertEqual(secondFunc.range.start.line, 26)
    XCTAssertEqual(secondFunc.range.start.column, 9)
    XCTAssertEqual(secondFunc.range.end.line, 28)
    XCTAssertEqual(secondFunc.range.end.column, 10)
  }

  func testInitFromFilePositive() throws {
    let url = root.appendingPathComponent("Positive.swift")
    XCTAssertNoThrow(try File(path: url.path))
  }

  func testInitFromFileNegative() throws {
    let url = root.appendingPathComponent("Negative.swift")
    XCTAssertThrowsError(try File(path: url.path))
  }
}

func XCTAssertEqualIgnoringRange<T>(
  _ expression1: @autoclosure () throws -> T,
  _ expression2: @autoclosure () throws -> T,
  _ message: @autoclosure () -> String = "",
  file: StaticString = #filePath,
  line: UInt = #line
) rethrows where T: Equatable, T: EquatableIgnoringSourceRange {
  let v1 = try expression1()
  let v2 = try expression2()

  if v1.eq(v2, ignoreRanges: true) {
    return
  }

  XCTAssertEqual(v1, v2, message(), file: file, line: line)
  XCTFail("if v1 != v2 (ignoring ranges), then v1 should not equal v2 when considering ranges")
}

extension String {
  fileprivate func parseAST() throws -> File {
    try .init(contents: self)
  }

  fileprivate func parseStatement() throws -> Statement {
    let ast = try parseAST()

    XCTAssertEqual(ast.statements.count, 1)

    return ast.statements[0]
  }

  fileprivate func parseExpr() throws -> Expr {
    let expr = try parseStatement() as? ExprNode

    return try XCTUnwrap(expr).expr
  }
}
