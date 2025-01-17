//
//  InferenceTests.swift
//  Typology
//
//  Created by Max Desiatov on 16/04/2019.
//  Copyright © 2019 Typology. All rights reserved.
//

import XCTest

@testable import TypologyCore

final class InferenceTests: XCTestCase {
  func testTernary() throws {
    let string = Expr.ternary(
      .literal(true),
      .literal("then"),
      .literal("else")
    )
    let int = Expr.ternary(
      .literal(.bool(false)),
      .literal(0),
      .literal(42)
    )
    let error = Expr.ternary(
      .literal(true),
      .literal("then"),
      .literal(42)
    )

    XCTAssertEqual(try string.infer(), .string)
    XCTAssertEqual(try int.infer(), .int)
    XCTAssertThrowsError(try error.infer())
  }

  func testApplication() throws {
    let increment = Expr.application("increment", [.literal(0)])
    let stringify = Expr.application("stringify", [.literal(0)])
    let error = Expr.application("increment", [.literal(false)])

    let e: Environment = [
      "increment": [.init(.int --> .int)],
      "stringify": [.init(.int --> .string)],
    ]

    XCTAssertEqual(try increment.infer(environment: e), .int)
    XCTAssertEqual(try stringify.infer(environment: e), .string)
    XCTAssertThrowsError(try error.infer())
  }

  func testLambda() throws {
    let lambda = Expr.lambda(
      ["x"],
      .application(
        "decode",
        [
          .application(
            "stringify",
            [.application("increment", ["x"])]
          )
        ]
      )
    )

    let error = Expr.lambda(
      ["x"],
      .application(
        "stringify",
        [
          .application(
            "decode",
            [.application("increment", ["x"])]
          )
        ]
      )
    )

    let e: Environment = [
      "increment": [.init(.int --> .int)],
      "stringify": [.init(.int --> .string)],
      "decode": [.init(.string --> .int)],
    ]

    XCTAssertEqual(try lambda.infer(environment: e), .int --> .int)
    XCTAssertThrowsError(try error.infer())
  }

  func testLambdaWithMultipleArguments() throws {
    let lambda = Expr.lambda(
      ["x", "y"],
      .application(
        "decode",
        [
          .application(
            "stringify",
            [
              .application("sum", ["x", "y"]),
              .application("sum", ["x", "y"]),
            ]
          ),
          .application(
            "stringify",
            [
              .application("sum", ["x", "y"]),
              .application("sum", ["x", "y"]),
            ]
          ),
        ]
      )
    )

    let e: Environment = [
      "sum": [.init([.int, .int] --> .int)],
      "stringify": [.init([.int, .int] --> .string)],
      "decode": [.init([.string, .string] --> .int)],
    ]

    XCTAssertEqual(try lambda.infer(environment: e), [.int, .int] --> .int)
  }

  func testLambdaWithMultipleArgumentsDiffrentTypes() throws {
    let lambda = Expr.lambda(
      ["str", "int"],
      .application(
        "decode",
        [
          .application("concatenate", ["int", "str"]),
          .application("sum", ["int", "int"]),
        ]
      )
    )

    let e: Environment = [
      "concatenate": [.init([.int, .string] --> .string)],
      "sum": [.init([.int, .int] --> .int)],
      "decode": [.init([.string, .int] --> .int)],
    ]

    XCTAssertEqual(
      try lambda.infer(environment: e),
      [.string, .int] --> .int
    )
  }

  func testLambdaApplication() throws {
    let lambda = Expr.application(
      .lambda(["x"], .ternary("x", .literal(1), .literal(0))), [.literal(true)]
    )

    let error = Expr.application(
      .lambda(["x"], .ternary("x", .literal(1), .literal(0))), [.literal("blah")]
    )

    XCTAssertEqual(try lambda.infer(), .int)
    XCTAssertThrowsError(try error.infer())
  }

  func testMember() throws {
    let appending = Expr.application(
      .member(.literal("Hello, "), "appending"),
      [.literal(" World")]
    )
    let count = Expr.member(.literal("Test"), "count")

    let m: Members = [
      "String": [
        "appending": [.init(.string --> .string)],
        "count": [.init(.int)],
      ]
    ]

    XCTAssertEqual(try appending.infer(members: m), .string)
    XCTAssertEqual(try count.infer(members: m), .int)
  }

  func testMemberOfMember() throws {
    let literal = ExprNode.literal("Test")
    let magnitude = Expr.member(.member(literal, "count"), "magnitude")
    let error = Expr.member(.member(literal, "magnitude"), "count")

    let m: Members = [
      "String": [
        "count": [.init(.int)]
      ],
      "Int": [
        "magnitude": [.init(.int)]
      ],
    ]

    XCTAssertEqual(try magnitude.infer(members: m), .int)
    XCTAssertThrowsError(try error.infer(members: m))
  }

  func testLambdaMember() throws {
    let lambda = ExprNode.application(
      .lambda(["x"], .ternary("x", .literal("one"), .literal("zero"))),
      [.literal(true)]
    )
    let count = Expr.member(lambda, "count")
    let error = Expr.member(lambda, "magnitude")

    let m: Members = [
      "String": [
        "count": [.init(.int)]
      ],
      "Int": [
        "magnitude": [.init(.int)]
      ],
    ]

    XCTAssertEqual(try count.infer(members: m), .int)
    XCTAssertThrowsError(try error.infer(members: m))
  }

  func testTupleMember() throws {
    let tuple = ExprNode.tuple([.literal(42), .literal("forty two")])

    XCTAssertEqual(try Expr.member(tuple, "0").infer(), .int)
    XCTAssertEqual(try Expr.member(tuple, "1").infer(), .string)
    XCTAssertThrowsError(try Expr.member(tuple, "2").infer())
  }

  func testNamedTupleMember() throws {
    let namedTuple = ExprNode.namedTuple([
      ("text", .literal("some text")),
      ("count", .literal(10)),
    ])
    let tuple = ExprNode.tuple([.literal("some text"), .literal(10)])
    let mixedTuple = ExprNode.namedTuple([
      ("text", .literal("some text")),
      (nil, .literal(10)),
    ])
    let mixedTuple2 = ExprNode.namedTuple([
      (nil, .literal("some text")),
      ("count", .literal(10)),
    ])
    let fewArguments = ExprNode.tuple([.literal("some text")])
    let wrongOrder = ExprNode.tuple([.literal(10), .literal("some text")])

    let e: Environment = [
      "acceptNamedTuple": [
        .init(
          .namedTuple([
            ("text", .string),
            ("count", .int),
          ]) --> .string)
      ],
      "acceptTuple": [
        .init(
          .namedTuple([
            (nil, .string),
            (nil, .int),
          ]) --> .string)
      ],
      "acceptMixedTuple": [
        .init(
          .namedTuple([
            ("text", .string),
            (nil, .int),
          ]) --> .string)
      ],
    ]

    XCTAssertEqual(try Expr.member(namedTuple, "text").infer(), .string)
    XCTAssertEqual(try Expr.member(namedTuple, "count").infer(), .int)
    XCTAssertThrowsError(try Expr.member(namedTuple, "name").infer())

    XCTAssertEqual(
      try Expr.application("acceptNamedTuple", [namedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptNamedTuple", [tuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptNamedTuple", [mixedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptNamedTuple", [mixedTuple2])
        .infer(environment: e), .string)

    XCTAssertEqual(
      try Expr.application("acceptTuple", [namedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptTuple", [tuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptTuple", [mixedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptTuple", [mixedTuple2])
        .infer(environment: e), .string)

    XCTAssertEqual(
      try Expr.application("acceptMixedTuple", [namedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptMixedTuple", [tuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptMixedTuple", [mixedTuple])
        .infer(environment: e), .string)
    XCTAssertEqual(
      try Expr.application("acceptMixedTuple", [mixedTuple2])
        .infer(environment: e), .string)

    XCTAssertThrowsError(
      try Expr.application("acceptNamedTuple", [fewArguments])
        .infer(environment: e))
    XCTAssertThrowsError(
      try Expr.application("acceptTuple", [fewArguments])
        .infer(environment: e))
    XCTAssertThrowsError(
      try Expr.application("acceptMixedTuple", [fewArguments])
        .infer(environment: e))

    XCTAssertThrowsError(
      try Expr.application("acceptNamedTuple", [wrongOrder])
        .infer(environment: e))
    XCTAssertThrowsError(
      try Expr.application("acceptTuple", [wrongOrder])
        .infer(environment: e))
    XCTAssertThrowsError(
      try Expr.application("acceptMixedTuple", [wrongOrder])
        .infer(environment: e))
  }

  func testOverload() throws {
    let uint = Type.constructor("UInt", [])

    let count = Expr.member(.application("f", []), "count")
    let magnitude = Expr.member(.application("f", []), "magnitude")
    let error = Expr.member(
      .application(
        "f",
        [
          .tuple(
            [.literal("blah")]
          )
        ]), "count"
    )

    let m: Members = [
      "String": [
        "count": [.init(.int)]
      ],
      "Int": [
        "magnitude": [.init(uint)]
      ],
    ]
    let e: Environment = [
      "f": [
        .init([] --> .int),
        .init([] --> .string),
      ]
    ]

    XCTAssertEqual(try count.infer(environment: e, members: m), .int)
    XCTAssertEqual(try magnitude.infer(environment: e, members: m), uint)
    XCTAssertThrowsError(try error.infer(environment: e, members: m))
  }

  func testNestedOverload() throws {
    let uint = Type.constructor("UInt", [])
    let a = Type.constructor("A", [])
    let b = Type.constructor("B", [])

    let magnitude = Expr.member(
      .member(.application("f", []), "a"),
      "magnitude"
    )
    let count = Expr.member(
      .member(.application("f", []), "b"),
      "count"
    )
    let ambiguousCount = Expr.member(
      .member(.application("f", []), "ambiguous"),
      "count"
    )
    let ambiguousMagnitude = Expr.member(
      .member(.application("f", []), "ambiguous"),
      "magnitude"
    )
    let ambiguous = Expr.member(.application("f", []), "ambiguous")
    let error = Expr.member(
      .member(.application("f", []), "ambiguous"),
      "ambiguous"
    )

    let m: Members = [
      "A": [
        "a": [.init(.int)],
        "ambiguous": [.init(.string)],
      ],
      "B": [
        "b": [.init(.string)],
        "ambiguous": [.init(.int)],
      ],
      "String": [
        "count": [.init(.int)]
      ],
      "Int": [
        "magnitude": [.init(uint)]
      ],
    ]
    let e: Environment = [
      "f": [
        .init([] --> a),
        .init([] --> b),
      ]
    ]

    XCTAssertEqual(try count.infer(environment: e, members: m), .int)
    XCTAssertEqual(try magnitude.infer(environment: e, members: m), uint)
    XCTAssertEqual(try ambiguousCount.infer(environment: e, members: m), .int)
    XCTAssertEqual(
      try ambiguousMagnitude.infer(environment: e, members: m),
      uint)
    XCTAssertThrowsError(try ambiguous.infer(environment: e, members: m))
    XCTAssertThrowsError(try error.infer(environment: e, members: m))
  }

  static var allTests = [
    ("testTernary", testTernary)
  ]
}
