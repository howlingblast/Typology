//
//  Literal.swift
//  Typology
//
//  Created by Max Desiatov on 25/05/2019.
//

enum Literal: Equatable {
  case integer(Int)
  case floating(Double)
  case bool(Bool)
  case string(String)

  var defaultType: Type {
    switch self {
    case .integer:
      return .int
    case .floating:
      return .double
    case .bool:
      return .bool
    case .string:
      return .string
    }
  }
}

extension Literal: EquatableIgnoringSourceRange {
  func eq(_ other: Literal, ignoreRanges: Bool) -> Bool {
    self == other
  }
}

extension Literal: ExpressibleByStringLiteral {
  init(stringLiteral value: String) {
    self = .string(value)
  }
}

extension Literal: ExpressibleByIntegerLiteral {
  init(integerLiteral value: IntegerLiteralType) {
    self = .integer(value)
  }
}

extension Literal: ExpressibleByBooleanLiteral {
  init(booleanLiteral value: BooleanLiteralType) {
    self = .bool(value)
  }
}
