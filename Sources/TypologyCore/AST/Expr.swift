//
//  Expr.swift
//  Typology
//
//  Created by Max Desiatov on 12/05/2019.
//

import Foundation
import SwiftOperators
import SwiftSyntax

private let dummyRange = SourceRange(
  start: SourceLocation(offset: 0), end: SourceLocation(offset: 0))

struct ExprNode: Statement, Equatable {
  private static func wrap<A>(_ ctor: @escaping (A) -> Expr) -> (A) -> ExprNode {
    { ExprNode(expr: ctor($0), range: dummyRange) }
  }

  private static func wrap<A, B>(_ ctor: @escaping (A, B) -> Expr) -> (A, B) -> ExprNode {
    { ExprNode(expr: ctor($0, $1), range: dummyRange) }
  }

  private static func wrap<A, B, C>(_ ctor: @escaping (A, B, C) -> Expr) -> (A, B, C) -> ExprNode {
    { ExprNode(expr: ctor($0, $1, $2), range: dummyRange) }
  }

  let expr: Expr
  let range: SourceRange

  func with(expr: Expr? = nil, range: SourceRange? = nil) -> ExprNode {
    ExprNode(expr: expr ?? self.expr, range: range ?? self.range)
  }

  static let identifier = wrap(Expr.identifier)
  static let application = wrap(Expr.application)
  static let lambda = wrap(Expr.lambda)
  static let literal = wrap(Expr.literal)
  static let interpolatedString = wrap(Expr.interpolatedString)
  static let ternary = wrap(Expr.ternary)
  static let member = wrap(Expr.member)
  static let namedTuple = wrap(Expr.namedTuple)
  static let binary = wrap(Expr.binary)
  static let tuple = wrap(Expr.tuple)
}

extension ExprNode {
  init(_ syntax: ExprSyntax, _ converter: SourceLocationConverter) throws {
    self = try syntax.toExprNode(converter)
  }

  init(_ expr: Expr, range: SourceRange? = nil) {
    self.init(expr: expr, range: range ?? dummyRange)
  }
}

indirect enum Expr {
  case identifier(Identifier)
  case application(ExprNode, [ExprNode])
  case lambda([Identifier], ExprNode)
  case literal(Literal)
  case interpolatedString(String, [(ExprNode, String)])
  case ternary(ExprNode, ExprNode, ExprNode)
  case member(ExprNode, Identifier)
  case namedTuple([(Identifier?, ExprNode)])
  case binary(ExprNode, Identifier, ExprNode)

  static func tuple(_ expressions: [ExprNode]) -> Expr {
    return .namedTuple(expressions.map { (nil, $0) })
  }

  func infer(
    environment: Environment = [:],
    members: Members = [:]
  ) throws -> Type {
    var system = ConstraintSystem(
      environment,
      members: members
    )
    let type = try system.infer(self)

    let solver = Solver(
      substitution: [:],
      system: system
    )
    return try type.apply(solver.solve())
  }
}

extension Expr {
  init(
    _ expr: ExprSyntaxProtocol, _ converter: SourceLocationConverter,
    operatorTable: OperatorTable = .standardOperators
  ) throws {
    self = try expr.toExpr(converter, operatorTable: operatorTable)
  }
}

extension Expr: Equatable {
  static func == (lhs: Expr, rhs: Expr) -> Bool {
    lhs.eq(rhs, ignoreRanges: false)
  }
}

extension Expr: ExpressibleByStringLiteral {
  init(stringLiteral value: String) {
    self = .identifier(value)
  }
}

extension ExprNode: ExpressibleByStringLiteral {
  init(stringLiteral value: String) {
    self = .identifier(value)
  }
}

extension ExprSyntaxProtocol {
  func toExprNode(
    _ converter: SourceLocationConverter, operatorTable: OperatorTable = .standardOperators
  ) throws
    -> ExprNode
  {
    .init(
      expr: try Expr(self, converter, operatorTable: operatorTable),
      range: self.sourceRange(converter: converter)
    )
  }

  func toExpr(
    _ converter: SourceLocationConverter, operatorTable: OperatorTable = .standardOperators
  )
    throws -> Expr
  {
    switch Syntax(self).as(SyntaxEnum.self) {
    case .identifierExpr(let identifier):
      return .identifier(identifier.identifier.text)

    case .functionCallExpr(let call):
      return try .application(
        call.calledExpression.toExprNode(converter, operatorTable: operatorTable),
        call.argumentList.map {
          try $0.expression.toExprNode(converter, operatorTable: operatorTable)
        }
      )

    case .sequenceExpr(let sequence):
      let folded = try operatorTable.foldSingle(sequence)

      return try folded.toExpr(converter, operatorTable: operatorTable)

    case .infixOperatorExpr(let binary):
      guard let op = BinaryOperatorExprSyntax(binary.operatorOperand) else {
        throw ASTError(binary.operatorOperand._syntaxNode, .unknownExprSyntax, converter)
      }

      return try .binary(
        binary.leftOperand.toExprNode(converter, operatorTable: operatorTable),
        op.operatorToken.text,
        binary.rightOperand.toExprNode(converter, operatorTable: operatorTable)
      )

    case .integerLiteralExpr(let literal):
      guard let int = Int(literal.digits.text) else {
        throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
      }
      return .literal(.integer(int))

    case .floatLiteralExpr(let literal):
      guard let double = Double(literal.floatingDigits.text) else {
        throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
      }
      return .literal(.floating(double))

    case .booleanLiteralExpr(let literal):
      guard let bool = Bool(literal.booleanLiteral.text) else {
        throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
      }
      return .literal(.bool(bool))

    case .stringLiteralExpr(let literal):
      var segments = literal.segments.makeIterator()
      var interpolations: [(ExprNode, String)] = []
      guard case .stringSegment(let left) = segments.next() else {
        throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
      }

      while let segment = segments.next() {
        guard case .expressionSegment(let expr) = segment else {
          throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
        }

        let converted = try expr.expressions.map {
          try $0.expression.toExprNode(converter, operatorTable: operatorTable)
        }
        let exprNode =
          converted.count == 1
          ? converted[0]
          : ExprNode(expr: .tuple(converted), range: expr.sourceRange(converter: converter))

        guard case .stringSegment(let literal) = segments.next() else {
          throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
        }

        interpolations.append((exprNode, literal.content.text))
      }

      if interpolations.isEmpty {
        return .literal(.string(left.content.text))
      }

      return .interpolatedString(left.content.text, interpolations)

    case .ternaryExpr(let ternary):
      return try .ternary(
        ternary.conditionExpression.toExprNode(converter, operatorTable: operatorTable),
        ternary.firstChoice.toExprNode(converter, operatorTable: operatorTable),
        ternary.secondChoice.toExprNode(converter, operatorTable: operatorTable)
      )

    default:
      throw ASTError(_syntaxNode, .unknownExprSyntax, converter)
    }
  }
}

protocol EquatableIgnoringSourceRange {
  func eq(_ other: Self, ignoreRanges: Bool) -> Bool
}

func ~= (pattern: Expr, value: Expr) -> Bool {
  pattern.eq(value, ignoreRanges: true)
}

func ~= (pattern: Expr, value: ExprNode) -> Bool {
  pattern.eq(value.expr, ignoreRanges: true)
}

extension Expr: EquatableIgnoringSourceRange {
  func eq(_ other: Self, ignoreRanges: Bool) -> Bool {
    switch (self, other) {
    case let (.identifier(li), .identifier(ri)):
      return li.eq(ri, ignoreRanges: ignoreRanges)

    case let (.application(lf, largs), .application(rf, rargs)):
      return TypologyCore.eq((lf, largs), (rf, rargs), ignoreRanges: ignoreRanges)

    case let (.lambda(lp, lb), .lambda(rp, rb)):
      return TypologyCore.eq((lp, lb), (rp, rb), ignoreRanges: ignoreRanges)

    case let (.literal(l), .literal(r)):
      return l.eq(r, ignoreRanges: ignoreRanges)

    case let (.interpolatedString(lstr, largs), .interpolatedString(rstr, rargs)):
      return lstr.eq(rstr, ignoreRanges: ignoreRanges)
        && largs.elementsEqual(
          rargs, by: { l, r in TypologyCore.eq(l, r, ignoreRanges: ignoreRanges) })

    case let (.ternary(lc, lt, le), .ternary(rc, rt, re)):
      return TypologyCore.eq((lc, lt, le), (rc, rt, re), ignoreRanges: ignoreRanges)

    case let (.member(lx, lm), .member(rx, rm)):
      return TypologyCore.eq((lx, lm), (rx, rm), ignoreRanges: ignoreRanges)

    case let (.namedTuple(lxs), .namedTuple(rxs)):
      return lxs.elementsEqual(
        rxs, by: { l, r in TypologyCore.eq(l, r, ignoreRanges: ignoreRanges) })

    case let (.binary(ll, lo, lr), .binary(rl, ro, rr)):
      return TypologyCore.eq((ll, lo, lr), (rl, ro, rr), ignoreRanges: ignoreRanges)

    default:
      return false
    }
  }
}

extension ExprNode: EquatableIgnoringSourceRange {
  func eq(_ other: Self, ignoreRanges: Bool) -> Bool {
    if !ignoreRanges && range != other.range {
      return false
    }

    return expr.eq(other.expr, ignoreRanges: ignoreRanges)
  }
}

extension Identifier: EquatableIgnoringSourceRange {
  func eq(_ other: Identifier, ignoreRanges: Bool) -> Bool {
    self == other
  }
}

extension Optional: EquatableIgnoringSourceRange where Wrapped: EquatableIgnoringSourceRange {
  func eq(_ other: Wrapped?, ignoreRanges: Bool) -> Bool {
    switch (self, other) {
    case (.none, .none):
      return true

    case (let a?, let b?):
      return a.eq(b, ignoreRanges: ignoreRanges)

    default:
      return false
    }
  }
}

extension Array: EquatableIgnoringSourceRange where Element: EquatableIgnoringSourceRange {
  func eq(_ other: [Element], ignoreRanges: Bool) -> Bool {
    elementsEqual(other, by: { $0.eq($1, ignoreRanges: ignoreRanges) })
  }
}

func eq<A, B>(_ a: (A, B), _ b: (A, B), ignoreRanges: Bool) -> Bool
where A: EquatableIgnoringSourceRange, B: EquatableIgnoringSourceRange {
  a.0.eq(b.0, ignoreRanges: ignoreRanges) && a.1.eq(b.1, ignoreRanges: ignoreRanges)
}

func eq<A, B, C>(_ a: (A, B, C), _ b: (A, B, C), ignoreRanges: Bool) -> Bool
where
  A: EquatableIgnoringSourceRange, B: EquatableIgnoringSourceRange, C: EquatableIgnoringSourceRange
{
  a.0.eq(b.0, ignoreRanges: ignoreRanges) && a.1.eq(b.1, ignoreRanges: ignoreRanges)
    && a.2.eq(b.2, ignoreRanges: ignoreRanges)
}
