//
//  ASTError.swift
//  Typology
//
//  Created by Max Desiatov on 07/06/2019.
//

import Foundation
import SwiftSyntax

struct ASTError: DiagnosticError {
  enum Value {
    case unknownExprSyntax
    case unknownStmtSyntax
    case unknownTypeSyntax
    case unknownPatternSyntax
    case unknownSyntax
  }

  let range: SourceRange
  let value: Value
  let syntax: String
}

extension ASTError {
  init(_ syntax: Syntax, _ value: Value, _ converter: SourceLocationConverter) {
    self.init(
      range: syntax.sourceRange(converter: converter), value: value,
      syntax: syntax.debugDescription())
  }
}
