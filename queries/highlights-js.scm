; Parameters
;-----------

(arrow_function
 parameter: (identifier) @variable)

; The underscore is a trick to handle formal_parameters in JS while still having a
; valid rule in TS
(arrow_function
 parameters: (_ (identifier) @variable))

(assignment_pattern
 left: (identifier) @variable)

(object_assignment_pattern
 left: (shorthand_property_identifier_pattern) @variable)

(rest_pattern
 (identifier) @variable)

(class_declaration
 name: (_) @constructor)

(for_in_statement
 left: (identifier) @variable)

; Function and method definitions
;--------------------------------

(function
  name: (identifier) @function)
(function_declaration
  name: (identifier) @function)
(method_definition
 name: (property_identifier) @keyword
 (#match? @keyword "constructor"))
(method_definition
  name: (property_identifier) @property.definition)

; Function and method calls
;--------------------------

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (member_expression
    property: (property_identifier) @function.method.call))

; Properties definitions
;-----------------------

(pair key: (property_identifier) @property.definition)
(pair_pattern key: (property_identifier) @property.definition)


; Special identifiers
;--------------------

((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

((property_identifier) @constructor
 (#match? @constructor "^[A-Z]"))

((identifier) @variable.builtin
 (#match? @variable.builtin "^(arguments|module|console|window|document)$")
 (#is-not? local))

((identifier) @function.builtin
 (#eq? @function.builtin "require")
 (#is-not? local))

; Definitions
;------------

(array_pattern (identifier) @variable)
(variable_declarator . (identifier) @variable)

; Properties
;-----------

(member_expression
 property: (property_identifier) @property)

; Imports
;--------

(import_clause (identifier) @variable)
(import_specifier
  name: (identifier) @property
  alias: (identifier) @variable)
(import_specifier
  name: (identifier) @variable
  !alias)
(namespace_import (identifier) @variable)
(object_pattern
 [
  (pair_pattern value: (identifier) @variable)
  (shorthand_property_identifier_pattern) @variable
  (rest_pattern (identifier) @variable)
  ])


; Variables
;----------

([
    (identifier)
    (shorthand_property_identifier)
    (shorthand_property_identifier_pattern)
 ] @variable.call)


; Tokens
;-------

(template_substitution
 "${" @punctuation.special
 "}" @punctuation.special)

[
  ";"
  "?."
  "."
  ","
] @punctuation.delimiter

[
  "-"
  "--"
  "-="
  "+"
  "++"
  "+="
  "*"
  "*="
  "**"
  "**="
  "/"
  "/="
  "%"
  "%="
  "<"
  "<="
  "<<"
  "<<="
  "="
  "=="
  "==="
  "!"
  "!="
  "!=="
  "=>"
  ">"
  ">="
  ">>"
  ">>="
  ">>>"
  ">>>="
  "~"
  "^"
  "&"
  "|"
  "^="
  "&="
  "|="
  "&&"
  "||"
  "??"
  "&&="
  "||="
  "??="
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
]  @punctuation.bracket

[
  "as"
  "async"
  "await"
  "break"
  "case"
  "catch"
  "class"
  "const"
  "continue"
  "debugger"
  "default"
  "delete"
  "do"
  "else"
  "export"
  "extends"
  "finally"
  "for"
  "from"
  "function"
  "get"
  "if"
  "import"
  "in"
  "instanceof"
  "let"
  "new"
  "of"
  "return"
  "set"
  "static"
  "switch"
  "target"
  "throw"
  "try"
  "typeof"
  "var"
  "while"
  "with"
  "yield"
] @keyword

(unary_expression "void" @keyword)

; Literals
;---------

(this) @variable.builtin
(super) @variable.builtin

[
  (true)
  (false)
  (null)
  (undefined)
] @constant.builtin

(comment) @comment
(hash_bang_line) @comment

(string) @string

(template_substitution
 "${" @punctuation.special
 (_) @embedded
 "}" @punctuation.special)

(template_string) @string

(regex) @string.special
(number) @number
