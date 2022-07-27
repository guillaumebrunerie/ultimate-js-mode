; Special identifiers
;--------------------

;; ([
;;     (identifier)
;;     (shorthand_property_identifier)
;;     (shorthand_property_identifier_pattern)
;;  ] @constant
;;  (#match? @constant "^[A-Z_][A-Z\\d_]+$"))


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

; Function and method definitions
;--------------------------------

(function
  name: (identifier) @function)
(function_declaration
  name: (identifier) @function)
(method_definition
  name: (property_identifier) @function.method)

;; (pair
;;   key: (property_identifier) @function.method
;;   value: [(function) (arrow_function)])

;; (assignment_expression
;;   left: (member_expression
;;     property: (property_identifier) @function.method)
;;   right: [(function) (arrow_function)])

(assignment_expression
  left: (member_expression
    property: (property_identifier) @property.definition))

(augmented_assignment_expression
  left: (member_expression
    property: (property_identifier) @variable))

(update_expression
  (member_expression
    property: (property_identifier) @variable))

(assignment_expression
  left: (identifier) @variable)

(augmented_assignment_expression
  left: (identifier) @variable)

(update_expression
  (identifier) @variable)

;; (variable_declarator
;;   name: (identifier) @function
;;   value: [(function) (arrow_function)])

;; (assignment_expression
;;   left: (identifier) @function
;;   right: [(function) (arrow_function)])

; Function and method calls
;--------------------------

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (member_expression
    property: (property_identifier) @function.method.call))

; Definitions
;------------

(array_pattern (identifier) @variable)
(variable_declarator . (identifier) @variable)
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

; Properties
;-----------

(property_identifier) @property

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

(template_substitution
 "${" @punctuation.special
 (_) @embedded
 "}" @punctuation.special)

[
  (string)
  (template_string)
] @string

(regex) @string.special
(number) @number

; Tokens
;-------

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
  "void"
  "while"
  "with"
  "yield"
] @keyword
