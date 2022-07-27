; Generic type parameters
(type_parameter
  name: (type_identifier) @type.parameter)

(mapped_type_clause
  name: (type_identifier) @type.parameter)

; Other types
(type_identifier) @type
(predefined_type) @type.builtin

(type_arguments
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

(type_parameters
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

; Variables

(required_parameter (identifier) @variable.parameter)
(optional_parameter (identifier) @variable.parameter)

(import_specifier "type" (identifier) @type)
(import_statement "import" "type" (import_clause (named_imports (import_specifier (identifier) @type))))

; Keywords

[ "abstract"
  "declare"
  "enum"
  "export"
  "implements"
  "interface"
  "keyof"
  "namespace"
  "private"
  "protected"
  "public"
  "type"
  "readonly"
  "override"
  "infer"
] @keyword

[
  "?"
  ":"
] @operator

(type_predicate
  "is" @keyword)
(asserts
  "asserts" @keyword)

; Template literal types
(template_literal_type) @string
