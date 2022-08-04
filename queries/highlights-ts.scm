(class_declaration
 name: (_) @constructor)

; Generic type parameters
(type_parameter
  name: (type_identifier) @type.parameter)

(mapped_type_clause
  name: (type_identifier) @type.parameter)

(infer_type (type_identifier) @type.parameter)

; Type definitions
(type_alias_declaration
 name: (type_identifier) @type.parameter)
(interface_declaration
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

; Properties

(method_signature
 name: (property_identifier) @keyword
 (#match? @keyword "constructor"))

(property_signature
 name: (property_identifier) @property.definition)

(method_signature
 name: (property_identifier) @property.definition)

(public_field_definition
 name: (property_identifier) @property.definition)

; Variables

(required_parameter (identifier) @variable)
(optional_parameter (identifier) @variable)

(import_specifier "type" (identifier) @type.parameter)
(import_statement "import" "type" (import_clause (named_imports (import_specifier (identifier) @type.parameter))))

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

(type_predicate
  "is" @keyword)
(asserts
  "asserts" @keyword)
(ambient_declaration
  "global" @keyword)
(ambient_declaration
  "module" @keyword)
(as_expression
  "const" @keyword)

; Template literal types
(template_literal_type) @string

(template_type
 "${" @punctuation.special
 (_) @embedded
 "}" @punctuation.special)
