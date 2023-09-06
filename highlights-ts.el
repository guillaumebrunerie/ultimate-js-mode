(defun ultimate-js-mode--partial-queries-ts (language)
  "Custom highlighting queries"
  (list
   :language language
   :feature 'highlight
   '(_

   )
   :language language
   :feature 'highlight
   '((class_declaration
    name: (_) @font-lock-function-name-face)
   )
   :language language
   :feature 'highlight
   '((function_signature
    name: (identifier) @font-lock-function-name-face
    (:match "\\`[A-Z]" @font-lock-function-name-face))
   )
   :language language
   :feature 'highlight
   '((function_signature
    name: (identifier) @font-lock-variable-name-face)

   ; Generic type parameters
   )
   :language language
   :feature 'highlight
   '((type_parameter
     name: (type_identifier) @font-lock-type-parameter-face)

   )
   :language language
   :feature 'highlight
   '((mapped_type_clause
     name: (type_identifier) @font-lock-type-parameter-face)

   )
   :language language
   :feature 'highlight
   "(infer_type . (type_identifier) @font-lock-type-parameter-face)"

   :language language
   :feature 'highlight
   '(_

   ; Type definitions
   )
   :language language
   :feature 'highlight
   '((type_alias_declaration
    name: (type_identifier) @font-lock-type-parameter-face)
   )
   :language language
   :feature 'highlight
   '((interface_declaration
    name: (type_identifier) @font-lock-type-parameter-face)

   ; Other types
   )
   :language language
   :feature 'highlight
   '((type_identifier) @font-lock-type-face
   )
   :language language
   :feature 'highlight
   '((predefined_type) @font-lock-builtin-face

   )
   :language language
   :feature 'highlight
   '((type_arguments
     "<" @font-lock-bracket-face
     ">" @font-lock-bracket-face)

   )
   :language language
   :feature 'highlight
   '((type_parameters
     "<" @font-lock-bracket-face
     ">" @font-lock-bracket-face)

   ; Properties

   )
   :language language
   :feature 'highlight
   '((method_signature
    name: (property_identifier) @font-lock-keyword-face
    (:match "constructor" @font-lock-keyword-face))

   )
   :language language
   :feature 'highlight
   '((property_signature
    name: (property_identifier) @font-lock-property.definition-face)

   )
   :language language
   :feature 'highlight
   '((method_signature
    name: (property_identifier) @font-lock-property.definition-face)

   )
   :language language
   :feature 'highlight
   '((public_field_definition
    name: (property_identifier) @font-lock-property.definition-face)

   ; Variables

   )
   :language language
   :feature 'highlight
   '((required_parameter (identifier) @font-lock-variable-name-face)
   )
   :language language
   :feature 'highlight
   '((optional_parameter (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((import_specifier "type" (identifier) @font-lock-type-parameter-face)
   )
   :language language
   :feature 'highlight
   '((import_statement "import" "type" (import_clause (named_imports (import_specifier (identifier) @font-lock-type-parameter-face))))
   )
   :language language
   :feature 'highlight
   '((export_specifier "type" (identifier) @font-lock-type-face)
   )
   :language language
   :feature 'highlight
   '((export_statement "export" "type" (export_clause (export_specifier (identifier) @font-lock-type-face)))

   ; Keywords

   )
   :language language
   :feature 'highlight
   '([ "abstract"
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
     "module"
     "extends"
     "satisfies"
   ] @font-lock-keyword-face

   )
   :language language
   :feature 'highlight
   '((type_predicate
     "is" @font-lock-keyword-face)
   )
   :language language
   :feature 'highlight
   '((asserts
     "asserts" @font-lock-keyword-face)
   )
   :language language
   :feature 'highlight
   '((ambient_declaration
     "global" @font-lock-keyword-face)
   )
   :language language
   :feature 'highlight
   '((as_expression
     "const" @font-lock-keyword-face)

   ; Template literal types
   )
   :language language
   :feature 'highlight
   '((template_literal_type) @ts--fontify-template-string

   )
   :language language
   :feature 'highlight
   '((template_type
    "${" @font-lock-punctuation-face
    (_) @font-lock-embedded-face
    "}" @font-lock-punctuation-face)
)))
