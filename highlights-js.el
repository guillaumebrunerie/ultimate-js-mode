(defun ultimate-js-mode--queries-js (language)
  "Custom highlighting queries"
  (treesit-font-lock-rules
   :language language
   :feature 'highlight
   '(_

   )
   :language language
   :feature 'highlight
   '((jsx_opening_element (identifier) @font-lock-tag-face (:match "\\`[a-z][^.]*\\'" @font-lock-tag-face))
   )
   :language language
   :feature 'highlight
   '((jsx_closing_element (identifier) @font-lock-tag-face (:match "\\`[a-z][^.]*\\'" @font-lock-tag-face))
   )
   :language language
   :feature 'highlight
   '((jsx_self_closing_element (identifier) @font-lock-tag-face (:match "\\`[a-z][^.]*\\'" @font-lock-tag-face))
   )
   :language language
   :feature 'highlight
   '((jsx_opening_element (identifier) @font-lock-function-name-face)
   )
   :language language
   :feature 'highlight
   '((jsx_closing_element (identifier) @font-lock-function-name-face)
   )
   :language language
   :feature 'highlight
   '((jsx_self_closing_element (identifier) @font-lock-function-name-face)

   )
   :language language
   :feature 'highlight
   '((jsx_attribute (property_identifier) @font-lock-attribute-face)
   )
   :language language
   :feature 'highlight
   '((jsx_opening_element (["<" ">"]) @font-lock-bracket-face)
   )
   :language language
   :feature 'highlight
   '((jsx_closing_element (["</" ">"]) @font-lock-bracket-face)
   )
   :language language
   :feature 'highlight
   '((jsx_self_closing_element (["<" "/>"]) @font-lock-bracket-face)
   ; Special identifiers
   ;--------------------

   )
   :language language
   :feature 'highlight
   '(((identifier) @font-lock-number-face
    (:match "\\`(Infinity\\|NaN)\\'" @font-lock-number-face))

   )
   :language language
   :feature 'highlight
   '(((identifier) @font-lock-function-name-face
    (:match "\\`[A-Z]" @font-lock-function-name-face))

   )
   :language language
   :feature 'highlight
   '((member_expression (property_identifier) @font-lock-function-name-face
    (:match "\\`[A-Z]" @font-lock-function-name-face))

   )
   :language language
   :feature 'highlight
   '(((identifier) @font-lock-builtin-face
    (:match "\\`(arguments\\|module\\|console\\|window\\|document)\\'" @font-lock-builtin-face))

   )
   :language language
   :feature 'highlight
   '(((identifier) @font-lock-builtin-face
    (:match "require" @font-lock-builtin-face))

   ; Parameters
   ;-----------

   )
   :language language
   :feature 'highlight
   '((arrow_function
    parameter: (identifier) @font-lock-variable-name-face)

   ; The underscore is a trick to handle formal_parameters in JS while still having a
   ; valid rule in TS
   )
   :language language
   :feature 'highlight
   '((arrow_function
    parameters: (_ (identifier) @font-lock-variable-name-face))

   )
   :language language
   :feature 'highlight
   '((assignment_pattern
    left: (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((object_assignment_pattern
    left: (shorthand_property_identifier_pattern) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((rest_pattern
    (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((class_declaration
    name: (_) @font-lock-function-name-face)

   )
   :language language
   :feature 'highlight
   '((for_in_statement
    left: (identifier) @font-lock-variable-name-face)

   ; Function and method definitions
   ;--------------------------------

   )
   :language language
   :feature 'highlight
   '((function
     name: (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((function_declaration
     name: (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((method_definition
    name: (property_identifier) @font-lock-keyword-face
    (:match "constructor" @font-lock-keyword-face))

   ;; (method_definition
   ;;   name: (property_identifier) @font-lock-property.definition-face)

   ; Function and method calls
   ;--------------------------

   ;; (call_expression
   ;;   function: (identifier) @font-lock-function.call-face)

   ;; (call_expression
   ;;   function: (member_expression
   ;;     property: (property_identifier) @font-lock-function.method.call-face))

   ; Properties definitions
   ;-----------------------

   ;; (pair key: (property_identifier) @font-lock-property.definition-face)

   ;; (pair_pattern key: (property_identifier) @font-lock-property.definition-face)

   ; Definitions
   ;------------

   )
   :language language
   :feature 'highlight
   '((array_pattern (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   "(variable_declarator . (identifier) @font-lock-variable-name-face)"

   :language language
   :feature 'highlight
   '(_

   ; Properties
   ;-----------

   ;; (member_expression
   ;;  property: (property_identifier) @font-lock-property-face)

   ; Imports
   ;--------

   )
   :language language
   :feature 'highlight
   '((import_clause (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((import_specifier
     alias: (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((import_specifier
     name: (identifier) @font-lock-variable-name-face
     !alias)

   )
   :language language
   :feature 'highlight
   '((namespace_import (identifier) @font-lock-variable-name-face)

   )
   :language language
   :feature 'highlight
   '((object_pattern
    [
     (pair_pattern value: (identifier) @font-lock-variable-name-face)
     (shorthand_property_identifier_pattern) @font-lock-variable-name-face
     (rest_pattern (identifier) @font-lock-variable-name-face)
     ])


   ; Variables
   ;----------

   ;; ([
   ;;     (identifier)
   ;;     (shorthand_property_identifier)
   ;;     (shorthand_property_identifier_pattern)
   ;;  ] @font-lock-variable.call-face)


   ; Tokens
   ;-------

   )
   :language language
   :feature 'highlight
   '((template_substitution
    "${" @font-lock-punctuation-face
    "}" @font-lock-punctuation-face)

   )
   :language language
   :feature 'highlight
   '([
     ";"
     (optional_chain)
     "."
     ","
   ] @font-lock-delimiter-face

   )
   :language language
   :feature 'highlight
   '([
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
   ] @font-lock-operator-face

   )
   :language language
   :feature 'highlight
   '([
     "("
     ")"
     "["
     "]"
     "{"
     "}"
   ] @font-lock-bracket-face

   )
   :language language
   :feature 'highlight
   '([
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
   ] @font-lock-keyword-face

   )
   :language language
   :feature 'highlight
   '((unary_expression "void" @font-lock-keyword-face)

   ; Literals
   ;---------

   )
   :language language
   :feature 'highlight
   '([
    (this)
    (super)
   ] @font-lock-builtin-face

   )
   :language language
   :feature 'highlight
   '([
     (true)
     (false)
     (null)
     (undefined)
   ] @font-lock-builtin-face

   )
   :language language
   :feature 'highlight
   '([
    (comment)
    (hash_bang_line)
   ] @font-lock-comment-face

   )
   :language language
   :feature 'highlight
   '([
    (string)
   ] @font-lock-string-face

   )
   :language language
   :feature 'highlight
   '((template_substitution
    "${" @font-lock-punctuation-face
    (_) @font-lock-embedded-face
    "}" @font-lock-punctuation-face)

   )
   :language language
   :feature 'highlight
   '([
    (template_string)
   ] @js--fontify-template-string

   )
   :language language
   :feature 'highlight
   '([
    (regex)
   ] @font-lock-regexp-face

   )
   :language language
   :feature 'highlight
   '([
    (number)
   ] @font-lock-number-face
)))
