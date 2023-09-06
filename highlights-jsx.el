(defun ultimate-js-mode--partial-queries-jsx (language)
  "Custom highlighting queries"
  (list
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
)))
