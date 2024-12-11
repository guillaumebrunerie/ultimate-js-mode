(defun ultimate-js-mode--partial-queries-css ()
  "Custom highlighting queries"
  (list

   :language 'css
   :override t
   :feature 'highlight
   '(((selectors) @css-selector))

   :language 'css
   :override t
   :feature 'highlight
   '(((property_name) @css-property))

   :language 'css
   :override t
   :feature 'highlight
   '(((plain_value) @font-lock-variable-name-face))

   :language 'css
   :override t
   :feature 'highlight
   '(((integer_value) @font-lock-number-face))

   :language 'css
   :override t
   :feature 'highlight
   '(((function_name) @font-lock-function-name-face))

   :language 'css
   :override t
   :feature 'highlight
   '(((comment) @font-lock-comment-face))

   :language 'css
   :override t
   :feature 'highlight
   '(((js_comment) @font-lock-comment-face))
   ))
