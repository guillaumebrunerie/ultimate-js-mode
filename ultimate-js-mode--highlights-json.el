(defun ultimate-js-mode--partial-queries-json (language)
  "Custom highlighting queries"
  (list
   :language language
   :feature 'highlight
   '((comment) @font-lock-comment-face)

   :language language
   :feature 'highlight
   '("," @font-lock-delimiter-face)

   :language language
   :feature 'highlight
   '(["[" "]" "{" "}"] @font-lock-bracket-face)

   :language language
   :feature 'highlight
   '([(true) (false) (null)] @font-lock-builtin-face)

   :language language
   :feature 'highlight
   '((pair key: (string) @font-lock-property-name-face))

   :language language
   :feature 'highlight
   '((string) @font-lock-string-face)

   :language language
   :feature 'highlight
   '((number) @font-lock-number-face)

   :language language
   :feature 'highlight
   :override t
   '((ERROR) @font-lock-warning-face)))

(provide 'ultimate-js-mode--highlights-json)
