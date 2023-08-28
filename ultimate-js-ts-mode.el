;;; ultimate-js-ts-mode.el --- Major mode for editing JS files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Guillaume Brunerie

;; Author: Guillaume Brunerie <guillaume.brunerie@gmail.com>
;; URL: https://github.com/guillaumebrunerie/ultimate-js-mode

;; See README.md for more information.

(require 'treesit)
(require 'js)
(require 'typescript-ts-mode)
(eval-when-compile (require 'rx))
(require 'c-ts-common)

(declare-function treesit-parser-create "treesit.c")

(defun ultimate-js-ts-mode--indent-rules (lang)
  "Indentation rules"
  (cond
   ((eq lang 'javascript) js--treesit-indent-rules)
   ((eq lang 'typescript) (typescript-ts-mode--indent-rules lang))
   ((eq lang 'tsx) (typescript-ts-mode--indent-rules lang))))

(load "highlights-js")
(load "highlights-ts")
(load "highlights-tsx")

(defun ultimate-js-ts-mode--font-lock-settings (lang)
  "Highlighting rules"
  (cond
   ((eq lang 'javascript) (ultimate-js-mode--queries-js lang))
   ((eq lang 'typescript) (ultimate-js-mode--queries-ts lang))
   ((eq lang 'tsx) (ultimate-js-mode--queries-tsx lang))))

;;;###autoload
(define-derived-mode ultimate-js-ts-mode prog-mode "UltimateJS[TS]"
  "Major mode for editing JS/JSX/TS/TSX/JSON files (Emacs 29 version)."
  :group 'ultimate-js
  :syntax-table typescript-ts-mode--syntax-table

  ;; Comments (TODO: better handling of comments in JSX)
  (c-ts-common-comment-setup)

  ;; Electricity
  (setq-local electric-indent-chars
              (append "{}():;,<>/" electric-indent-chars))
  (setq-local electric-layout-rules
          '((?\; . after) (?\{ . after) (?\} . before)))

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("class_declaration"
                            "method_definition"
                            "function_declaration"
                            "lexical_declaration")))
  (setq-local treesit-defun-name-function #'js--treesit-defun-name)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `(("Function" "\\`function_declaration\\'" nil nil)
                ("Variable" "\\`lexical_declaration\\'"
                 js--treesit-valid-imenu-entry nil)
                ("Class" ,(rx bos (or "class_declaration"
                                      "method_definition")
                              eos)
                 nil nil)))

  ;; The name of the tree-sitter grammar, `javascript` for both .js and .jsx,
  ;; `typescript` for .ts, and `tsx` for .tsx
  (setq-local ultimate-js--lang 'javascript)
  ;; Determine the language based on the file extension
  (cond
   ((string-suffix-p ".json" buffer-file-name) (setq mode-name "UltimateJSON[TS]"))
   ((string-suffix-p ".jsx" buffer-file-name) (setq mode-name "UltimateJSX[TS]"))
   ((string-suffix-p ".d.ts" buffer-file-name) (setq mode-name "UltimateDTS[TS]") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".ts" buffer-file-name) (setq mode-name "UltimateTS[TS]") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".cts" buffer-file-name) (setq mode-name "UltimateTS[TS]") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".mts" buffer-file-name) (setq mode-name "UltimateTS[TS]") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".tsx" buffer-file-name) (setq mode-name "UltimateTSX[TS]") (setq ultimate-js--lang 'tsx)))

  (when (treesit-ready-p ultimate-js--lang)
    (treesit-parser-create ultimate-js--lang)

    ;; Indentation
    (setq-local
     treesit-simple-indent-rules
     (ultimate-js-ts-mode--indent-rules ultimate-js--lang))

    ;; Syntax highlighting
    (setq-local
     treesit-font-lock-settings
     (ultimate-js-ts-mode--font-lock-settings ultimate-js--lang))
    (setq-local treesit-font-lock-feature-list
                '((highlight)))

    (treesit-major-mode-setup)))

(provide 'ultimate-js-ts-mode)
