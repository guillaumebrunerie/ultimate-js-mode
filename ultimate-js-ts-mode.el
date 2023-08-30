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

(defvar json--treesit-indent-rules
  `((json
     ((parent-is "program") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "array") parent-bol js-indent-level)
     ((parent-is "object") parent-bol js-indent-level)
     ((parent-is "pair") parent-bol js-indent-level)
     (no-node parent-bol 0))))

(defun ultimate-js-ts-mode--indent-rules (lang)
  "Indentation rules"
  (cond
   ((eq lang 'json) json--treesit-indent-rules)
   ((eq lang 'javascript) js--treesit-indent-rules)
   ((eq lang 'typescript) (typescript-ts-mode--indent-rules lang))
   ((eq lang 'tsx) (typescript-ts-mode--indent-rules lang))))

(load "highlights-json")
(load "highlights-js")
(load "highlights-ts")
(load "highlights-tsx")

(defun ultimate-js-ts-mode--font-lock-settings (lang)
  "Highlighting rules"
  (cond
   ((eq lang 'json) (ultimate-js-mode--queries-json lang))
   ((eq lang 'javascript) (ultimate-js-mode--queries-js lang))
   ((eq lang 'typescript) (ultimate-js-mode--queries-ts lang))
   ((eq lang 'tsx) (ultimate-js-mode--queries-tsx lang))))



(defun ts--fontify-template-string (node override start end &rest _)
  "Fontify template string but not substitution inside it.
NODE is the template_string node.  START and END mark the region
to be fontified.

OVERRIDE is the override flag described in
`treesit-font-lock-rules'."
  ;; You would have thought that the children of the string node spans
  ;; the whole string.  No, the children of the template_string only
  ;; includes the starting "`", any template_substitution, and the
  ;; closing "`".  That's why we have to track BEG instead of just
  ;; fontifying each child.
  (let ((child (treesit-node-child node 0))
        (font-beg (treesit-node-start node)))
    (while child
      (let ((font-end (if (equal (treesit-node-type child)
                                 "template_type")
                          (treesit-node-start child)
                        (treesit-node-end child))))
        (setq font-beg (max start font-beg))
        (when (< font-beg end)
          (treesit-fontify-with-override
           font-beg font-end 'font-lock-string-face override start end)))
      (setq font-beg (treesit-node-end child)
            child (treesit-node-next-sibling child)))))



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
   ((string-suffix-p ".json" buffer-file-name) (setq mode-name "UltimateJSON[TS]") (setq ultimate-js--lang 'json))
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

;;;; Interactive commands and keybindings
(defun ultimate-js-ts-electric-lt ()
  "Insert a context-sensitive less-than sign.
If the less-than sign would start a JSX block, it
inserts `</>' and places the cursor inside the new tag.

Adapted from RJSX"
    (interactive)
    (if (save-excursion
          (forward-comment most-negative-fixnum)
          (skip-chars-backward "\n\r")
          (or (= (point) (point-min))
              (memq (char-before) (append "=(?:>}&|{," nil))
              (let ((start (- (point) 6)))
                (and (>= start (point-min))
                     (string= (buffer-substring start (point)) "return")))))
        (progn (self-insert-command 1 ?<)
               (insert "/>")
               (backward-char 2))
      (self-insert-command 1 ?<)))

(defun ultimate-js-ts-expand-self-closing-tag (text)
  "Expand NODE into a balanced tag.
Assumes NODE is self-closing `rjsx-node', and that point is at
the self-closing slash."
  (delete-char 1)
  (search-forward ">")
  (save-excursion
    (insert "</" text ">")))

(defun ultimate-js-ts--get-self-closing-tag-name ()
  "Checks whether the point is on the slash of a self-closing tag. If so, return
the name of the tag, otherwise return nil."
  (if (and (looking-at-p "/>")
           (eq (char-before) ?<))
      ""
    (let* ((node (treesit-node-at (point)))
           (parent (and node (treesit-node-parent node))))
      (when (and node
                 parent
                 (string= (treesit-node-type node) "/>")
                 (string= (treesit-node-type parent) "jsx_self_closing_element"))
        (treesit-node-text (treesit-node-child parent 1))))))

(defun ultimate-js-ts-electric-gt ()
  "Insert a context-sensitive greater-than sign.
If point is in a self-closing JSX tag just before the
slash, it creates a matching end-tag and places point just inside
the tags.
Taken from RJSX"
  (interactive)
  (let ((text (ultimate-js-ts--get-self-closing-tag-name)))
    (if text
        (ultimate-js-ts-expand-self-closing-tag text)
      (self-insert-command 1 ?>))))

(define-key ultimate-js-ts-mode-map "<" 'ultimate-js-ts-electric-lt)
(define-key ultimate-js-ts-mode-map ">" 'ultimate-js-ts-electric-gt)

(provide 'ultimate-js-ts-mode)
