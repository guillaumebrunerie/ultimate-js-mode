;;; ultimate-js-mode.el --- Major mode for editing JS files  -*- lexical-binding: t; -*-

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


;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; Return the parent or grand parent if it is a template string
(defvar ultimate-js--template-string
  (lambda (node parent bol &rest _)
    (cond ((string= (treesit-node-type parent) "template_string") parent)
          ((string= (treesit-node-type (treesit-node-parent parent)) "template_string") (treesit-node-parent parent)))))

;; Determine when to indent inside a template string (first line and after an
;; opening brace)
(defvar ultimate-js--should-indent
  (lambda (node parent bol &rest _)
    (let ((current-line (line-number-at-pos bol))
          (previous-line (line-number-at-pos (treesit-node-start (funcall ultimate-js--template-string node parent bol)))))
      (or
       (= current-line (1+ previous-line))
       (save-excursion
         (goto-char bol)
         (forward-line -1)
         (end-of-line)
         (backward-char 1)
         (looking-at "{"))))))

;; Determine when to deindent inside a template string (closing brace or the end
;; of the template string)
(defvar ultimate-js--should-deindent
  (lambda (_n _p bol &rest _)
    (save-excursion
      (goto-char bol)
      (beginning-of-line)
      (looking-at "[ \t]*\\(}$\\|`\\)"))))

;; Fix indentation rules, and add custom ones for styled components
(defun ultimate-js--custom-ts-indent-rules (lang)
  `((,lang
     ;; Needs to be repeated here (for template substitutions)
     ((node-is "}") parent-bol 0)
     ;; Rules for template strings, with support for styled components
     ((and ,ultimate-js--template-string ,ultimate-js--should-indent) prev-line typescript-ts-mode-indent-offset)
     ((and ,ultimate-js--template-string ,ultimate-js--should-deindent) prev-line ,(- 4)) ;; Not sure how to deindent with a variable
     (,ultimate-js--template-string prev-line 0)
     ;; Fix text inside JSX
     ((parent-is "jsx_text") grand-parent typescript-ts-mode-indent-offset)
     ;; Fix interfaces
     ((parent-is "interface_body") parent-bol typescript-ts-mode-indent-offset)
     ;; Fix switch/case rules
     ((parent-is "switch_body") parent-bol typescript-ts-mode-indent-offset)
     ;; Fix exports
     ((parent-is "export_clause") parent-bol typescript-ts-mode-indent-offset)
     ;; Original rules
     ,@(cdar (typescript-ts-mode--indent-rules lang))
     ;; Fix switch/case rules
     ((parent-is "switch_body") parent-bol typescript-ts-mode-indent-offset))))

;; Indentation rules for JSON
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

(defun ultimate-js-mode--indent-rules (lang)
  "Indentation rules"
  (cond
   ((eq lang 'json) json--treesit-indent-rules)
   ((eq lang 'javascript) js--treesit-indent-rules)
   ((eq lang 'typescript) (ultimate-js--custom-ts-indent-rules lang))
   ((eq lang 'tsx) (ultimate-js--custom-ts-indent-rules lang))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(load "highlights-json")
(load "highlights-js")
(load "highlights-ts")
(load "highlights-jsx")
(load "highlights-css")

(defvar ultimate-js-mode--queries-js
  (apply #'treesit-font-lock-rules
         (append
          ;; (ultimate-js-mode--partial-queries-css)
          (ultimate-js-mode--partial-queries-jsx 'javascript)
          (ultimate-js-mode--partial-queries-js 'javascript))))

(defvar ultimate-js-mode--queries-ts
  (apply #'treesit-font-lock-rules
         (append
          ;; (ultimate-js-mode--partial-queries-css)
          (ultimate-js-mode--partial-queries-ts 'typescript)
          (ultimate-js-mode--partial-queries-js 'typescript))))

(defvar ultimate-js-mode--queries-tsx
  (apply #'treesit-font-lock-rules
         (append
          ;; (ultimate-js-mode--partial-queries-css)
          (ultimate-js-mode--partial-queries-ts 'tsx)
          (ultimate-js-mode--partial-queries-jsx 'tsx)
          (ultimate-js-mode--partial-queries-js 'tsx))))

(defun ultimate-js-mode--font-lock-settings (lang)
  "Highlighting rules"
  (cond
   ((eq lang 'json) (ultimate-js-mode--queries-json 'json))
   ((eq lang 'javascript) ultimate-js-mode--queries-js)
   ((eq lang 'typescript) ultimate-js-mode--queries-ts)
   ((eq lang 'tsx) ultimate-js-mode--queries-tsx)))

(defvar ultimate-js-mode--syntax-table
  (let ((table (make-syntax-table typescript-ts-mode--syntax-table)))
    (modify-syntax-entry ?<  "("     table)
    (modify-syntax-entry ?>  ")"     table)
    table)
  "Syntax table for `ultimate-ts-mode'.")

(defun ultimate-js-mode--language-at-point (point)
  "Return the language at POINT ."
  (if-let ((node (treesit-node-at point 'javascript))
           ((string-equal (treesit-node-type node) "string_fragment"))
           (parent (treesit-node-parent node))
           ((string-equal (treesit-node-type parent) "template_string"))
           (grand-parent (treesit-node-parent parent))
           ((string-equal (treesit-node-type grand-parent) "call_expression"))
           (tag (treesit-node-child grand-parent 0))
           ((string-equal (treesit-node-text tag) "css")))
      'css ultimate-js--lang))

;;;;;;;;;;;;;;;;
;; Major mode ;;
;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode ultimate-js-mode prog-mode "UltimateJS"
  "Major mode for editing JS/JSX/TS/TSX/JSON files."
  :group 'ultimate-js
  :syntax-table ultimate-js-mode--syntax-table

  ;; Comments
  (c-ts-common-comment-setup)

  ;; Electricity
  (setq-local electric-indent-chars
              (append "{}():;,<>/" electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  ;; The name of the tree-sitter grammar, `javascript` for both .js and .jsx,
  ;; `typescript` for .ts, and `tsx` for .tsx
  (setq-local ultimate-js--lang 'javascript)
  ;; Determine the language based on the file extension
  (cond
   ((string-suffix-p ".json" buffer-file-name) (setq mode-name "UltimateJSON") (setq ultimate-js--lang 'json))
   ((string-suffix-p ".jsx" buffer-file-name) (setq mode-name "UltimateJSX"))
   ((string-suffix-p ".d.ts" buffer-file-name) (setq mode-name "UltimateDTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".d.cts" buffer-file-name) (setq mode-name "UltimateDTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".d.mts" buffer-file-name) (setq mode-name "UltimateDTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".ts" buffer-file-name) (setq mode-name "UltimateTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".cts" buffer-file-name) (setq mode-name "UltimateTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".mts" buffer-file-name) (setq mode-name "UltimateTS") (setq ultimate-js--lang 'typescript))
   ((string-suffix-p ".tsx" buffer-file-name) (setq mode-name "UltimateTSX") (setq ultimate-js--lang 'tsx)))

  (when (treesit-ready-p ultimate-js--lang)
    (treesit-parser-create ultimate-js--lang)

    ;; (treesit-parser-create 'css)
    ;; (setq-local treesit-range-settings
    ;;             (treesit-range-rules
    ;;              :embed 'css
    ;;              :host ultimate-js--lang
    ;;              '((call_expression
    ;;                 function: (identifier) @_tag
    ;;                 (:match "\\`css\\'" @_tag)
    ;;                 arguments: (template_string (string_fragment) @capture)))))
    ;; (setq-local treesit-language-at-point-function #'ultimate-js-mode--language-at-point)

    ;; Indentation
    (setq-local
     treesit-simple-indent-rules
     (ultimate-js-mode--indent-rules ultimate-js--lang))

    ;; Syntax highlighting
    (setq-local
     treesit-font-lock-settings
     (ultimate-js-mode--font-lock-settings ultimate-js--lang))
    (setq-local treesit-font-lock-feature-list
                '((highlight)))

    (treesit-major-mode-setup)))


;;;;;;;;;;;;;;
;; Comments ;;
;;;;;;;;;;;;;;

;; JSX needs to be commented out differently, with {/* */}, so we try to detect
;; which kind of comment is needed using tree-sitter.
;;
;; Go up the tree from the beginning position until we hit
;; - a jsx_expression: we are inside (nested) JavaScript, use normal commenting
;; - a jsx_self_closing_element/jsx_opening_element: we are inside attributes in a JSX tag, use normal commenting as well
;; - a jsx_element: we are inside JSX, use special commenting!
;; - nothing: we are inside JavaScript, use normal commenting
(defun ultimate-js--comment-dwim (arg)
  (interactive "*P")
  (let* ((beg (region-beginning))
         (stops '("jsx_element" "jsx_expression" "jsx_self_closing_element" "jsx_opening_element"))
         (parent (treesit-parent-until
                  (treesit-node-at beg)
                  (lambda (node) (and (seq-contains-p stops (treesit-node-type node) #'string=) (< (treesit-node-start node) beg))))))
    (if (and parent (string= (treesit-node-type parent) "jsx_element"))
        (let ((comment-start "{/*")
              (comment-end "*/}")
              (comment-end-skip " *\\*/}")
              (comment-start-skip "{/\\* *"))
          (comment-dwim arg))
      (comment-dwim arg))))
(define-key ultimate-js-mode-map (kbd "M-;") #'ultimate-js--comment-dwim)


;;;;;;;;;;;;;;;;;;;
;; Electric tags ;;
;;;;;;;;;;;;;;;;;;;

;;;; Interactive commands and keybindings
(defun ultimate-js-electric-lt ()
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
      (progn (modify-syntax-entry ?<  "." ultimate-js-mode--syntax-table)
             (self-insert-command 1 ?<)
             (insert "/>")
             (backward-char 2)
             (modify-syntax-entry ?<  "(" ultimate-js-mode--syntax-table))
    (modify-syntax-entry ?<  "." ultimate-js-mode--syntax-table)
    (self-insert-command 1 ?<)
    (modify-syntax-entry ?<  "(" ultimate-js-mode--syntax-table)))

(defun ultimate-js-expand-self-closing-tag (text)
  "Expand NODE into a balanced tag.
Assumes NODE is self-closing `rjsx-node', and that point is at
the self-closing slash."
  (delete-char 1)
  (search-forward ">")
  (save-excursion
    (insert "</" text ">")))

(defun ultimate-js--get-self-closing-tag-name ()
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

(defun ultimate-js-electric-gt ()
  "Insert a context-sensitive greater-than sign.
If point is in a self-closing JSX tag just before the
slash, it creates a matching end-tag and places point just inside
the tags.
Taken from RJSX"
  (interactive)
  (let ((text (ultimate-js--get-self-closing-tag-name)))
    (if text
        (ultimate-js-expand-self-closing-tag text)
      (self-insert-command 1 ?>))))

(defun ultimate-js-electric-delete (arg &optional killp)
  (interactive "*p\nP")
  (if (and (looking-at "/>") (looking-back "<"))
      (progn
        (delete-char 2)
        (backward-delete-char arg killp))
    (backward-delete-char arg killp)))

(define-key ultimate-js-mode-map "<" 'ultimate-js-electric-lt)
(define-key ultimate-js-mode-map ">" 'ultimate-js-electric-gt)
(define-key ultimate-js-mode-map "\177" #'ultimate-js-electric-delete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Angle brackets as delimiters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ultimate-js-is-opening-angle-bracket (pos)
  "Check if a < at POS should be treated as an opening bracket."
  (let ((node (treesit-node-parent (treesit-node-at pos))))
    (when node
      (or (equal (treesit-node-type node) "type_arguments")
          (equal (treesit-node-type node) "type_parameters")
          (equal (treesit-node-type node) "jsx_opening_element")
          (equal (treesit-node-type node) "jsx_self_closing_element")))))

(defun ultimate-js-is-closing-angle-bracket (pos)
  "Check if a > at POS should be treated as a closing bracket."
  (let ((node (treesit-node-parent (treesit-node-at pos))))
    (when node
      (or (equal (treesit-node-type node) "type_arguments")
          (equal (treesit-node-type node) "type_parameters")
          (equal (treesit-node-type node) "jsx_closing_element")
          (equal (treesit-node-type node) "jsx_self_closing_element")))))

(defun ultimate-js-update-generic-delimiters (beg end _len)
  "Update syntax properties for `<` and `>` using Tree-sitter nodes."
  (with-silent-modifications
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (let ((end (min (point-max) end)))
        (while (re-search-forward "[<>]" end t)
          (let ((pos (match-beginning 0)))
            (if (char-equal (char-after pos) ?<)
                (put-text-property pos (1+ pos) 'syntax-table
                                   (if (ultimate-js-is-opening-angle-bracket pos)
                                       '(4 . ?>)
                                     '(1 . nil)))
              (put-text-property pos (1+ pos) 'syntax-table
                                 (if (ultimate-js-is-closing-angle-bracket pos)
                                     '(5 . ?<) '(1 . nil))))))))))

(add-hook 'ultimate-js-mode-hook
          (lambda ()
            (add-hook 'after-change-functions
                      #'ultimate-js-update-generic-delimiters
                      nil t)))

(add-hook 'ultimate-js-mode-hook
          (lambda ()
            (ultimate-js-update-generic-delimiters (point-min) (point-max) nil)))

(add-hook 'ultimate-js-mode-hook
          (lambda ()
            (setq-local parse-sexp-lookup-properties t)))


(provide 'ultimate-js-mode)
