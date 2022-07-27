;;; ultimate-js-mode.el --- Major mode for editing JS files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Guillaume Brunerie

;; Author: Guillaume Brunerie <guillaume.brunerie@gmail.com>
;; URL: https://github.com/guillaumebrunerie/ultimate-js-mode
;; Package-Requires: (tree-sitter (tsc "0.16.1"))

;; See README.md for more information.

(require 'js)
(require 'json)
(require 'tsc)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'font-lock)

(defconst ultimate-js-mode--loaddir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that ultimate-js-mode was loaded from.")

;; Required for JSX indentation from js-mode to work
(defvar ultimate-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `ultimate-js-mode'.")

;;;###autoload
(define-derived-mode ultimate-js-mode prog-mode "UltimateJS"
  :group 'ultimate-js
  "Major mode for editing JS/JSX/TS/TSX files."

  ;; The name of the tree-sitter grammar, `javascript` for both .js and .jsx,
  ;; `typescript` for .ts, and `tsx` for .tsx
  (setq-local ultimate-js--lang 'javascript)
  ;; Whether or not we are in jsx/tsx mode
  (setq-local ultimate-js--is-jsx-tsx nil)
  ;; Determine the language based on the file extension
  (let* ((extension (file-name-extension buffer-file-name)))
	(cond
	 ((string= extension "jsx") (setq mode-name "UltimateJSX") (setq ultimate-js--is-jsx-tsx t))
	 ((string= extension "ts") (setq mode-name "UltimateTS") (setq ultimate-js--lang 'typescript))
	 ((string= extension "tsx") (setq mode-name "UltimateTSX") (setq ultimate-js--lang 'tsx) (setq ultimate-js--is-jsx-tsx t))))

  (setq-local comment-start "// ")

  ;; Indentation from js-mode, see later for Typescript
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (add-hook 'syntax-propertize-extend-region-functions
            #'js--syntax-propertize-extend-region 'append 'local)
  (setq-local indent-line-function
              #'js-indent-line)
  (when ultimate-js--is-jsx-tsx
	(setq-local js-jsx-syntax t))
  
  ;; Highlighting from tree-sitter
  (setq-local
   tree-sitter-language
   (tree-sitter-load
	ultimate-js--lang
	(concat ultimate-js-mode--loaddir "libs/" (symbol-name ultimate-js--lang))))
  (setq-local tree-sitter-hl-default-patterns (ultimate-js-mode--get-highlights-queries ultimate-js--lang))
  (setq-local jit-lock-defer-time 0)
  (setq-local font-lock-defaults '(nil t))
  (tree-sitter-hl-mode))

;; Helper function to read a file to a string
(defun ultimate-js-mode--read-file (file-name)
  (with-temp-buffer
    (insert-file-contents (concat ultimate-js-mode--loaddir file-name))
    (buffer-string)))

;; Helper function to return the highlighting queries for a given language.
(defun ultimate-js-mode--get-highlights-queries (lang)
  (let* ((source-langs (pcase lang
						 ('javascript '("jsx" "js-params" "js"))
						 ('typescript '("ts" "js"))
						 ('tsx '("ts" "jsx" "js"))))
		 (sources (mapcar (lambda (source) (concat "queries/highlights-" source ".scm")) source-langs)))
	(mapconcat #'ultimate-js-mode--read-file sources "\n")))

;; Old version, using package.json upstream
;;
;; The highlighting queries from upstream are a bit complicated to get. They
;; consist of several files that need to be concatenated, and the list of those
;; files is given in the `package.json` file. It is done here in a bit of a
;; hacky way. Maybe I should have forked tree-sitter-langs instead
;;
;; (defun ultimate-js-mode--get-highlights-queries (lang)
;;   (let* ((package-json (ultimate-js-mode--read-file (if (eq lang 'javascript) "tree-sitter-javascript/package.json" "tree-sitter-typescript/package.json")))
;;          (sources (gethash "highlights" (car (gethash "tree-sitter" (json-parse-string package-json :array-type 'list)))))
;; 		 (sources (if (eq lang 'tsx) (gethash "highlights" (cadr (gethash "tree-sitter" (json-parse-string package-json :array-type 'list)))) sources))
;;          (sources (mapcar (lambda (source) (concat "tree-sitter-" (if (eq lang 'javascript) "javascript/" "typescript/") source)) sources)))
;;     (mapconcat #'ultimate-js-mode--read-file sources "\n")))


;; Indentation fix for TS/TSX
;;
;; js-mode has a pretty complex system using syntax properties to correctly
;; parse and indent JSX (JSX tags are potentially ambiguous with respect to
;; comparisons, and JS and JSX can be nested into each other arbitrarily deep).
;; The js-jsx--syntax-propertize-tag function is responsible for setting up
;; those syntax properties.
;;
;; But this creates problems in Typescript as generics are parsed as JSX tags
;; and indentation breaks down. So we fix this problem by using information from
;; tree-sitter to determine whether we are in a type, and only add the syntax
;; properties if we aren’t.
;;
;; Another solution would be to use indentation from typescript-mode, but
;; adapting that to TSX seems significantly harder than adapting indentation
;; from js-mode to Typescript.
(defun ultimate-js-mode--tsx-fix (orig-fun &rest args)
  (let* ((type (tsc-node-type (tsc-get-parent (tree-sitter-node-at-point)))))
	(unless (or (eq type 'type_arguments) (eq type 'predefined_type) (eq type 'type_parameter))
	  (apply orig-fun args))))
(advice-add 'js-jsx--syntax-propertize-tag :around #'ultimate-js-mode--tsx-fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax-aware parenthesis electricity ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ultimate-js--no-electric-before "[],;)}\n]")

(defun ultimate-js-mode--highest-node-in-line-at-position (position)
  "Get the highest node starting at the position and ending on the same line"
  (let* ((current-node (tree-sitter-node-at-pos nil position)))
    (while (and
            current-node
            (when-let ((parent-node (tsc-get-parent current-node)))
              (when (and ;; parent and current share same position
                     (eq (tsc-node-start-position parent-node)
                         (tsc-node-start-position current-node))
					 (eq (line-number-at-pos (tsc-node-start-position parent-node))
						 (line-number-at-pos (tsc-node-end-position parent-node))))
				;; move upwards to the parent node
				(setq current-node parent-node)))))
    current-node))

;;;; Interactive commands and keybindings
(defun ultimate-js-electric-open (open-char close-char &optional basic)
  "Insert a pair of parentheses around the largest node starting at the
point and contained in the same line. If basic, only insert a pair of
parentheses."
  (lambda ()
    (interactive)
	(if (region-active-p)
		(save-excursion
		  (let* ((start (region-beginning))
				 (end (region-end)))
			(goto-char end)
			(insert close-char)
			(goto-char start)
			(insert open-char)))
      (let* ((node (ultimate-js-mode--highest-node-in-line-at-position (point)))
			 (large-node
              (and (not basic)
                   (>= (tsc-node-start-position node) (point))
                   (<= (tsc-node-start-position (tsc-get-parent node)) (point))
                   (not (string-match-p ultimate-js--no-electric-before (tsc-node-text node)))))
			 (size (if large-node
                       (- (tsc-node-end-position node)
                          (point))
					 0)))
		(self-insert-command 1 open-char)
		(forward-char size)
		(insert close-char)
		(backward-char (+ 1 size))))))

(defun ultimate-js-electric-close (open-char close-char)
  "Insert a closing character, unless there is already one."
  (lambda ()
    (interactive)
    (if (eq close-char (char-after))
        (forward-char 1)
        (self-insert-command 1 close-char))))

(defun ultimate-js-electric-open-close (char)
  "Insert two such character, unless there is already one."
  (lambda ()
    (interactive)
	(if (region-active-p)
		(save-excursion
		  (let* ((start (region-beginning))
				 (end (region-end)))
			(goto-char end)
			(insert char)
			(goto-char start)
			(insert char)))
      (if (eq char (char-after))
          (forward-char 1)
		(self-insert-command 1 char)
		(insert char)
		(backward-char 1)))))

(defun ultimate-js--electric-newline-between-parens (open-char close-char)
  "If the point is right between the open and close chars, insert a new line and
another one after the point, and indent them."
  (when (and (eq (char-before) open-char)
             (eq (char-after) close-char))
    (newline 2)
    (funcall indent-line-function)
    (forward-line -1)
    (funcall indent-line-function)
    (back-to-indentation)
    t))

(defun ultimate-js-electric-newline ()
  "Insert a new line, and potentially a new one plus indentation if we are
between parentheses."
  (interactive)
  (cond ((ultimate-js--electric-newline-between-parens ?( ?)))
        ((ultimate-js--electric-newline-between-parens ?[ ?]))
        ((ultimate-js--electric-newline-between-parens ?{ ?}))
        ((ultimate-js--electric-newline-between-parens ?> ?<))
        (t (newline 1 t))))

(defun ultimate-js--electric-backspace-between-parens (open-char close-char)
  "If the previous character is an opening parenthesis, remove it and the
corresponding closing parenthesis"
  (when (eq (char-before) open-char)
    (ignore-errors
      (let ((original-line (line-number-at-pos))
            (new-line (save-excursion
                        (backward-char 1)
                        (forward-sexp)
                        (line-number-at-pos))))
        (when (eq original-line new-line)
          (save-excursion
            (backward-char 1)
            (forward-sexp)
            (delete-char -1)))))))

(defun ultimate-js--electric-backspace-between-indented-parens (open-char close-char)
  "If we are in the middle of whitespace between parenteses, remove the
whitespace"
  (let* ((position-before (save-excursion
                            (skip-chars-backward " \n\t")
                            (when (eq (char-before) open-char)
                              (point))))
         (position-after (save-excursion
                           (skip-chars-forward " \n\t")
                           (when (eq (char-after) close-char)
                             (point)))))
    (when (and
           position-before
           position-after
           (eq (- (line-number-at-pos position-after) (line-number-at-pos position-before)) 2))
      (goto-char position-before)
      (delete-char (- position-after position-before))
      t)))

;; The guiding principle is that backspace right after a key that usually
;; inserts a character, should undo whatever it did
(defun ultimate-js-electric-backspace ()
  "Delete the region if it is active, or a pair of parentheses, or whitespace
between parentheses, or the previous character."
  (interactive)
  (cond ((use-region-p) (delete-active-region))
        ((ultimate-js--electric-backspace-between-parens ?( ?)))
        ((ultimate-js--electric-backspace-between-parens ?[ ?]))
        ((ultimate-js--electric-backspace-between-parens ?{ ?}))
        ((ultimate-js--electric-backspace-between-parens ?\" ?\"))
        ((ultimate-js--electric-backspace-between-parens ?' ?'))
        ((ultimate-js--electric-backspace-between-parens ?` ?`))
        ((ultimate-js--electric-backspace-between-indented-parens ?( ?)))
        ((ultimate-js--electric-backspace-between-indented-parens ?[ ?]))
        ((ultimate-js--electric-backspace-between-indented-parens ?{ ?}))
        (t (delete-char -1))))

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
        (progn (self-insert-command 1 ?<)
               (insert "/>")
               (backward-char 2))
      (self-insert-command 1 ?<)))

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
    (let* ((node (tree-sitter-node-at-point))
           (parent (and node (tsc-get-parent node))))
      (when (and node
                 parent
                 (string= (tsc-node-type node) "/>")
                 (eq (tsc-node-type parent) 'jsx_self_closing_element))
        (tsc-node-text (tsc-get-nth-child parent 1))))))

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

(define-key ultimate-js-mode-map "{" (ultimate-js-electric-open ?\{ ?\} t))
(define-key ultimate-js-mode-map "(" (ultimate-js-electric-open ?\( ?\)))
(define-key ultimate-js-mode-map "[" (ultimate-js-electric-open ?\[ ?\]))

(define-key ultimate-js-mode-map "}" (ultimate-js-electric-close ?\{ ?\}))
(define-key ultimate-js-mode-map ")" (ultimate-js-electric-close ?\( ?\)))
(define-key ultimate-js-mode-map "]" (ultimate-js-electric-close ?\[ ?\]))

(define-key ultimate-js-mode-map "\"" (ultimate-js-electric-open-close ?\"))
(define-key ultimate-js-mode-map "'"  (ultimate-js-electric-open-close ?'))
(define-key ultimate-js-mode-map "`"  (ultimate-js-electric-open-close ?`))

(define-key ultimate-js-mode-map "" 'ultimate-js-electric-newline)

(define-key ultimate-js-mode-map "" 'ultimate-js-electric-backspace)

(define-key ultimate-js-mode-map "<" 'ultimate-js-electric-lt)
(define-key ultimate-js-mode-map ">" 'ultimate-js-electric-gt)

;; ;; Moving between references, not quite working yet

;; (defun ultimate-js-find-next-reference (&optional include-declaration)
;;   "Find next reference of the symbol at point, in the same file.
;; Adapted from lsp-ui-find-next-reference, which goes across files."
;;   (interactive)
;;   (let* ((cur (list buffer-file-name (1- (line-number-at-pos)) (- (point) (line-beginning-position))))
;;          (refs (lsp-ui--reference-triples include-declaration))
;;          (idx -1)
;;          (refs (seq-filter (lambda (ref) (string= (car ref) buffer-file-name)) refs))
;;          (res (-first (lambda (ref) (cl-incf idx) (lsp-ui--location< cur ref)) refs)))
;;     (if refs
;;       (if res
;;           (progn
;;             (goto-char 1)
;;             (forward-line (cadr res))
;;             (forward-char (caddr res))
;;             (cons idx (length refs)))
;;         (cons 0 0))
;;       (forward-paragraph))))

;; (defun ultimate-js-find-prev-reference (&optional include-declaration)
;;   "Find previous reference of the symbol at point, in the same file.
;; Adapted from lsp-ui-find-prev-reference, which goes across files."
;;   (interactive)
;;   (let* ((cur (list buffer-file-name (1- (line-number-at-pos)) (- (point) (line-beginning-position))))
;;          (refs (lsp-ui--reference-triples include-declaration))
;;          (idx -1)
;;          (refs (seq-filter (lambda (ref) (string= (car ref) buffer-file-name)) refs))
;;          (res (-last (lambda (ref) (and (lsp-ui--location< ref cur) (cl-incf idx))) refs)))
;;     (if refs
;;         (if res
;;             (progn
;;               (goto-char 1)
;;               (forward-line (cadr res))
;;               (forward-char (caddr res))
;;               (cons idx (length refs)))
;;           (cons 0 0))
;;       (backward-paragraph))))

;; (define-key ultimate-js-mode-map (kbd "C-<down>")  #'ultimate-js-find-next-reference)
;; (define-key ultimate-js-mode-map (kbd "C-<up>")    #'ultimate-js-find-prev-reference)

(provide 'ultimate-js-mode)
