;;; haskell-ts-mode.el --- A treesit based major mode for haskell -*- lexical-binding:t -*-

;; Copyright (C) 2024  Pranshu Sharma


;; Author: Pranshu Sharma <pranshusharma366 at gmail>
;; URL: https://codeberg.org/pranshu/haskell-ts-mode
;; Keywords: tree-sitter, haskell, emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a WIP mode that uses treesitter to provide all the basic
;; major mode stuff, like indentation, font lock, etc...

;;; Code:

(eval-when-compile
 (require 'treesit)
 (require 'comint))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defvar haskell-ts-font-lock-feature-list
  '((comment str pragma parens)
    (type definition function args)
    (match keyword)
    (otherwise)))

;; TODO change to defvar
(defvar haskell-ts-font-lock
      (treesit-font-lock-rules
       :language 'haskell
       :feature 'parens
       `(["(" ")" "[" "]"] @font-lock-operator-face
       (infix operator: (_) @font-lock-operator-face))
       :language 'haskell
       :feature 'keyword
       `(["module" "import" "data" "let" "where" "case"
	  "if" "then" "else" "of" "do" "in" "instance"]
	 @font-lock-keyword-face)
       :language 'haskell
       :feature 'otherwise
       :override t
       `(((match (guards guard: (boolean (variable) @font-lock-keyword-face)))
	 (:match "otherwise" @font-lock-keyword-face)))
       :language 'haskell
       :feature 'args
       :override 'keep
       (concat
	"(function (infix left_operand: (_) @haskell-ts-fontify-arg))"
	"(function (infix right_operand: (_) @haskell-ts-fontify-arg))"
	"(generator . (_) @haskell-ts-fontify-arg)"
	"(bind (as (variable) . (_) @haskell-ts-fontify-arg))"
        "(patterns) @haskell-ts-fontify-arg"
	)
       :language 'haskell
       :feature 'type
       `((type) @font-lock-type-face
	 (constructor) @font-lock-type-face)
       :language 'haskell
       :feature 'function
       :override t
       `((function (variable) @font-lock-function-name-face)
	 (function (infix (operator)  @font-lock-function-name-face))
	 (bind (variable) @font-lock-function-name-face)
	 (function (infix (infix_id (variable) @font-lock-function-name-face)))
	 (bind (as (variable) @font-lock-function-name-face)))
       :language 'haskell
       :feature 'match
       `((match ("|" @font-lock-doc-face) ("=" @font-lock-doc-face))
	 (list_comprehension ("|" @font-lock-doc-face
			      (qualifiers (generator "<-" @font-lock-doc-face))))
	 (match ("->" @font-lock-doc-face)))
       :language 'haskell
       :feature 'comment
       `(((comment) @font-lock-comment-face))
       :language 'haskell
       :feature 'pragma
       `((pragma) @font-lock-preprocessor-face)
       :language 'haskell
       :feature 'str
       `((char) @font-lock-string-face
	 (string) @font-lock-string-face)))

(defvar haskell-ts-indent-rules
      `((haskell
	 ((node-is "comment") column-0 0)
	 ((parent-is "imports") column-0 0)

	 ;; Infix
	 ((parent-is "infix") parent 0)
	 ((node-is "infix") grand-parent 2)
	 
	 ;; list
	 ((node-is "]") parent 0)
	 ((parent-is "list") parent 1)
	 
	 ;; If then else
	 ((node-is "then") parent 2)
	 ((node-is "^else$") parent 2)

	 ((node-is "^in$") parent 2)
 
	 ((parent-is "apply") parent -1)
	 
	 ;; Match
	 ((match "match" nil nil 2 2) parent 2)
	 ((node-is "match") prev-sibling 0)
	 ((parent-is "match") grand-parent 2)

	 ;; Do Hg
	 ((lambda (node parent bol)
	    (string= "do" (treesit-node-type (treesit-node-prev-sibling node))))
	  grand-parent 0)
	 ((parent-is "do") prev-sibling 0)

	 ((node-is "alternatives") grand-parent 0)
	 ((parent-is "alternatives") grand-parent 2)

	 (no-node prev-adaptive-prefix 0)
	 
	 ((parent-is "data_constructors") parent 0)

	 ;; where
	 ((lambda (node parent bol)
	    (string= "where" (treesit-node-type (treesit-node-prev-sibling node))))
	  (lambda (a b c)
	    (+ 1 (treesit-node-start (treesit-node-prev-sibling b))))
	  3)
	 ((parent-is "local_binds") prev-sibling 0)
	 ((node-is "^where$") parent 2)

	 ((parent-is "haskell") column-0 0)
	 ((parent-is "declarations") column-0 0)

	 ((parent-is "record") grand-parent 0)
	 
	 ;; Backup
	 (catch-all parent 2)
	 )))

;; Copied from haskell-mode
(defvar haskell-ts-mode-syntax-table
    (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  "< 123" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?\` "$`" table)

    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "!#$%&*+./:<=>?@^|~,;\\")
    table))


;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "haskell ts mode"
  "major mode for Haskell files using tree-sitter"
  :syntax-table haskell-ts-mode-syntax-table
  :interactive t
  (unless (treesit-ready-p 'haskell)
    (error "Tree-sitter for Haskell is not available"))
  (treesit-parser-create 'haskell)
  (setq-local treesit-defun-type-regexp "\\(?:\\(?:function\\|struct\\)_definition\\)")
  ;; Indent 
  (setq-local treesit-simple-indent-rules haskell-ts-indent-rules)
  ;; Misc
  (setq-local comment-start "{- ")
  (setq-local comment-end "-}")
  (setq-local comment-start-skip "\\(?:--+\\|{-+\\) *")
  (setq-local comment-end-skip "[ \t]*--+}")
  (setq-local indent-tabs-mode nil)
  (setq-local electric-pair-pairs
	      (list (cons ?` ?`) (cons ?( ?)) (cons ?{ ?}) (cons ?" ?") (cons ?[ ?])))
  (setq-local treesit-defun-name-function 'haskell-ts-defun-name)
  (setq-local treesit-defun-type-regexp "function")
  ;; Imenu
  (setq-local treesit-simple-imenu-settings
	      `((nil haskell-ts-imenu-func-node-p nil
		     ,(haskell-ts-imenu-name-function 'haskell-ts-imenu-func-node-p))
		("Signatures.." haskell-ts-imenu-sig-node-p nil
		 ,(haskell-ts-imenu-name-function 'haskell-ts-imenu-sig-node-p))
		("Data..." haskell_ts-imenu-data-type-p nil
		 (lambda (node)
		   (treesit-node-text (treesit-node-child node 1))))))
  ;; font-lock.
  (setq-local treesit-font-lock-settings haskell-ts-font-lock)
  (setq-local treesit-font-lock-feature-list	
	      haskell-ts-font-lock-feature-list)
  (treesit-major-mode-setup))

(defun haskell-ts-fontify-arg (node &optional override start end)
  (if (string= "variable" (treesit-node-type node))
      (put-text-property
       (treesit-node-start node)
       (treesit-node-end node)
       'face font-lock-variable-name-face)
    (mapc 'haskell-ts-fontify-arg (treesit-node-children node))))

(defun haskell-ts-imenu-node-p (regex node)
    (and (string-match-p regex (treesit-node-type node))
	 (string= (treesit-node-type (treesit-node-parent node)) "declarations")))

(defmacro haskell-ts-imenu-name-function (check-func)
  `(lambda (node)
    (let ((name (treesit-node-text node)))
      (if (funcall ,check-func node)
	  (haskell-ts-defun-name node)
	nil))))

(defun haskell-ts-imenu-func-node-p (node)
  (haskell-ts-imenu-node-p "function\\|bind" node))

(defun haskell-ts-imenu-sig-node-p (node)
  (haskell-ts-imenu-node-p "signature" node))

(defun haskell_ts-imenu-data-type-p (node)
  (haskell-ts-imenu-node-p "data_type" node))

(defun haskell-ts-defun-name (node)
  (treesit-node-text (treesit-node-child node 0)))

(defun haskell-compile-region-and-go (start end)
  "compile the current region in the haskell proc, and switch to its buffer."
  (interactive "r")
  (comint-send-region (haskellsession) start end)
  (comint-send-string (haskellsession) "\n"))

(defun run-haskell()
  (interactive)
  (when (not (comint-check-proc "*haskell*"))
    (set-buffer (apply (function make-comint)
		       "haskell" "ghci" nil `(,buffer-file-name))))
  (pop-to-buffer-same-window "*haskell*"))

(defun haskellsession ()
  (get-buffer-process "*haskell*"))

(define-key haskell-ts-mode-map (kbd "C-c c") 'haskell-compile-region-and-go)
(define-key haskell-ts-mode-map (kbd "C-c r") 'run-haskell)

(when (treesit-ready-p 'haskell)
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode)))

(provide 'haskell-ts-mode)
