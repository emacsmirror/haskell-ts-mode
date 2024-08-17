
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

;; TODO change to defvar
(defvar haskell-ts-font-lock
      (treesit-font-lock-rules
       :language 'haskell
       :feature 'keyword
       `(["module" "import" "data" "let" "where" "case"
	  "if" "then" "else" "of" "do" "in" "instance"]
	 @font-lock-keyword-face
	 ["(" ")" "[" "]"] @font-lock-operator-face)
       :language 'haskell
       :feature 'type
       `((type) @font-lock-type-face
	 (constructor) @font-lock-type-face)
       :language 'haskell
       :feature 'function
       `((function (variable) @font-lock-function-name-face)
	 (function (infix (operator)  @font-lock-function-name-face))
	 (bind (variable) @font-lock-function-name-face)
	 (function (infix (infix_id (variable) @font-lock-function-name-face))))
       :language 'haskell
       :feature 'args
       `((function (patterns) @font-lock-variable-name-face)
	 (function (infix (variable)  @font-lock-variable-name-face))
	 (lambda (patterns (variable) @font-lock-variable-name-face)))
       :language 'haskell
       :feature 'match
       `((match ("|" @font-lock-doc-face) ("=" @font-lock-doc-face))
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

	 ;; list
	 ((node-is "]") parent 0)
	 ((parent-is "list") parent 1)
	 
	 ;; If then else
	 ((node-is "then") parent 2)
	 ((node-is "^else$") parent 2)

	 ((node-is "^in$") parent 2)
 
	 ((parent-is "apply") parent 2)
	 
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

	 ;; Infix
	 ((node-is "infix") grand-parent 2)
	 ((parent-is "infix") parent 0)

	 ((parent-is "data_constructors") parent 0)

	 ;; where
	 ((lambda (node parent bol)
	    (string= "where" (treesit-node-type (treesit-node-prev-sibling node))))
	  (lambda (a b c)
	    (+ 1 (treesit-node-start (treesit-node-prev-sibling b))))
	  3)
	 ((parent-is "local_binds") prev-sibling 0)
	 ((node-is "^where$") parent 2)

	 (no-node prev-adaptive-prefix 0)
	 ((parent-is "haskell") column-0 0)
	 ((parent-is "declarations") column-0 0)

	 ((parent-is "record") grand-parent 0)
	 
	 ;; Backup
	 (catch-all parent 2)
	 )))

;; Copied from haskell-mode
(setq haskell-ts-mode-syntax-table
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
	      (list (cons ?` ?`) (cons ?( ?)) (cons ?{ ?}) (cons ?" ?")))
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
	      '(( comment  str pragma)
		(type definition )
		(args function match keyword)))
  (treesit-major-mode-setup))

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
