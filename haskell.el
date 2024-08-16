
(require 'treesit)
(require 'comint)
(declare-function treesit-parser-create "treesit.c")

;; TODO change to defvar
(setq haskell-ts-font-lock
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

;; TODO change to defvar
(setq haskell-ts-indent-rules
	  `((haskell
		 ((node-is "comment") column-0 0)
		 ((parent-is "haskell") column-0 0)
		 ((parent-is "declarations") column-0 0)
		 ((parent-is "imports") column-0 0)
		 ((parent-is "local_binds") prev-sibling 0)
		 ((parent-is "apply") parent 2)
		 ;; Match
		 ((match "match" nil nil 2 2) parent 2)
		 ((node-is "match") prev-sibling 0)
		 ;; Do Hg
		 ((parent-is "do") prev-sibling 0)
		 ((match nil "do" nil 1 1) great-grand-parent 2)
		 ((node-is "alternatives") grand-parent 0)
		 ((parent-is "alternatives") grand-parent 2)

		 ;; Infix
		 ((node-is "infix") grand-parent 2)
		 ((parent-is "infix") parent 0)
		 ;; Where PS  TODO 2nd
		 
		 ((lambda (node parent bol)
			(string= "where" (treesit-node-type (treesit-node-prev-sibling node))))
		  (lambda (a b c)
		   (+ 1 (treesit-node-start (treesit-node-prev-sibling b))))
		  3)
		 ((parent-is "local_binds") prev-sibling 2)
		 ((node-is "^where$") parent 2)
		 ;; If statement
		 ((node-is "then") parent 2)
		 ((node-is "else") parent 2)

		 ;; lists
		 ((node-is "^in$") parent 2)
		 ((lambda (a b c)
			(save-excursion
			  (goto-char c)
			  (re-search-forward "^[ \t]*$" (line-end-position) t)))
		  prev-adaptive-prefix 0))))


;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "haskell ts mode"
  "Mjaor mode for Haskell files using tree-sitter"
  :group 'haskell
  (unless (treesit-ready-p 'haskell)
	(error "Tree-sitter for Haskell is not available"))
  (treesit-parser-create 'haskell)
  (setq-local treesit-defun-type-regexp "\\(?:\\(?:function\\|struct\\)_definition\\)")
  ;; Indent 
  (setq-local treesit-simple-indent-rules haskell-ts-indent-rules)
  ;; Misc
  (setq-local comment-start "--")
  (setq-local indent-tabs-mode nil)
  (setq-local electric-pair-pairs
			  (list (cons ?` ?`) (cons ?( ?)) (cons ?{ ?}) (cons ?' ?') (cons ?" ?")))
  (setq-local treesit-defun-name-function 'haskell-ts-defun-name)
  (setq-local treesit-defun-type-regexp "function")
  ;; Imenu
  (setq-local treesit-simple-imenu-settings
			  `((nil haskell-ts-imenu-func-node-p nil haskell-ts-imenu-name-function)
				("Signatures.." haskell-ts-imenu-sig-node-p nil haskell-ts-imenu-sig-name-function)))
  ;; font-lock.
  (setq-local treesit-font-lock-settings haskell-ts-font-lock)
  (setq-local treesit-font-lock-feature-list
			  '(( comment  str pragma type keyword definition function args match)))
  (treesit-major-mode-setup))

(defun haskell-ts-imenu-func-node-p (node)
  (and (string-match-p "function\\|bind" (treesit-node-type node))
	   (string= (treesit-node-type (treesit-node-parent node)) "declarations")))

(defun haskell-ts-imenu-sig-node-p (node)
  (and (string-match-p "signature" (treesit-node-type node))
	   (string= (treesit-node-type (treesit-node-parent node)) "declarations")))

(defun haskell-ts-imenu-sig-name-function (node)
  (let ((name (treesit-node-text node)))
	(if (haskell-ts-imenu-sig-node-p node)
		(haskell-ts-defun-name node)
	  nil)))

(defun haskell-ts-imenu-name-function (node)
  (let ((name (treesit-node-text node)))
	(if (haskell-ts-imenu-func-node-p node)
		(haskell-ts-defun-name node)
	  nil)))

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

;; (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))
