
(require 'treesit)
(require 'comint)
(declare-function treesit-parser-create "treesit.c")

(setq haskell-ts-font-lock
	  (treesit-font-lock-rules
	   :language 'haskell
	   :feature 'keyword
	   `(["module" "import" "data" "let" "where" "case"
		  "if" "then" "else" "of" "do" "in" ]
		 @font-lock-keyword-face)
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
	   `((match ("|" @font-lock-doc-face))
		 (match  ("=" @font-lock-doc-face))
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
;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "haskell ts mode"
  "Major mode for Haskell files using tree-sitter"
  :group 'haskell
  (unless (treesit-ready-p 'haskell)
	(error "Tree-sitter for Haskell is not available"))
  (treesit-parser-create 'haskell)
  (setq-local treesit-defun-type-regexp "\\(?:\\(?:function\\|struct\\)_definition\\)")
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

(add-to-list ' auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))
