;; -*- lexical-binding: t -*-

(require 'pcomplete)

(defun ghci-match-partial-command ()
  "Return the command name at point, or nil if none is found."
  (save-excursion
    (comint-bol nil)
    (when (looking-at " *\\(:[a-z]+\\)$")
      (match-string-no-properties 1))))

(defun prefix-p (string1 string2)
  "Is STRING1 a prefix of STRING2?"
  (and (<= (length string1)
           (length string2))
       (string-equal string1
                     (substring string2 0 (length string1)))))

(defun ghci-command-completion ()
  "Return the completion data for the command at point, if any."
  (let ((command (ghci-match-partial-command)))
    (when command
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (completions
             (remove-if-not
              (lambda (candidate)
                (prefix-p command candidate))
              ghci-commands)))
        (list
         beg end
         (lambda (string pred action)
           (complete-with-action action completions string pred))
         :exit-function
         (lambda (_string finished)
           (when (memq finished '(sole finished))
             (if (looking-at " ")
                 (goto-char (match-end 0))
               (insert " ")))))))))

(defconst ghci-commands
  '(":add"
    ":browse" ":browse!"
    ":cd"
    ":cmd"
    ":ctags" ":ctags!"
    ":def"
    ":edit"
    ":etags"
    ":help"
    ":info"
    ":kind"
    ":load"
    ":main"
    ":module"
    ":quit"
    ":reload"
    ":run"
    ":type"
    ":undef"
    ":abandon"
    ":back"
    ":break"
    ":continue"
    ":delete"
    ":force"
    ":forward"
    ":history"
    ":list"
    ":print"
    ":sprint"
    ":step"
    ":steplocal"
    ":stepmodule"
    ":trace"
    ":set"
    ":unset"
    ":show")
  "Commands available from the GHCi prompt.")

(defvar exposed-modules nil
  "The list of exposed modules.")

(defun exposed-modules ()
  "Return the list of exposed modules from the registered
packages in both the global and user databases."
  (with-temp-buffer
    (call-process "ghc-pkg" nil (current-buffer) nil "dump")
    (goto-char (point-min))
    (loop while (re-search-forward
                 (concat "exposed: True\n"
                         "exposed-modules:"
                         "\\(\\(?:.*\n?\\)*?\\)"
                         "hidden-modules")
                 nil t)
          nconc (split-string (match-string 1) "[\s\n]+" t))))

(defun update-exposed-modules ()
  (setq exposed-modules (exposed-modules)))

(defun pcomplete/:add ()
  (while (pcomplete-here* (pcomplete-entries))))

(defun pcomplete/:browse ()
  (pcomplete-here* exposed-modules))

(fset 'pcomplete/:browse! 'pcomplete/:browse)

(defun pcomplete/:cd ()
  (pcomplete-here* (pcomplete-dirs)))

(defun pcomplete/:edit ()
  (pcomplete-here* (pcomplete-entries)))

(fset 'pcomplete/:e 'pcomplete/:edit)

(defun pcomplete/:load ()
  (while (pcomplete-here* (pcomplete-entries))))

(fset 'pcomplete/:l 'pcomplete/:load)

(defun pcomplete/:module ()
  (while (pcomplete-here* exposed-modules)))

(defun language-options ()
  (mapcar (lambda (extension)
            (concat "-X" extension))
          (split-string
           (shell-command-to-string "ghc --supported-extensions")
           "\n")))

(defvar language-options nil)

(defun update-language-options ()
  (setq language-options (language-options)))

(defconst warning-options
  '("-w" "-W" "-Wall" "-Wwarn" "-Werror"
    "-fwarn-unrecognised-pragmas"
    "-fno-warn-unrecognised-pragmas"
    "-fwarn-warnings-deprecations"
    "-fno-warn-warnings-deprecations"
    "-fwarn-deprecated-flags"
    "-fno-warn-deprecated-flags"
    "-fwarn-duplicate-exports"
    "-fno-warn-duplicate-exports"
    "-fwarn-hi-shadowing"
    "-fno-warn-hi-shadowing"
    "-fwarn-implicit-prelude"
    "-fno-warn-implicit-prelude"
    "-fwarn-incomplete-patterns"
    "-fno-warn-incomplete-patterns"
    "-fwarn-incomplete-record-updates"
    "-fno-warn-incomplete-record-updates"
    "-fwarn-lazy-unlifted-bindings"
    "-fno-warn-lazy-unlifted-bindings"
    "-fwarn-missing-fields"
    "-fno-warn-missing-fields"
    "-fwarn-missing-import-lists"
    "-fnowarn-missing-import-lists"
    "-fwarn-missing-methods"
    "-fno-warn-missing-methods"
    "-fwarn-missing-signatures"
    "-fno-warn-missing-signatures"
    "-fwarn-name-shadowing"
    "-fno-warn-name-shadowing"
    "-fwarn-orphans"
    "-fno-warn-orphans"
    "-fwarn-overlapping-patterns"
    "-fno-warn-overlapping-patterns"
    "-fwarn-tabs"
    "-fno-warn-tabs"
    "-fwarn-type-defaults"
    "-fno-warn-type-defaults"
    "-fwarn-monomorphism-restriction"
    "-fno-warn-monomorphism-restriction"
    "-fwarn-unused-binds"
    "-fno-warn-unused-binds"
    "-fwarn-unused-imports"
    "-fno-warn-unused-imports"
    "-fwarn-unused-matches"
    "-fno-warn-unused-matches"
    "-fwarn-unused-do-bind"
    "-fno-warn-unused-do-bind"
    "-fwarn-wrong-do-bind"
    "-fno-warn-wrong-do-bind"))

(defconst debugging-options
  '("-dcore-lint"
    "-ddump-asm"
    "-ddump-bcos"
    "-ddump-cmm"
    "-ddump-cpranal"
    "-ddump-cse"
    "-ddump-deriv"
    "-ddump-ds"
    "-ddump-flatC"
    "-ddump-foreign"
    "-ddump-hpc"
    "-ddump-inlinings"
    "-ddump-llvm"
    "-ddump-occur-anal"
    "-ddump-opt-cmm"
    "-ddump-parsed"
    "-ddump-prep"
    "-ddump-rn"
    "-ddump-rules"
    "-ddump-simpl"
    "-ddump-simpl-phases"
    "-ddump-simpl-iterations"
    "-ddump-spec"
    "-ddump-splices"
    "-ddump-stg"
    "-ddump-stranal"
    "-ddump-tc"
    "-ddump-types"
    "-ddump-worker-wrapper"
    "-ddump-if-trace"
    "-ddump-tc-trace"
    "-ddump-rn-trace"
    "-ddump-rn-stats"
    "-ddump-simpl-stats"
    "-dno-debug-output"
    "-dppr-debug"
    "-dsuppress-uniques"
    "-dsuppress-coercions"
    "-dsuppress-module-prefixes"
    "-dppr-noprags"
    "-dppr-user-length"
    "-dsource-stats"
    "-dcmm-lint"
    "-dstg-lint"
    "-dstg-stats"
    "-dverbose-core2core"
    "-dverbose-stg2stg"
    "-dshow-passes"
    "-dfaststring-stats"))

(defun ghci-set/unset-options ()
  (append language-options
          warning-options
          debugging-options
          '("+r" "+s" "+t")))

(defun pcomplete/:set ()
  (while (pcomplete-here*
          (append (ghci-set/unset-options)
                  '("args"
                    "prog"
                    "prompt"
                    "editor"
                    "stop")))))

(fset 'pcomplete/:s 'pcomplete/:set)

(defun pcomplete/:unset ()
  (while (pcomplete-here* (ghci-set/unset-options))))

(defconst ghci-show-commands
  '("bindings"
    "breaks"
    "contexts"
    "modules"
    "packages"
    "languages"
    "args"
    "prog"
    "prompt"
    "editor"
    "stop")
  "GHCi commands for displaying information.")

(defun pcomplete/:show ()
  (pcomplete-here* ghci-show-commands))

(fset 'pcomplete/:m 'pcomplete/:module)

(defvar ghci-completion-mode nil
  "Non-nil if GHCi completion mode is in effect.")
(make-variable-buffer-local 'ghci-completion-mode)

(defvar ghci-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'completion-at-point)
    map))

(defun turn-on-ghci-completion ()
  (setq ghci-completion-mode t)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-parse-comint-arguments)
  (add-hook 'comint-dynamic-complete-functions
            'pcomplete-completions-at-point nil 'local)
  (add-hook 'comint-dynamic-complete-functions
            'ghci-command-completion nil 'local)
  (update-exposed-modules)
  (let ((map (current-local-map)))
    (while (and map (not (eq map ghci-completion-map)))
      (setq map (keymap-parent map)))
    (unless map
      (set-keymap-parent ghci-completion-map (current-local-map))
      (setq map (make-sparse-keymap))
      (set-keymap-parent map ghci-completion-map)
      (use-local-map map))))

(defun turn-off-ghci-completion ()
  (remove-hook 'comint-dynamic-complete-functions
               'pcomplete-completions-at-point 'local)
  (remove-hook 'comint-dynamic-complete-functions
               'ghci-command-completion 'local)
  (let ((map (current-local-map)))
    (while map
      (let ((parent (keymap-parent map)))
        (if (eq ghci-completion-map parent)
            (set-keymap-parent map (keymap-parent parent))
          (setq map parent)))))
  (setq ghci-completion-mode nil))

(or (assq 'ghci-completion-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((ghci-completion-mode " GHCi-Completion")))))

(defun ghci-completion-mode (&optional arg)
  (interactive "P")
  (setq ghci-completion-mode
        (if (null arg)
            (not ghci-completion-mode)
          (> (prefix-numeric-value arg) 0)))
  (if ghci-completion-mode
      (turn-on-ghci-completion)
    (turn-off-ghci-completion)))

(provide 'ghci-completion)
