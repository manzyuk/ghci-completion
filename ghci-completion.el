;; -*- lexical-binding: t -*-

(require 'pcomplete)

(defun exposed-modules ()
  (with-temp-buffer
    (call-process "ghc-pkg" nil (current-buffer) nil "dump")
    (goto-char (point-min))
    (sort (loop while (re-search-forward
                       (concat "exposed: True\n"
                               "exposed-modules:"
                               "\\(\\(?:.*\n?\\)*?\\)"
                               "hidden-modules")
                       nil t)
                nconc (split-string (match-string 1) "[\s\n]+" t))
          'string<)))

(defun ghci-match-partial-command ()
  (save-excursion
    (comint-bol)
    (when (looking-at " *\\(:[a-z]+\\)$")
      (match-string-no-properties 1))))

(defun prefix-p (string1 string2)
  "Is STING1 a prefix of STRING2?"
  (and (<= (length string1)
           (length string2))
       (string-equal string1
                     (substring string2 0 (length string1)))))

(defun ghci-command-completion ()
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

(defvar ghci-commands
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

(defvar ghci-show-commands
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
