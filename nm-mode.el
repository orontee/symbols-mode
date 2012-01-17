(defgroup nm nil
  "List symbols from object files."
  :group 'tools)

(defcustom nm-demangle-names t
  "Non-nil means decode low-level symbol names into user-level names."
  :type 'boolean
  :group 'nm)

(defvar nm-object-file nil)

(defvar nm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'nm-display-manual)
    (define-key map "t" 'nm-toggle-demangling)
    map)
  "Keymap used for programming modes.")

(define-derived-mode nm-mode tabulated-list-mode "Symbols"
  "Major mode for listing the symbols from an object file."
  (setq tabulated-list-format [("Value" 8 t)
			       ("T" 1 t)
			       ("Name" 60 t)
			       ("File" 0 t)])
  (make-local-variable 'nm-object-file)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'nm-list-symbols--refresh nil t)
  (tabulated-list-init-header))

(defun list-symbols (file)
  "Display the list of symbols in FILE.

The return is always nil."
  (interactive "fFile name: ")
  (setq buffer (get-buffer-create "*Symbol List*"))
  (with-current-buffer buffer
    (nm-mode)
    (setq nm-object-file file)
    (nm-list-symbols--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(defun nm-list-symbols--refresh ()
  "Recompute the list of symbols from `nm-object-file'."
  (setq tabulated-list-entries nil)
  (if nm-object-file
      (let ((file nm-object-file)
	    (entries nil))
	(progn
	  (with-temp-buffer
	    (let ((args (concat "-l"	; Use debugging information to
					; try to find a filename and
					; line number
				(if nm-demangle-names "C"))))
	      (call-process "nm" nil (current-buffer) nil args (expand-file-name file))
	      (goto-char (point-min))
	      (while
		  (re-search-forward
		   "\\(\[0-9a-f \]+\\) \\(\[AbBCDdGgiNpRrSsTtUuVvWw-?\]\\) \\(.*\\)" nil t)
		(let* ((value (match-string 1))
		       (type (match-string 2))
		       (others (match-string 3))
		       (name (car (split-string others "\t")))
		       (location (or (cadr (split-string others "\t")) ""))
		       (file (car (split-string location ":")))
		       (line (cadr (split-string location ":")))
		       (label
			(if (and (stringp file) (file-exists-p file) line)
			    `(,name
			      face link
			      help-echo ,(concat "Jump to line " line " of file `" file "'")
			      follow-link t
			      file ,file
			      line ,(string-to-number line)
			      action nm-find-file)
			  name)))
		  (push (list (line-number-at-pos) (vector value type label location))
			entries)))))
	  (setq tabulated-list-entries entries)))
    (message "No object file associated to buffer")))
  
(defun nm-find-file (button)
  (let ((line (button-get button 'line))
	(buff (find-file (button-get button 'file))))
    (when (numberp line)
      (with-current-buffer buff
	(goto-char (point-min)) (forward-line (1- line))))))

(require 'info)
(defun nm-display-manual ()
  "Go to Info buffer that displays nm manual, creating it if none already exists."
  (interactive)
  (let ((blist (buffer-list))
	(manual-re (concat "\\(/\\|\\`\\)" "binutils" "\\(\\.\\|\\'\\)"))
	(case-fold-search t)
	found)
    (dolist (buffer blist)
      (with-current-buffer buffer
	(if (and (eq major-mode 'Info-mode)
		 (stringp Info-current-file)
		 (string-match manual-re Info-current-file))
	    (setq found buffer
		  blist nil))))
    (if found
	(switch-to-buffer found)
      (info-initialize)
      (info (Info-find-file "binutils")))
    (Info-goto-node "(binutils)nm")))

(defun nm-toggle-demangling ()
  "Toggle whether to decode low-level symbol names into
user-level names."
  (interactive)
  (setq nm-demangle-names (not nm-demangle-names))
  (nm-list-symbols--refresh)
  (tabulated-list-print))

