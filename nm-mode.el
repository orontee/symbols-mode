;;; nm-mode.el --- List symbols of object files

;; Copyright (C) 2012  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: tools, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See `list-symbols' docstring for usage information.

;;; Code:

(defgroup nm nil
  "List symbols from object files."
  :group 'tools)

(defcustom nm-demangle-names t
  "Non-nil means decode low-level symbol names into user-level names."
  :type 'boolean
  :group 'nm)

(defcustom nm-undefined-symbols-only nil
  "Non-nil means display only undefined symbols."
  :type 'boolean
  :group 'nm)

(defvar nm-object-file nil)

(defvar nm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'nm-display-manual)
    (define-key map "t" 'nm-toggle-demangling)
    (define-key map "u" 'nm-toggle-undefined-only)
    map)
  "Keymap used for programming modes.")

(define-derived-mode nm-mode tabulated-list-mode "Symbols"
  "Major mode for listing the symbols from an object file."
  (setq tabulated-list-format [("Value" 16 t)
			       ("T" 1 t)
			       ("Name" 60 t)
			       ("File" 0 t)])
  (make-local-variable 'nm-object-file)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'nm-list-symbols--refresh nil t)
  (tabulated-list-init-header))

(defun list-symbols (file)
  "Display the list of symbols in FILE.

Set `nm-demangle-names' to t to decode low-level symbol names to
user-level names.

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
	    (let ((args (concat "-l"
				(if nm-demangle-names "C")
				(if nm-undefined-symbols-only "u"))))
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

(defun nm-toggle-undefined-only ()
  "Toggle whether to display only undefined symbols or not."
  (interactive)
  (setq nm-undefined-symbols-only (not nm-undefined-symbols-only))
  (nm-list-symbols--refresh)
  (tabulated-list-print))

(provide 'list-symbols)
(provide 'nm-mode)

;;; nm-mode.el ends here
