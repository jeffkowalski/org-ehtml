;;; org-ehtml-server.el --- emacs web server for editable Org-mode files -*- lexical-binding: t -*-

;; Copyright (C) 2012 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: org web-server javascript html

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'web-server)
(require 'ox-ehtml)
(require 'org-agenda)
(require 'cl-lib)
(require 'subr-x)

(declare-function org-agenda-write "org-agenda"
		  (file &optional open nosettings agenda-bufname))

(defun org-ehtml-docroot-setter (symbol value)
  "Function used to ensure `org-ehtml-docroot' is expanded when set."
  (let* ((orig (eval value))
         (expanded (expand-file-name orig)))
    (unless (string= expanded orig)
      (warn "expanded `org-ehtml-docroot' from %S to %S"
            orig expanded))
    (set-default symbol expanded)))

(let ((default (expand-file-name
                "public_org"
                (expand-file-name
                 ".."
                 (file-name-directory
                  (or load-file-name (buffer-file-name)))))))
  (eval `(defcustom org-ehtml-docroot ,default
           "Document root from which to serve Org-mode files.
This value should be fully expanded as with `expand-file-name'
and should not contain e.g., \"~\" for a user home directory."
           :group 'org-ehtml
           :type 'string
           :risky t
           :set #'org-ehtml-docroot-setter)))

(defvar org-ehtml-before-save-hook nil
  "Hook run in a file buffer before saving web edits.
If any function in this hook returns nil then the edit is aborted.")

(defvar org-ehtml-after-save-hook nil
  "Hook run in a file buffer after saving web edits.
Functions of this hook will be called on the `ws-request' object
as their only argument.")

(defvar org-ehtml-dir-match "^\\([^\.].*[^~]\\|\\.\\.\\)$"
  "Match string passed to `directory-files-and-attributes' for dir listing.")

(defvar org-ehtml-allow-agenda nil
  "If non-nil agenda views are allowed.")

(defvar org-ehtml-allow-src-block nil
  "If calling of source blocks from the generated HTML page is allowed.")

(defvar org-ehtml-handler
  '(((:GET  . ".*") . org-ehtml-file-handler)
    ((:POST . ".*") . org-ehtml-edit-handler)))

(defun org-ehtml-file-handler (request)
  (with-slots (process headers) request
    (cond
     ((assoc "src_block_name" headers)
      (org-ehtml-src-block-handler request))
     ((alist-get "ehtml-query" headers nil nil 'string-equal)
      (org-ehtml-query-handler request))
     (t
      (let ((path (ws-in-directory-p org-ehtml-docroot
                                     (substring (cdr (assoc :GET headers)) 1))))
	(if path
            (org-ehtml-serve-file path process)
          (ws-send-404 process)))))))

(defun org-ehtml-send-400 (proc message)
  "Send 400 to PROC with a MESSAGE."
  (ws-response-header proc 400 '("Content-type" . "text/html"))
  (process-send-string proc message)
  (throw 'close-connection nil))

(defvar org-agenda-buffer-name)
(defun org-ehtml-serve-file (file proc)
  (cond
   ;; agenda support
   ((and org-ehtml-allow-agenda
         (string-match "/agenda/\\([^/]*\\)\\(?:/\\(.*\\)\\)?" file))
    (let ((cmd (match-string 1 file))
          (params (when (match-string 2 file)
                    (split-string (match-string 2 file) "/"))))
      (pcase cmd
        ((or `"day" `"week" `"fortnight" `"month" `"year"
             (pred (lambda (x) (string-match-p "[1-9][0-9]*" x))))
         (org-agenda-list nil
                          (when params (car params))
                          (if (string-match-p "[1-9][0-9]*" cmd)
                              (string-to-number cmd)
                            (intern-soft cmd))))
        (`"todo"
         (org-todo-list))
        (`"tags"
         (let* ((todo-only (string= (car params) "todo"))
                (match (if todo-only
                           (cadr params)
                         (car params))))
           (if (and (stringp match) (string-match-p "\\S-" match))
               (org-tags-view todo-only match)
             (org-ehtml-send-400 proc "Missing params."))))
        (`"custom"
         (let ((custom org-agenda-custom-commands)
               (prefixes nil)
               (descriptions nil)
	       entry
	       key
	       desc)
           (while (setq entry (pop custom))
             (setq key (car entry) desc (nth 1 entry))
             (when (> (length key) 0)
               (cl-pushnew key prefixes)
               (cl-pushnew
                (format "<a href=\"/agenda/custom/%s\">%s</a>:%s " key key desc)
		descriptions)))
           (if (member (car params) prefixes)
               (org-agenda nil (car params))
             (org-ehtml-send-400 proc
                                 (format
                                  "Invalid custom command.  Try %s."
                                  (mapconcat 'identity descriptions " or "))))))
        (_
         (org-ehtml-send-400 proc (format "Unknown Agenda Command `%s'.  Try\
 <a href=\"/agenda/day\">day</a> or <a href=\"/agenda/todo\">todo</a>." cmd))))
      (with-current-buffer org-agenda-buffer-name
        (let ((fname (make-temp-file "agenda-" nil ".html")))
          (org-agenda-write fname)
          (ws-send-file proc fname)))))
   ;; normal files (including index.org or index.html if they exist)
   ((or (not (file-directory-p file))
        (let ((i-org  (expand-file-name "index.org" file))
              (i-html (expand-file-name "index.html" file)))
          (or (and (file-exists-p i-org)  (setq file i-org))
              (and (file-exists-p i-html) (setq file i-html)))))
    (ws-send-file proc
      (if (member (file-name-extension file) '("org" "html"))
          (org-ehtml-cached file) file)
      (when (member (file-name-extension file) '("org" "html"))
          '"text/html; charset=utf-8")))
   ;; directory listing
   ((file-directory-p file)
    (ws-send-directory-list proc file))
   ;; none of the above -> missing file
   (t (ws-send-404 proc))))

(defun org-ehtml-edit-handler (request)
  (with-slots (process headers) request
    (let* ((path       (substring (cdr (assoc "path" headers)) 1))
           (beg (string-to-number (cdr (assoc "beg"  headers))))
           (end (string-to-number (cdr (assoc "end"  headers))))
           (org                   (decode-coding-string (cdr (assoc "org"  headers)) 'utf-8-unix t)))
      (when (string= (file-name-nondirectory path) "")
        (setq path (concat path "index.org")))
      (when (string= (file-name-extension path) "html")
        (setq path (concat (file-name-sans-extension path) ".org")))
      (org-babel-with-temp-filebuffer (expand-file-name path org-ehtml-docroot)
        (let ((orig (buffer-string)))
          (replace-region beg end org)
          (if (run-hook-with-args-until-failure 'org-ehtml-before-save-hook)
              (save-buffer)
            (replace-region (point-min) (point-max) orig)
            (ws-send-500 process "edit failed `org-ehtml-before-save-hook'")))
        (run-hook-with-args 'org-ehtml-after-save-hook request))
      (ws-response-header process 200
        '("Content-type" . "text/html; charset=utf-8"))
      (process-send-string process
        (org-export-string-as org 'html 'body-only '(:with-toc nil))))))

(defun org-ehtml-src-block-handler (request)
  "Run the source block :src_block_name in REQUEST."
  (with-slots (process headers) request
    (let* ((path       (substring (cdr (assoc "path" headers)) 1))
	   (src-block-name (cdr (assoc "src_block_name" headers))))
      (cl-assert (stringp src-block-name) nil "Internal error in `org-ehtml-src-block-handler'. Expected src_block_name, got %s" src-block-name)
      (when (string= (file-name-nondirectory path) "")
        (setq path (concat path "index.org")))
      (when (string= (file-name-extension path) "html")
        (setq path (concat (file-name-sans-extension path) ".org")))
      (org-babel-with-temp-filebuffer (expand-file-name path org-ehtml-docroot)
        (let* ((org-confirm-babel-evaluate nil)
	       (result (eval `(org-sbe ,src-block-name))))
          (run-hook-with-args 'org-ehtml-after-src-block request src-block-name result)))
      (ws-response-header process 200
			  '("Content-type" . "text/html; charset=utf-8")))
    ))

(defun org-ehtml-qsymbol-p (form)
  "Return non-nil if FORM is a quoted symbol."
  (and (consp form)
       (eq (car form) 'quote)
       (symbolp (cadr form))))
;; Test:
;; (org-ehtml-qsymbol-p (car '('sym)))

(defun org-ehtml-safe-form-p (form)
  "Test whether FORM from a url is safe for evaluation."
  (when-let (((consp form))
	     (sym (car form))
	     ((symbolp sym))
	     (test (get sym 'org-ehtml-safe-form)))
    (cl-loop for arg-test in (cdr test)
	     for arg in (cdr form)
	     unless (funcall arg-test arg)
	     return nil
	     finally return t)))

(defvar org-ehtml-after-query nil
  "Hook run after handling queries ?ehtml-query=...")

(defun org-ehtml-query-handler (request)
  "Run the source block :src_block_name in REQUEST."
  (with-slots (process headers) request
    (let* ((query (alist-get "ehtml-query" headers nil nil #'string-equal)))
      (cl-assert (stringp query) nil "Internal error in `org-ehtml-src-block-handler'. Expected query url, got %s" query)
      (when-let ((cmd (read query))
		 ((or (consp cmd)
		      (user-error "Expected form as query of url got %s")))
		 ((or (org-ehtml-safe-form-p cmd)
		      (user-error "Not a safe command for ehtml query: %s" cmd)))
		 (result (eval cmd)))
        (run-hook-with-args 'org-ehtml-after-query request cmd result)))
    (ws-response-header process 200
			'("Content-type" . "text/html; charset=utf-8"))))

(provide 'org-ehtml-server)
;;; org-ehtml-server.el ends here
