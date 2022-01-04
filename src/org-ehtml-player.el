;;; org-ehtml-play.el --- Player add-on for ehtml    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: multimedia

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

;; 

;;; Code:
(require 'org-ehtml-server)
(require 'subr-x)
(require 'cl-lib)

(defvar org-ehtml-player-process nil
  "Process for `org-ehtml-play'.")

(defcustom org-ehtml-players
  '((mplayer .
     ("mplayer" "%u"))
    (vlc .
     ("vlc" "-Idummy" "%u")))
  "Mapping of player symbols to command lists.
The first element of the list is the command to be executed.
The others are the arguments for the command.
An %u in the argument list is replaced by the URL to be played."
  :group 'org-export-ehtml
  :type '(repeat (cons symbol (repeat string))))

(defun org-ehtml-play (player url)
  "Start URL in PLAYER.
PLAYER is mapped by `org-ehtml-players' to
a list of a command with arguments.
%u is replaced by URL in the argument list."
  (when (process-live-p org-ehtml-player-process)
    (kill-process org-ehtml-player-process))
  (with-current-buffer (get-buffer-create "*ehtml-play*")
    (erase-buffer))
  (when-let ((fun-list (alist-get player org-ehtml-players)))
    (setq fun-list (mapcar
		    (lambda (str)
		      (format-spec str (list
					(cons ?u url))))
		    fun-list))
    (setq org-ehtml-player-process
	  (apply #'start-process "ehtml-play" "*ehtml-play*"
		 fun-list))
    (with-current-buffer "*ehtml-play*"
      (write-region nil nil "/tmp/ehtml.log" t))
    ))

(defun org-ehtml-play-link-export (player path description backend)
  "Export links of type [[ehtml-PLAYERNAME:PATH][DESCRIPTION]].
The PLAYER is mapped to a command by `org-ehtml-players'.
Only ehtml BACKEND is supported."
  (unless (memq backend '(ehtml html)) ;; This should be (eq backend 'ehtml), but there is actually a bug in `org-html-link'.
    (user-error "Org ehtml links can only be exported to ehtml"))
  (format "<a href=\"?%s\">%s</a>"
	  (url-encode-url (format "%S" (list 'org-ehtml-play (list 'quote player) path)))
	  description))

(put 'org-ehtml-play 'org-ehtml-safe-form '(t org-ehtml-qsymbol-p stringp))
;; (org-ehtml-safe-form-p '(org-ehtml-play 'mplayer "http://test"))

(org-link-set-parameters "ehtml-mplayer"
			 :follow
			 (lambda (url) (org-ehtml-play 'mplayer url))
			 :export
			 (lambda (&rest args)
			   (apply #'org-ehtml-play-link-export 'mplayer args))
			 )

(org-link-set-parameters "ehtml-vlc"
			 :follow
			 (lambda (url) (org-ehtml-play 'vlc url))
			 :export
			 (lambda (&rest args)
			   (apply #'org-ehtml-play-link-export 'vlc args))
			 )

(provide 'org-ehtml-player)
;;; org-ehtml-player.el ends here
