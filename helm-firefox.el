;;; helm-firefox.el --- Firefox bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Version: 1.6.8
;; Package-Requires: ((helm "1.5") (cl-lib "0.5") (emacs "24.1"))
;; URL: https://github.com/emacs-helm/helm-firefox

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'helm-adaptive)
(require 'helm-net)

;;
;; You will have to set firefox to import bookmarks in his html file bookmarks.html.
;; (only for firefox versions >=3)
;; To achieve that, open about:config in firefox and double click on this line to enable value
;; to true:
;; user_pref("browser.bookmarks.autoExportHTML", false);
;; You should have now:
;; user_pref("browser.bookmarks.autoExportHTML", true);
;; NOTE: This is also working in the same way for mozilla aka seamonkey.


(defgroup helm-firefox nil
  "Helm libraries and applications for Firefox navigator."
  :group 'helm)

(defcustom helm-firefox-default-directory "~/.mozilla/firefox/"
  "The root directory containing firefox config.
On Mac OS X, probably set to \"~/Library/Application Support/Firefox/\"."
  :group 'helm-firefox
  :type 'string)

(defvar helm-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ \"]*")
(defvar helm-firefox-bookmarks-regexp ">\\([^><]+.\\)</[aA]>")
(defvar helm-firefox-bookmarks-subdirectory-regex "<H[1-6][^>]*>\\([^<]*\\)</H.>")
(defvar helm-firefox-separator " » ")

(defun helm-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let* ((moz-dir helm-firefox-default-directory)
         (moz-user-dir
          (with-current-buffer (find-file-noselect (expand-file-name "profiles.ini" moz-dir))
            (goto-char (point-min))
            (prog1
                (when (search-forward "Path=" nil t)
                  (buffer-substring-no-properties (point) (point-at-eol)))
              (kill-buffer)))))
    (file-name-as-directory (concat moz-dir moz-user-dir))))

(defun helm-firefox-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title stack)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "href=\\|^ *<DT><A HREF=\\|<DT><H\\|</DL>" nil t)
        (forward-line 0)
        (cond (;; Matches on bookmark folders (<H3>...</H3>).
               (string-equal (match-string 0) "<DT><H")
               ;; Extract bookmark folders name
               (if (re-search-forward
                    helm-firefox-bookmarks-subdirectory-regex
                    (point-at-eol) t)
                   (push (match-string 1) stack)))
              (;; Matches end of bookmark folder.
               (string-equal (match-string 0) "</DL>")
               (pop stack))
              (t
               (when (re-search-forward url-regexp nil t)
                 (setq url (match-string 0)))
               (when (re-search-forward bmk-regexp nil t)
                 (setq title (match-string 1)))
               (push
                (cons
                 ;; "Dir >> Dir >> Title"
                 (mapconcat 'identity
                            (reverse (cons title stack))
                            helm-firefox-separator)
                 url)
                bookmarks-alist)))
        (forward-line)))
    (nreverse bookmarks-alist)))


(defun helm-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (expand-file-name "bookmarks.html" (helm-get-firefox-user-init-dir)))

(defvar helm-firefox-bookmarks-alist nil)
(defvar helm-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq helm-firefox-bookmarks-alist
                    (helm-firefox-bookmarks-to-alist
                     (helm-guess-firefox-bookmark-file)
                     helm-firefox-bookmark-url-regexp
                     helm-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-firefox-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-firefox-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-browse-url
                     (helm-firefox-bookmarks-get-value candidate))))
               ("Copy Url"
                . (lambda (candidate)
                    (let ((url (helm-firefox-bookmarks-get-value
                                candidate)))
                      (kill-new url)
                      (message "`%s' copied to kill-ring" url))))))))

(defun helm-firefox-bookmarks-get-value (elm)
  (assoc-default elm helm-firefox-bookmarks-alist))

(defun helm-highlight-firefox-bookmarks (bookmarks _source)
  (cl-loop for i in bookmarks
           for elements = (split-string i helm-firefox-separator)
           for path = (butlast elements)
           for prefix = (if path
                            (concat
                             (mapconcat 'identity path helm-firefox-separator)
                             helm-firefox-separator)
                          "")
           for title = (car (last elements))
           collect (concat
                    (propertize
                     prefix 'face '((:foreground "DarkGray"))
                     'help-echo (helm-firefox-bookmarks-get-value i))
                    (propertize
                     title 'face '((:foreground "YellowGreen"))
                     'help-echo (helm-firefox-bookmarks-get-value i)))
                     ))

;;;###autoload
(defun helm-firefox-bookmarks ()
  "Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value \
to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.
"
  (interactive)
  (helm-other-buffer 'helm-source-firefox-bookmarks
                     "*Helm Firefox*"))


(provide 'helm-firefox)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-firefox.el ends here
