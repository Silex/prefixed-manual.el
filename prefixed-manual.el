;;; prefixed-manual.el --- Prefixed manual utilities  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/prefixed-manual.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((dash "2.14.1") (inflections "2.5") (s "1.12.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows you to browser the manual in a prefixed manner.

;;; Code:

(require 'info)

(require 's)
(require 'dash)
(require 'inflections)

(defmacro prefixed-manual-with-manual (node &rest body)
  "Go to manual NODE then execute BODY, then exit."
  (declare (indent defun))
  `(progn
     (Info-goto-node node)
     (let ((results ,@body))
       (Info-exit)
       results)))

(defun prefixed-manual-function-signature (function)
  "Return the signature of FUNCTION."
  (let* ((args (help-function-arglist function t)))
    (prin1-to-string (cons function args))))

(defun prefixed-manual-current-children-nodes ()
  "Return the current node's children nodes."
  (let* ((Info-complete-menu-buffer (current-buffer))
         (raw-nodes (nreverse (condition-case nil (Info-complete-menu-item "" nil t) (error nil))))
         (nodes (-map #'Info-extract-menu-item raw-nodes)))
    (-remove-item "Index" nodes)))

(defun prefixed-manual-children-nodes (node)
  "Return the NODE's children nodes."
  (prefixed-manual-with-manual node
    (prefixed-manual-current-children-nodes)))

(defun prefixed-manual-descendants-nodes (node)
  "Return the NODE's descendants nodes."
  (let ((nodes (prefixed-manual-children-nodes node)))
    (-concat nodes (-map #'prefixed-manual-descendants-nodes nodes))))

(defun prefixed-manual-descendants-nodes-debug (node deep)
  "Return the NODE's descendants nodes."
  (message "%s-> %s" (make-string deep "-") node)
  (let ((nodes (prefixed-manual-children-nodes node)))
    (-concat nodes (--map (prefixed-manual-descendants-nodes it (+ 1 deep)) nodes))))

(defun prefixed-manual-descendants-nodes-stack-version (node)
  "Return the NODE's descendants nodes."
  (let ((nodes (prefixed-manual-children-nodes node))
        (results))
    (while nodes
      (let ((item (pop nodes)))
        (push item results)
        (setq nodes (-concat (prefixed-manual-children-nodes item) nodes))))
    results))

(defun prefixed-manual-self-and-descendants-nodes (node)
  "Return the NODE and its descendants nodes."
  (-flatten (list node (prefixed-manual-descendants-nodes node))))

(defun prefixed-manual-current-children-functions ()
  "Return the current node's children functions."
  (let* ((text (substring-no-properties (buffer-string)))
         (matches (s-match-strings-all "-- \\(?:Command\\|Function\\): \\([a-z_-]+\\)" text)))
    (--map (intern (cadr it)) matches)))

(defun prefixed-manual-children-functions (node)
  "Return the NODE's children functions."
  (prefixed-manual-with-manual node
    (prefixed-manual-current-children-functions)))

(defun prefixed-manual-descendants-functions (node)
  "Return the NODE's descendants functions."
  (let* ((nodes (prefixed-manual-self-and-descendants-nodes node)))
    (-flatten (-map #'prefixed-manual-children-functions nodes))))

(defun prefixed-manual-read-topic ()
  "Read a topic from the Elisp manual."
  (completing-read "Topic: " (prefixed-manual-children-nodes "(elisp)")))

(defun prefixed-manual-read-topic-function (topic)
  "Read a function from TOPIC."
  (intern (completing-read "Function: " (prefixed-manual-descendants-functions topic))))

(defun prefixed-manual-read-function ()
  "Read a function from a topic."
  (prefixed-manual-read-topic-function (prefixed-manual-read-topic)))

(defun prefixed-manual-describe-function (function)
  "Like `describe-function', display the full documentation of FUNCTION but restrict to an elisp scope first."
  (interactive (list (prefixed-manual-read-function)))
  (describe-function function))

(defun prefixed-manual-overview ()
  "Displays an overview of the Elisp API."
  (interactive)
  (let ((topics (prefixed-manual-children-nodes "(elisp)"))
        (buffer (generate-new-buffer "* prefixed-manual - overview *")))
    (set-buffer buffer)
    (erase-buffer)
    (--each topics
      (insert (format ";; %s\n" it))
      (let ((functions (prefixed-manual-descendants-functions it)))
        (set-buffer buffer)
        (--map (insert (prefixed-manual-function-signature it) "\n") functions))
      (insert "---------------------------\n"))
    (switch-to-buffer buffer)
    (emacs-lisp-mode)))

(defun prefixed-manual-prefix-topic (topic)
  "Create prefixed aliases for TOPIC."
  (interactive (list (prefixed-manual-read-topic)))
  (let ((functions (prefixed-manual-descendants-functions topic))
        (buffer (generate-new-buffer (format "* prefixed-manual - %s *" topic)))
        (prefix (inflection-singularize-string (s-downcase (car (s-split " " topic))))))
    (set-buffer buffer)
    (erase-buffer)
    (--each functions (insert it "\n"))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (replace-regexp "\\(.*\\)" (format "(defalias '%s-\\1 #'\\1)" prefix))
    (emacs-lisp-mode)))

(provide 'prefixed-manual)

;;; prefixed-manual.el ends here
