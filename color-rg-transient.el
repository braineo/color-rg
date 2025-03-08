;;; color-rg-transient.el --- Transient interface for color-rg  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Binbin Ye

;; Author: Binbin Ye
;; Keywords: matching, tools
;; Package-Requires: ((emacs "27.1") (transient "0.4.0") (color-rg "5.6"))

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

;; This package provides a transient interface for color-rg.
;; It allows users to interactively set search options and see live previews
;; of their search results.

;;; Code:


(require 'transient)
(require 'color-rg)

(defgroup color-rg-transient nil
  "Transient interface for color-rg."
  :group 'color-rg)

(defcustom color-rg-transient-history-file
  (expand-file-name "color-rg-transient-history.el" user-emacs-directory)
  "File to save color-rg transient state history."
  :type 'file
  :group 'color-rg-transient)

(defvar color-rg-transient-search-history nil
  "History of search terms.")

(defvar color-rg-transient-replace-history nil
  "History of replacement terms.")

(defvar color-rg-transient-include-history nil
  "History of include patterns.")

(defvar color-rg-transient-exclude-history nil
  "History of exclude patterns.")

(defclass color-rg-transient-variable-with-history (transient-variable)
  ((history-key :initarg :history-key :initform nil)))

(cl-defmethod transient-init-value ((obj color-rg-transient-variable-with-history))
  (oset obj value (transient-get-value)))

(cl-defmethod transient-infix-read ((obj color-rg-transient-variable-with-history))
  (let ((history-var (oref obj history-key)))
    (read-string (concat (oref obj description) ": ")
                (oref obj value)
                history-var)))

(cl-defmethod transient-format-value ((obj color-rg-transient-variable-with-history))
  (let ((value (oref obj value)))
    (if (or (null value) (string-empty-p value))
        (propertize "not set" 'face 'transient-inactive-value)
      (propertize value 'face 'transient-value))))

(transient-define-infix color-rg-transient--search ()
  :class 'color-rg-transient-variable-with-history
  :description "Search"
  :variable 'color-rg-transient--search-value
  :history-key 'color-rg-transient-search-history)

(transient-define-infix color-rg-transient--replace ()
  :class 'color-rg-transient-variable-with-history
  :description "Replace"
  :variable 'color-rg-transient--replace-value
  :history-key 'color-rg-transient-replace-history)

(transient-define-infix color-rg-transient--include ()
  :class 'color-rg-transient-variable-with-history
  :description "Include"
  :variable 'color-rg-transient--include-value
  :history-key 'color-rg-transient-include-history)

(transient-define-infix color-rg-transient--exclude ()
  :class 'color-rg-transient-variable-with-history
  :description "Exclude"
  :variable 'color-rg-transient--exclude-value
  :history-key 'color-rg-transient-exclude-history)

(transient-define-argument color-rg-transient--case ()
  :description "Match Case"
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "\\(--ignore-case\\|--smart-case\\)"
  :choices '("--ignore-case" "--smart-case"))

(transient-define-argument color-rg-transient--literal ()
  :description "Literal"
  :class 'transient-switch
  :argument "--fixed-strings")

(transient-define-argument color-rg-transient--word ()
  :description "Match whole word"
  :class 'transient-switch
  :argument "--word-regexp")

(transient-define-argument color-rg-transient--hidden ()
  :description "Search Hidden"
  :class 'transient-switch
  :argument "--hidden")

(transient-define-argument color-rg-transient--buffer ()
  :description "Current buffer"
  :class 'transient-switch
  :argument "--current-buffer")

(transient-define-argument color-rg-transient--unrestricted ()
  :description "Unrestricted"
  :class 'transient-switch
  :argument "--unrestricted")

;;;###autoload (autoload 'color-rg-transient "color-rg-transient" nil t)
(transient-define-prefix color-rg-transient ()
  "Color-rg transient menu."
  :value '("--smart-case")
  ["Input"
   ("p" "Search" color-rg-transient--search)
   ("r" "Replace" color-rg-transient--replace)]
  ["Options"
   ("s" "Match Case" color-rg-transient--case)
   ("f" "Literal" color-rg-transient--literal)
   ("w" "Match whole word" color-rg-transient--word)
   ("h" "Search Hidden" color-rg-transient--hidden)]
  ["Scope"
   ("b" "Current buffer" color-rg-transient--buffer)
   ("u" "Unrestricted" color-rg-transient--unrestricted)
   ("i" "Include" color-rg-transient--include)
   ("x" "Exclude" color-rg-transient--exclude)]
  [["Actions"
    ("RET" "Execute" color-rg-transient-execute)
    ("q" "Quit" transient-quit-all)]]
  (interactive)
  (transient-setup 'color-rg-transient))

(defun color-rg-transient--get-search-term ()
  (or (transient-arg-value "--search=" (transient-args 'color-rg-transient))
      (car color-rg-transient-search-history)
      ""))

(defun color-rg-transient--get-replace-term ()
  (transient-arg-value "--replace=" (transient-args 'color-rg-transient)))

(defun color-rg-transient-execute ()
  "Execute color-rg search/replace based on transient arguments."
  (interactive)
  (let* ((args (transient-args 'color-rg-transient))
         (search-term (color-rg-transient--get-search-term))
         (replace-term (color-rg-transient--get-replace-term))
         (literal (member "--fixed-strings" args))
         (case-sensitive (member "--smart-case" args))
         (current-buffer (member "--current-buffer" args))
         (include (transient-arg-value "--include=" args))
         (exclude (transient-arg-value "--exclude=" args)))

    ;; Save state for next time
    (transient-set-value 'color-rg-transient args)

    (if replace-term
        (progn
          ;; Perform search and replace
          (color-rg-search search-term
                          (if current-buffer buffer-file-name default-directory)
                          (or include "everything")
                          nil  ; file-list
                          literal
                          nil  ; no-ignore
                          nil  ; no-node
                          case-sensitive)
          ;; Then trigger replace
          (color-rg-replace-all-matches))
      ;; Just search
      (color-rg-search search-term
                       (if current-buffer buffer-file-name default-directory)
                       (or include "everything")
                       nil  ; file-list
                       literal
                       nil  ; no-ignore
                       nil  ; no-node
                       case-sensitive))))

(provide 'color-rg-transient)
