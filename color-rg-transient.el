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

(defvar color-rg-transient-search-history nil
  "History of search terms.")

(defvar color-rg-transient-replace-history nil
  "History of replacement terms.")

(defvar color-rg-transient-include-history nil
  "History of include patterns.")

(defvar color-rg-transient-exclude-history nil
  "History of exclude patterns.")

;; Persistent storage for the transient values between sessions
(defvar color-rg-transient--search-value nil
  "Store search value between sessions.")

(defvar color-rg-transient--replace-value nil
  "Store replace value between sessions.")

(defvar color-rg-transient--include-value nil
  "Store include pattern between sessions.")

(defvar color-rg-transient--exclude-value nil
  "Store exclude pattern between sessions.")

;; Save flags between sessions
(defvar color-rg-transient--saved-flags nil
  "Store flag values between sessions.")

(defclass color-rg-transient-persistent-variable (transient-variable)
  ((variable :initarg :variable :initform nil)
   (history-key :initarg :history-key :initform nil)
   (default-value :initarg :default-value :initform nil))
  "Class for color-rg-transient variables that persist between sessions.")

(cl-defmethod transient-init-value ((obj color-rg-transient-persistent-variable))
  "Initialize the value from the associated variable."
  (let ((var (oref obj variable))
        (default (oref obj default-value)))
    (oset obj value (if (and var (boundp var) (symbol-value var))
                        (symbol-value var)
                      default))))

(cl-defmethod transient-infix-set ((obj color-rg-transient-persistent-variable) value)
  "Set the variable value to persist between sessions."
  (let ((var (oref obj variable)))
    (when var
      (set var value))
    (oset obj value value)
    value))

(cl-defmethod transient-format-value ((obj color-rg-transient-persistent-variable))
  "Format the value for display."
  (let ((value (oref obj value)))
    (if (or (null value) (string-empty-p value))
        (propertize "not set" 'face 'transient-inactive-value)
      (propertize value 'face 'transient-value))))

(defclass color-rg-transient-unset-variable (color-rg-transient-persistent-variable)
  ((unset-label :initarg :unset-label :initform "Unset"))
  "Class for color-rg-transient variables that can be unset.")

(cl-defmethod transient-infix-read ((obj color-rg-transient-persistent-variable))
  "Read a string value with history."
  (let* ((prompt (concat (oref obj description) ": "))
         (history-var (oref obj history-key))
         (current (oref obj value))
         (value (read-string prompt current history-var)))
    (transient-infix-set obj value)
    (transient-set)
    value))

(cl-defmethod transient-infix-read ((obj color-rg-transient-unset-variable))
  "Read a value or allow unsetting it."
  (let* ((choices (list (cons (oref obj unset-label) 'unset)
                       (cons "Set value" 'set)))
         (choice (completing-read "Action: " choices nil t))
         (history-var (oref obj history-key))
         (current (oref obj value))
         (value (if (string= choice (oref obj unset-label))
                   nil
                 (read-string (concat (oref obj description) ": ") 
                              current history-var))))
    (transient-infix-set obj value)
    (transient-set)
    value))

(transient-define-infix color-rg-transient--search ()
  :class 'color-rg-transient-persistent-variable
  :description "Search"
  :variable 'color-rg-transient--search-value
  :history-key 'color-rg-transient-search-history)

(transient-define-infix color-rg-transient--replace ()
  :class 'color-rg-transient-unset-variable
  :description "Replace"
  :variable 'color-rg-transient--replace-value
  :history-key 'color-rg-transient-replace-history
  :unset-label "Unset (search only)")

(transient-define-infix color-rg-transient--include ()
  :class 'color-rg-transient-persistent-variable
  :description "Include"
  :variable 'color-rg-transient--include-value
  :history-key 'color-rg-transient-include-history)

(transient-define-infix color-rg-transient--exclude ()
  :class 'color-rg-transient-persistent-variable
  :description "Exclude"
  :variable 'color-rg-transient--exclude-value
  :history-key 'color-rg-transient-exclude-history)

;; Custom class that saves state when changed
(defclass color-rg-transient-flag (transient-switches)
  ()
  "A transient-switches class that saves flags when changed.")

(cl-defmethod transient-infix-set ((obj color-rg-transient-flag) value)
  "Set the value and save all flags."
  (cl-call-next-method obj value)
  ;; Save the new state of all flags
  (setq color-rg-transient--saved-flags (transient-args 'color-rg-transient))
  (transient-set))

(defclass color-rg-transient-switch (transient-switch)
  ()
  "A transient-switch class that saves flags when changed.")

(cl-defmethod transient-infix-set ((obj color-rg-transient-switch) value)
  "Set the value and save all flags."
  (cl-call-next-method obj value)
  ;; Save the new state of all flags
  (setq color-rg-transient--saved-flags (transient-args 'color-rg-transient))
  (transient-set))

(transient-define-argument color-rg-transient--case ()
  :description "Match Case"
  :class 'color-rg-transient-flag
  :argument-format "%s"
  :argument-regexp "\\(--ignore-case\\|--smart-case\\)"
  :init-value (lambda (obj) (oset obj value "--smart-case"))
  :choices '("--ignore-case" "--smart-case"))

(transient-define-argument color-rg-transient--literal ()
  :description "Literal"
  :class 'color-rg-transient-switch
  :argument "--fixed-strings")

(transient-define-argument color-rg-transient--word ()
  :description "Match whole word"
  :class 'color-rg-transient-switch
  :argument "--word-regexp")

(transient-define-argument color-rg-transient--hidden ()
  :description "Search Hidden"
  :class 'color-rg-transient-switch
  :argument "--hidden")

(transient-define-argument color-rg-transient--buffer ()
  :description "Current buffer"
  :class 'color-rg-transient-switch
  :argument "--current-buffer")

(transient-define-argument color-rg-transient--unrestricted ()
  :description "Unrestricted"
  :class 'color-rg-transient-switch
  :argument "--unrestricted")

;;;###autoload (autoload 'color-rg-transient "color-rg-transient" nil t)
(transient-define-prefix color-rg-transient ()
  "Color-rg transient menu."
  :value (lambda ()
           (if color-rg-transient--saved-flags
               color-rg-transient--saved-flags
             '("--smart-case")))
  :history-key 'color-rg-transient

  ["Input"
   ("p" "Search" color-rg-transient--search)
   ("r" "Replace" color-rg-transient--replace)]
  [["Options"
    ("s" "Match Case" color-rg-transient--case)
    ("f" "Literal" color-rg-transient--literal)
    ("w" "Match whole word" color-rg-transient--word)
    ("h" "Search Hidden" color-rg-transient--hidden)]
   ["Scope"
    ("b" "Current buffer" color-rg-transient--buffer)
    ("u" "Unrestricted" color-rg-transient--unrestricted)
    ("i" "Include" color-rg-transient--include)
    ("x" "Exclude" color-rg-transient--exclude)]]
  ["Actions"
   [("RET" "Execute" color-rg-transient-execute)]
   [("q" "Quit" transient-quit-all)]]
  (interactive)
  (transient-setup 'color-rg-transient))

(defun color-rg-transient-execute ()
  "Execute color-rg search/replace based on transient arguments."
  (interactive)
  (let* ((args (transient-args 'color-rg-transient))
         (search-term color-rg-transient--search-value)
         (replace-term color-rg-transient--replace-value)
         (literal (member "--fixed-strings" args))
         (case-sensitive (member "--smart-case" args))
         (current-buffer (member "--current-buffer" args))
         (include color-rg-transient--include-value)
         (exclude color-rg-transient--exclude-value))
    ;; Make sure we have the latest flags
    (setq color-rg-transient--saved-flags args)
    ;; Save state for next time
    (transient-save)

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
          (color-rg-replace-all-matches replace-term))
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
;;; color-rg-transient.el ends here
