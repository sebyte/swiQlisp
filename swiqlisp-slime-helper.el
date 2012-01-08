;;; swiqlisp-slime-helper.el

;;; Copyright © 2011 Sebastian D. Tennant <sdt@sebyte.me>
;;;
;;; This file is part of swiQlisp — site-wide Quicklisp.
;;;
;;; swiQlisp is free software: you can redistribute it and/or modify it under the
;;; terms of the GNU General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your option) any later
;;; version.
;;;
;;; swiQlisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; swiQlisp.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;;
;;;  When loaded (by Emacs) this file (slime-helper.el) configures Emacs to use
;;;  swiQlisp SLIME (the Superior Lisp Interaction Mode for Emacs).
;;;
;;;  Simply add the following line:
;;;
;;;   (load "~swiqlisp/slime-helper.el")
;;;
;;;  to your ~/.emacs init file (and remove any existing Quicklisp SLIME
;;;  configuration).
;;;
;;;  For some reason SLIME is called 'swank' in Quicklisp so don't forget to:
;;;
;;;   $ sudo swiqlisp install swank
;;;
;;;  before restarting Emacs.  Do not use swiQlisp to install the package
;;;  'quicklisp-slime-helper'.  It assumes Quicklisp is installed under your
;;;  home directory and will not work as expected.

(setq quicklisp-slime-helper-base (expand-file-name "~swiqlisp/quicklisp/"))

(defun quicklisp-slime-helper-file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun quicklisp-slime-helper-slime-directory ()
  (let ((location-file
         (concat quicklisp-slime-helper-base
                 "dists/quicklisp/installed/systems/swank.txt")))
    (when (file-exists-p location-file)
      (let ((relative (quicklisp-slime-helper-file-contents location-file)))
        (file-name-directory
         (concat quicklisp-slime-helper-base relative))))))

(let* ((quicklisp-slime-directory (quicklisp-slime-helper-slime-directory)))
  (add-to-list 'load-path quicklisp-slime-directory)
  (require 'slime-autoloads)
  (setq slime-backend
        (expand-file-name "swank-loader.lisp" quicklisp-slime-directory))
  (setq slime-path quicklisp-slime-directory)
  (slime-setup '(slime-fancy)))
