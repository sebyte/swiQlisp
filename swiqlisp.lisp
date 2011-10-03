;;; swiqlisp.lisp

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
;;;  This lisp source file is copied to ~swiqlisp by the installation script
;;;  swiqlisp-install (where it is loaded by the lisp invoked each time the main
;;;  site-wide management tool swiqlisp is run).


(use-package "QL-DIST")

;;; ============================================================================
;;; utility functions

(defun string-truncate (str &optional (width 25))
  (if (<= (length str) width)
      str
    (format nil "~a..." (subseq str 0 (- width 3)))))

(defun split-list (l)
  (let ((numthrees (floor (length l) 3)) ones twos threes)
    (dotimes (i numthrees)
      (push (pop l) ones) (push (pop l) twos) (push (pop l) threes))
    (values
     ;; threes
     (reverse ones)
     (reverse twos)
     (reverse threes)
     ;; stragglers (at most two)
     l)))

(defun write-threes (leftcol midcol rightcol
                     &optional (3col-format " ~25a ~25a ~25a~%"))
  (mapc (lambda (objX objY objZ)
          (format t 3col-format (string-truncate (name objX))
                                (string-truncate (name objY))
                                (string-truncate (name objZ))))
        leftcol midcol rightcol))

(defun write-stragglers (objects &optional (straggler-format " ~25a"))
  (format t straggler-format (string-truncate (name (car objects))))
  (when (cadr objects)
    (format t straggler-format (string-truncate (name (cadr objects)))))
  (terpri))

;;; ============================================================================
;;; query functions

;;; projects
(defun list-installed-projects ()
  (mapc (lambda (d)
          (let ((installed-releases (installed-releases d)))
            (multiple-value-bind (leftcol midcol rightcol stragglers)
                (split-list installed-releases)
              (format t "~%~d projects installed from distribution: ~a~%~%"
                      (length installed-releases) (name d))
              (write-threes leftcol midcol rightcol)
              (when stragglers (write-stragglers stragglers))))
          (terpri))
        (all-dists)))

(defun list-available-projects ()
  (mapc (lambda (d)
          (let* ((provided-releases (provided-releases d))
                 (installed-releases (installed-releases d))
                 (available-releases
                  (remove-if (lambda (r)
                               (member r installed-releases :test #'equal))
                             provided-releases)))
            (multiple-value-bind (leftcol midcol rightcol stragglers)
                (split-list available-releases)
              (format t "~%~d other projects available from distribution: ~a~%~%"
                      (length available-releases) (name d))
              (write-threes leftcol midcol rightcol)
              (when stragglers (write-stragglers stragglers))))
          (terpri))
        (all-dists)))

;;; systems
(defun list-installed-systems ()
  (mapc (lambda (d)
          (let ((installed-systems (installed-systems d)))
            (multiple-value-bind (leftcol midcol rightcol stragglers)
                (split-list installed-systems)
              (format t "~%~d systems installed from distribution: ~a~%~%"
                      (length installed-systems) (name d))
              (write-threes leftcol midcol rightcol)
              (when stragglers (write-stragglers stragglers))))
          (terpri))
        (all-dists)))

(defun list-available-systems ()
  (mapc (lambda (d)
          (let* ((provided-systems (provided-systems d))
                 (installed-systems (installed-systems d))
                 (available-systems
                  (remove-if (lambda (r)
                               (member r installed-systems :test #'equal))
                             provided-systems)))
            (multiple-value-bind (leftcol midcol rightcol stragglers)
                (split-list available-systems)
              (format t "~%~d other systems available from distribution: ~a~%~%"
                      (length available-systems) (name d))
              (write-threes leftcol midcol rightcol)
              (when stragglers (write-stragglers stragglers))))
          (terpri))
        (all-dists)))

(defun swiqlisp-apropos (term)
  (let (matches)
    (flet ((matcher (system)
             (when (or (search term (name system))
                       (search term (name (release system))))
               (push system matches))))
      (mapc #'matcher (provided-systems t))
      (when matches
        (format t "~%~d system matches found:~%~%" (length matches))
        (multiple-value-bind (leftcol midcol rightcol stragglers)
            (split-list (reverse matches))
          (write-threes leftcol midcol rightcol)
          (when stragglers (write-stragglers stragglers)))
        (terpri)))))


;;; ============================================================================
;;; install functions

;;; projects
(defun project-existsp (project-name)
  (find-release project-name))

(defun project-installedp (project-name)
  (let ((project-obj (find-release project-name)))
    (installedp project-obj)))

(defun install-project (project-name)
  (if (project-existsp project-name)
      (if (project-installedp project-name)
          (format t "~%Project ~a is already installed.~%~%" project-name)
        ;; install each system provided by the project in turn
        (dolist (system (provided-systems (find-release project-name))
                 (format t "~%Project ~a successfully installed.~%~%" project-name))
          (unless (installedp (find-system (name system)))
            (ql:quickload (name system)))))
    (format t "~%Project ~a not found.~%~%" project-name)))

;;; systems
(defun system-existsp (system-name)
  (find-system system-name))

(defun system-installedp (system-name)
  (let ((system-obj (find-system system-name)))
    (installedp system-obj)))

(defun install-system (system-name)
  (if (system-existsp system-name)
      (if (system-installedp system-name)
          (format t "~%System ~a is already installed.~%~%" system-name)
        (ql:quickload system-name))
    (format t "~%System ~a not found.~%~%" system-name)))
