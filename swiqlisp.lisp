;;; ~/github/sebyte/swiQlisp/swiqlisp.lisp

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
;;;  This is the only lisp source file.  The rest of swiQlisp consists of shell
;;;  scripts.

(use-package "QL-DIST")


;;; ============================================================================
;;; utility functions

(defun string-truncate (str &optional (width 25))
  (if (<= (length str) width)
      str
    (format nil "~a..." (subseq str 0 (- width 3)))))

(defun groups-of-three (l)
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

(defun 3col-write (objects &key (col-width 25) (reader #'identity))
  (let ((3col-format (format nil " ~~~da ~~~da ~~~da~%"
                             col-width col-width col-width))
        (straggler-format (format nil " ~~~da" col-width)))
    (multiple-value-bind (leftcol midcol rightcol stragglers)
        (groups-of-three
         (sort objects (lambda (a b)
                         (when (string< (funcall reader a)
                                        (funcall reader b))
                           t))))
      ;; threes
      (mapc (lambda (objX objY objZ)
              (format t 3col-format
                      (string-truncate (funcall reader objX))
                      (string-truncate (funcall reader objY))
                      (string-truncate (funcall reader objZ))))
            leftcol midcol rightcol)
      ;; stragglers (at most two)
      (when (car stragglers)
        (format t straggler-format
                (string-truncate (funcall reader (car stragglers))))
        (when (cadr stragglers)
          (format t straggler-format
                  (string-truncate (funcall reader (cadr stragglers)))))
        (terpri))
      (terpri))))

(defun disk-file-to-list (file)
  (let (lines)
    (with-open-file (filestream file)
      (do ((line (read-line filestream nil 'eof) (read-line filestream nil 'eof)))
          ((eq line 'eof))
        (push line lines)))
    (reverse lines)))

(defun system-exists-p (system-name)
  (find-system system-name))

(defun release-exists-p (release-name)
  (find-release release-name))

(defun system-installed-p (system-name)
  (let ((system-obj (find-system system-name)))
    (installedp system-obj)))

(defun fetch-system-plus-dependencies (system-name)
  ;; fetch 'main' system
  (if (asdf:find-system system-name nil) ; use ASDF's FIND-SYSTEM to allow for
                                         ; sb-* systems, and allow missing
      (format t "~%System ~a is already installed." system-name)
    (install (find-system system-name)))
  ;; now fetch dependencies
  (let ((dependencies
         ;; COMPONENT-DEPENDS-ON returns a list within a list.  The first item
         ;; in the inner list in the name of the class of operation, in this
         ;; case LOAD-SOURCE-OP, so we need to drop that and convert the
         ;; remaining items to lowercase strings
         (map
          'list
          #'string-downcase
          (cdar ; remove outer list and first entry of inner list
           (asdf:component-depends-on
            'asdf:load-source-op
            (asdf:find-system system-name))))))
    ;; uncomment to debug
    ;(format t "~%~s~%" dependencies)
    (dolist (component dependencies)
      ;; recurse
      (fetch-system-plus-dependencies component))))


;;; ============================================================================
;;; top level query functions

;; systems
(defun list-installed-systems ()
  (mapc (lambda (d)
          (let ((installed-systems (installed-systems d)))
            (format t "~%~d system~:p installed from distribution: ~a~%~%"
                    (length installed-systems) (name d))
            (3col-write installed-systems :reader #'name)))
        (all-dists)))

(defun list-available-systems ()
  (mapc (lambda (d)
          (let* ((provided-systems (provided-systems d))
                 (installed-systems (installed-systems d))
                 (available-systems
                  (remove-if (lambda (r)
                               (member r installed-systems :test #'equal))
                             provided-systems)))
            (format t "~%~d other system~:p available from distribution: ~a~%~%"
                    (length available-systems) (name d))
            (3col-write available-systems :reader #'name)))
        (all-dists)))

(defun list-release-systems (release-name)
  (if (release-exists-p release-name)
      (let ((release-systems (provided-systems (find-release release-name))))
        (format t "~%The ~a release includes ~d system~:p:~%~%"
                release-name (length release-systems))
        (3col-write release-systems :reader #'name))
    (format t "~%Release ~a not found.~%~%" release-name)))

(defun list-dependencies (system-name)
  (if (system-exists-p system-name)
      (let* ((direct-dependencies (required-systems (find-system system-name)))
             (all-dependencies direct-dependencies))
        (labels ((find-sub-dependencies (system-names)
                   (mapc
                    (lambda (sys-name)
                      (pushnew sys-name all-dependencies :test #'equal)
                      (find-sub-dependencies ; recursively
                       (required-systems (find-system sys-name))))
                    system-names)))
          (find-sub-dependencies direct-dependencies)
          (format t "~%~a depends on the following ~d systems:~%~%"
                  system-name (length all-dependencies))
          (3col-write all-dependencies)))
    (format t "~%System ~a not found.~%~%" system-name)))

(defun swiqlisp-apropos (term)
  (let (matches)
    (flet ((matcher (system)
             (when (or (search term (name system))
                       (search term (name (release system))))
               (push system matches))))
      (mapc #'matcher (provided-systems t))
      (format t "~%~d matching system~:p found:~%~%" (length matches))
      (3col-write matches :reader #'name))))

;; releases
(defun list-installed-releases ()
  (mapc (lambda (d)
          (let ((installed-releases (installed-releases d)))
            (format t "~%~d release~:p installed from distribution: ~a~%~%"
                    (length installed-releases) (name d))
            (3col-write installed-releases :reader #'name)))
        (all-dists)))

(defun list-available-releases ()
  (mapc (lambda (d)
          (let* ((provided-releases (provided-releases d))
                 (installed-releases (installed-releases d))
                 (available-releases
                  (remove-if (lambda (r)
                               (member r installed-releases :test #'equal))
                             provided-releases)))
            (format t "~%~d other release~:p available from distribution: ~a~%~%"
                    (length available-releases) (name d))
            (3col-write available-releases :reader #'name)))
        (all-dists)))

;; dist versions
(defun versions ()
  (let ((versions (available-versions (dist "quicklisp"))))
    (format t "~%~{ ~a~%~}" (mapcar #'cdr (reverse versions)))))


;;; ============================================================================
;;; top level (un)installation functions

(defun install-system (system-name &optional no-compile-p)
  (if (system-exists-p system-name)
      (if (system-installed-p system-name)
          (format t "~%System ~a is already installed.~%~%" system-name)
        (if no-compile-p
            (fetch-system-plus-dependencies system-name)
          (ql:quickload system-name)))
    (format t "~%System ~a not found.~%~%" system-name)))

(defun additional-systems-report ()
  (let ((newly-installed-systems
         (disk-file-to-list (ql:qmerge #p"../tmp/installed-systems.added"))))
    (format t "~%~d newly installed system~:p now available.~%~%"
            (length newly-installed-systems))
    (3col-write newly-installed-systems :reader #'pathname-name)))

(defun uninstall-system (system-name)
  (if (system-exists-p system-name)
      (if (system-installed-p system-name)
          (uninstall (find-system system-name))
        (format t "~%System ~a is not installed.~%~%" system-name))
    (format t "~%System ~a not found.~%~%" system-name)))

(defun removed-systems-report ()
  (let ((newly-removed-systems
         (disk-file-to-list (ql:qmerge #p"../tmp/installed-systems.removed"))))
    (format t "~%~d newly removed system~:p no longer available.~%~%"
            (length newly-removed-systems))
    (3col-write newly-removed-systems :reader #'pathname-name)))


;;; ============================================================================
;;; {up|down}date functions

(defun self-update () (ql:update-client))

(defun update-systems () (ql:update-all-dists))

(defun downdate-systems (dist) (install-dist dist :replace t))


;;; ============================================================================
;;; misc

(defun delete-old-systems () (mapc #'ql-dist::clean (all-dists)))
