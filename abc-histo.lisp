(defpackage :abc-histo
  (:use :cl :lisp-unit))

(in-package :abc-histo)
(defvar *db*)

(defclass memory-db ()
  ((words :initform (make-hash-table :test 'equal)
          :accessor words)))

(defun add-dir (directory &optional (db *db*))
  (when (cl-fad:directory-exists-p directory)
    (cl-fad:walk-directory
     directory
     (lambda (file)
       (with-open-file (in file)
         (read-line in nil nil)
         (loop for line = (read-line in nil nil)
            while line
            do (add-line line db)))))))

(defun histo (&optional (db *db*))
  (histogram db))

(defun add (abcstring &optional (db *db*))
  (add-line abcstring db))

(defun add-line (abcstring db)
  (let ((words (subseq (cl-ppcre:split "\\s+" abcstring) 1)))
    (dolist (word words)
      (add-word (string-downcase word) db))))

(defmethod histogram ((db memory-db))
  (alexandria:hash-table-alist (words db)))

(defmethod add-word (word (db memory-db))
  (incf (gethash word (words db) 0)))

(defmethod count-of (key (db memory-db))
  (gethash key (words db)))

(remove-tests :all)

(define-test abc-histo-add
  (let ((*db* (make-instance 'memory-db))
        (data "A Arnold  Annette"))
    (add data *db*)
    (assert-equal 1 (count-of "arnold" *db*))))

(define-test abc-histo-histogram
  (let ((*db* (make-instance 'memory-db))
        (data "A Arnold Annette annette"))
    (add data *db*)
    (assert-equal 1 (cdr (assoc "arnold" (histo) :test #'string=)))
    (assert-equal 2 (cdr (assoc "annette" (histo) :test #'string=)))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
