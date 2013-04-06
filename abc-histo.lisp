(defpackage :abc-histo
  (:use :cl :lisp-unit))

(in-package :abc-histo)
(defvar *db*)

(defclass memory-db ()
  ((words :initform (make-hash-table :test 'equal)
          :accessor words)
   (topics :initform (make-hash-table :test 'equal)
           :accessor topics)))

(defun top (n alist)
  (subseq (sort alist #'> :key #'cdr) 0 n))

(defun top-histo (n &key topic (db *db*))
  (top n (histo :topic topic :db db)))

(defun add-dir (directory &optional (db *db*))
  (when (cl-fad:directory-exists-p directory)
    (cl-fad:walk-directory
     directory
     (lambda (file)
       (with-open-file (in file)
         (let ((topic (string-downcase (read-line in nil nil))))
           (loop for line = (read-line in nil nil)
              while line
              do (add-line line topic db))))))))

(defun histo (&key topic (db *db*))
  (if topic
      (alexandria:hash-table-alist (gethash (string-downcase topic) (topics db)))
      (histogram db)))

(defun search-histo (prefix &key topic (db *db*))
  (let ((prefix (string-downcase prefix)))
    (remove-if-not (lambda (word-count)
                     (alexandria:starts-with-subseq prefix (car word-count)))
                   (histo :topic topic :db db))))

(defun add (abcstring &key topic (db *db*))
  (add-line abcstring topic db))

(defun add-line (abcstring topic db)
  (let ((words (subseq (cl-ppcre:split "\\s+" abcstring) 1)))
    (dolist (word words)
      (if topic
          (add-word (string-downcase word) (string-downcase topic) db)
          (add-word (string-downcase word) nil db)))))

(defmethod histogram ((db memory-db))
  (alexandria:hash-table-alist (words db)))

(defmethod add-word (word topic (db memory-db))
  (incf (gethash word (words db) 0))
  (when topic
    (unless (gethash topic (topics db))
      (setf (gethash topic (topics db)) (make-hash-table :test 'equal)))
    (incf (gethash word (gethash topic (topics db)) 0))))

(defmethod count-of (key (db memory-db))
  (gethash key (words db)))

(remove-tests :all)

(define-test abc-histo-add
  (let ((*db* (make-instance 'memory-db))
        (data "A Arnold  Annette"))
    (add data)
    (assert-equal 1 (count-of "arnold" *db*))))

(define-test abc-histo-histogram
  (let ((*db* (make-instance 'memory-db))
        (data "A Arnold Annette annette"))
    (add data)
    (assert-equal 1 (cdr (assoc "arnold" (histo) :test #'string=)))
    (assert-equal 2 (cdr (assoc "annette" (histo) :test #'string=)))))

(define-test abc-search-topic
  (let ((*db* (make-instance 'memory-db)))
    (add "A Arnold Alice" :topic "Names")
    (add "A Anbei")
    (assert-true (assoc "arnold" (histo :topic "Names") :test #'string=))
    (assert-false (assoc "anbei" (histo :topic "Names") :test #'string=))
    (assert-true (search-histo "ar" :topic "names"))
    (assert-true (search-histo "An"))
    (assert-false (search-histo "an" :topic "names"))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
