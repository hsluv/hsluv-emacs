;;; hsluv-test.el --- hsluv color space conversions tests -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; This test package defines the functions to test the hsluv conversion code.

;;; Code:

(require 'json)
(require 'cl)

(defconst EPSILON 1.0e-10)

(defun hsluv--read-test-file (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list)
        )
    (json-read-file file)))

(defun tupples-equalp (a1 a2)
  (float-arrays-equalp a1 a2 EPSILON))

(defun float-arrays-equalp (a1 a2 epsilon)
  "A1 A2 EPSILON."
  (every (lambda (v) (<= v epsilon))
         (mapcar* (lambda (v1 v2) (abs (- v1 v2))) a1 a2)))

;;(hsluv--test "test/snapshot-rev4.json")

(defun luvtest-dump (color)
  "Output COLOR to the console for debugging purposes."
  (pp color)
  ;;(pp (car color))
  ;;(pp (cdr (assoc "rgb" (cdr color))))
  )

(defun luvtest-expected-result (color space)
  "Return the expected SPACE result for COLOR."
  (cdr (assoc space (cdr color))))

(defun luvtest-generate-test-name (color)
  "Generate an interned symbol representing the testname for COLOR."
  (intern (concat "test" (substring (car color) 1))))

(defmacro should-tupples (from to)
  "Generate a should clause FROM TO."
  `(should (tupples-equalp (,(intern (concat "hsluv-" from "-to-" to)) ,(intern from)) ,(intern to))))

(defun luvtest-gentest (color)
  "Generate a test for COLOR."
  (let* ((hex (car color))
         (testname (luvtest-generate-test-name color))
         (lch (luvtest-expected-result color "lch"))
         (luv (luvtest-expected-result color "luv"))
         (rgb (luvtest-expected-result color "rgb"))
         (xyz (luvtest-expected-result color "xyz"))
         (hpluv (luvtest-expected-result color "hpluv"))
         (hsluv (luvtest-expected-result color "hsluv")))
    (eval `(ert-deftest ,testname ()
             (should (tupples-equalp (hsluv-hex-to-rgb ,hex) ',rgb))
             (should (tupples-equalp (hsluv-xyz-to-rgb ',xyz) ',rgb))
             (should (tupples-equalp (hsluv-rgb-to-xyz ',rgb) ',xyz))
             (should (tupples-equalp (hsluv-xyz-to-luv ',xyz) ',luv))
             (should (tupples-equalp (hsluv-luv-to-xyz ',luv) ',xyz))
             (should (tupples-equalp (hsluv-lch-to-luv ',lch) ',luv))
             (should (tupples-equalp (hsluv-luv-to-lch ',luv) ',lch))
             (should (tupples-equalp (hsluv-hsluv-to-lch ',hsluv) ',lch))
             (should (tupples-equalp (hsluv-lch-to-hsluv ',lch) ',hsluv))
             (should (tupples-equalp (hsluv-hpluv-to-lch ',hpluv) ',lch))
             (should (tupples-equalp (hsluv-lch-to-hpluv ',lch) ',hpluv))
             ))))

;; let's try something completely different ;-)
(defun luvtest (file)
  "Load expected conversions from FILE and test them 1 by 1."
  ;;(mapc 'luvtest-single (hsluv--read-test-file file))
  (mapc 'luvtest-gentest (hsluv--read-test-file file))
  )


(provide 'hsluv-test)
;;; hsluv-test.el ends here
