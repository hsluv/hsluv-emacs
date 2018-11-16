;;; hsluv-test.el --- hsluv color space conversions tests -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; This test package defines the functions to test the hsluv conversion code.

;;; Code:

(require 'json)
(require 'cl)

(defconst hsluvtest--epsilon 1.0e-10)

(defun hsluv--read-test-file (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list)
        )
    (json-read-file file)))

(defun hsluvtest--tupples-epsilon-equalp (a1 a2 epsilon)
  "Determine if corresponding elements in A1 and A2 differ by at most EPSILON."
  (every (lambda (v) (<= v epsilon))
         (mapcar* (lambda (v1 v2) (abs (- v1 v2))) a1 a2)))

(defun hsluvtest--tupples-equalp (a1 a2)
  "Determine if tuples A1 and A2 are equal (i.e. differ by at most HSLUVTEST--EPSILON)."
  (hsluvtest--tupples-epsilon-equalp a1 a2 hsluvtest--epsilon))

(defun hsluvtest--expected-result (color space)
  "Return the expected SPACE result for COLOR."
  (cdr (assoc space (cdr color))))

(defun hsluvtest--generate-test-name (color)
  "Generate an interned symbol representing the testname for COLOR."
  (intern (concat "test" (substring (car color) 1))))

(defun hsluvtest--generate-ert-test (color)
  "Generate a test for COLOR."
  (let* ((hex (car color))
         (testname (hsluvtest--generate-test-name color))
         (lch (hsluvtest--expected-result color "lch"))
         (luv (hsluvtest--expected-result color "luv"))
         (rgb (hsluvtest--expected-result color "rgb"))
         (xyz (hsluvtest--expected-result color "xyz"))
         (hpluv (hsluvtest--expected-result color "hpluv"))
         (hsluv (hsluvtest--expected-result color "hsluv")))
    (eval `(ert-deftest ,testname ()
             (should (hsluvtest--tupples-equalp (hsluv-hex-to-rgb ,hex) ',rgb))
             (should (hsluvtest--tupples-equalp (hsluv-xyz-to-rgb ',xyz) ',rgb))
             (should (hsluvtest--tupples-equalp (hsluv-rgb-to-xyz ',rgb) ',xyz))
             (should (hsluvtest--tupples-equalp (hsluv-xyz-to-luv ',xyz) ',luv))
             (should (hsluvtest--tupples-equalp (hsluv-luv-to-xyz ',luv) ',xyz))
             (should (hsluvtest--tupples-equalp (hsluv-lch-to-luv ',lch) ',luv))
             (should (hsluvtest--tupples-equalp (hsluv-luv-to-lch ',luv) ',lch))
             (should (hsluvtest--tupples-equalp (hsluv-hsluv-to-lch ',hsluv) ',lch))
             (should (hsluvtest--tupples-equalp (hsluv-lch-to-hsluv ',lch) ',hsluv))
             (should (hsluvtest--tupples-equalp (hsluv-hpluv-to-lch ',hpluv) ',lch))
             (should (hsluvtest--tupples-equalp (hsluv-lch-to-hpluv ',lch) ',hpluv))
             ))))

(defun hsluvtest (file)
  "Load expected conversions from FILE and test them 1 by 1."
  (mapc 'hsluvtest--generate-ert-test (hsluv--read-test-file file)))


(provide 'hsluv-test)
;;; hsluv-test.el ends here
