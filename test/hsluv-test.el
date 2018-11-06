;;; hsluv-test.el --- hsluv color space conversions tests -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; This test package defines the functions to test the hsluv conversion code.

;;; Code:

(require 'json)
(require 'cl)

(defconst EPSILON 1.0e-12)

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

(defun hsluv--test-assert (hex conversion input expected actual)
  (if (not (float-arrays-equalp expected actual EPSILON))
      (progn
        (princ (concat hex " " conversion ":"))
        (terpri)
        (princ "    input   : ")
        (princ input)
        (terpri)
        (princ "    expected: ")
        (princ expected)
        (terpri)
        (princ "    got     : ")
        (princ actual)
        (terpri)
        1)
    0))

(defun hsluv--test (file)
  (let ((test-set (hsluv--read-test-file file)))
    (dolist (color test-set)
      (let* ((hex (car color))
             (lch)
             (luv)
             (rgb)
             (xyz)
             (hpluv)
             (hsluv)
             (errors 0))
        (dolist (cspace (cdr color))
          (let ((symbl (car cspace))
                (tuple (mapcar 'float (cdr cspace))))
            (cond ((equal "lch" symbl)
                   (setq lch tuple))
                  ((equal "luv" symbl)
                   (setq luv tuple))
                  ((equal "rgb" symbl)
                   (setq rgb tuple))
                  ((equal "xyz" symbl)
                   (setq xyz tuple))
                  ((equal "hpluv" symbl)
                   (setq hpluv tuple))
                  ((equal "hsluv" symbl)
                   (setq hsluv tuple))
                  (t
                   (error (concat hex ": Unexpected cspace '" symbl "'"))))))
        (setq errors (+ errors (hsluv--test-assert hex "hex-rgb" hex rgb (color-name-to-rgb hex))))
        (setq errors (+ errors (hsluv--test-assert hex "xyz-rgb" xyz rgb (hsluv-xyz-to-rgb xyz))))
        (setq errors (+ errors (hsluv--test-assert hex "rgb-xyz" rgb xyz (hsluv-rgb-to-xyz rgb))))
        (setq errors (+ errors (hsluv--test-assert hex "xyz-luv" xyz luv (hsluv-xyz-to-luv xyz))))
        (setq errors (+ errors (hsluv--test-assert hex "luv-xyz" luv xyz (hsluv-luv-to-xyz luv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-luv" lch luv (hsluv-lch-to-luv lch))))
        (setq errors (+ errors (hsluv--test-assert hex "luv-lch" luv lch (hsluv-luv-to-lch luv))))
        (setq errors (+ errors (hsluv--test-assert hex "hsluv-lch" hsluv lch (hsluv-hsluv-to-lch hsluv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-hsluv" lch hsluv (hsluv-lch-to-hsluv lch))))
        (setq errors (+ errors (hsluv--test-assert hex "hpluv-lch" hpluv lch (hsluv-hpluv-to-lch hpluv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-hpluv" lch hpluv (hsluv-lch-to-hpluv lch))))

        (when (> errors 0)
          (error (concat hex ": " (number-to-string errors) " conversion" (if (> errors 1) "s" "") " failed.")))
        errors))))

;;(hsluv--test "test/snapshot-rev4.json")

(defconst TESTDATA "/Users/gvermeiren/src/geertv/emacs-hsluv/test/snapshot-rev4.json")

(defun luvtest-dump (color)
  "Output COLOR to the console for debugging purposes."
  (pp color)
  ;;(pp (car color))
  ;;(pp (cdr (assoc "rgb" (cdr color))))
  )

;; (defmacro hsluv-deftest (color)
;;   "Generate a test for hsluv COLOR conversion."
;;   (macrolet ((testname (make-symbol (concat "test" (car color)))))
;;     `(ert-deftest ,testname ()
;;        (should (float-arrays-equalp (color-name-to-rgb (car color)) (cdr (assoc "rgb" (cdr color))) EPSILON)))))

(defmacro hsluv-deftest (color)
  "Generate a test named TESTNAME for hsluv COLOR conversion."
  ;; (let* ((suffix (substring (car color) 1))))
  `(ert-deftest ,(intern (concat "test-" (substring (car color) 1))) ()
     (should (float-arrays-equalp
              (color-name-to-rgb (car ,color))
              (cdr (assoc "rgb" (cdr ,color)))
              EPSILON))
     )
  )

;; (defun luvtest-single (color)
;;   "Test all conversions for one COLOR."
;;   ;;(luvtest-dump color)
;;   (hsluv-deftest color)
;;   )

(defun luvtest-single (color)
  (eval `(ert-deftest ,(intern (concat "test-" (substring (car color) 1))) ()
           (should (float-arrays-equalp
                    ,(color-name-to-rgb (car color))
                    ,(cdr (assoc "rgb" (cdr color)))
                    EPSILON))
           )))

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
