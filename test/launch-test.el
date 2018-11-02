(require 'hsluv)
(require 'hsluv-test)
(when (> (hsluv--test "test/snapshot-rev4.json") 0)
  (error (kill-emacs 1)))
