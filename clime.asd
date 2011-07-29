;;; :FILE-CREATED <Timestamp: #{2011-07-26T15:54:41-04:00Z}#{11302} - by MON>
;;; :FILE clime/clime.asd
;;; ==============================

;; ,----
;; | "I am sick to death of knee-jerk anti-LOOPism and I am beginning to
;; |  irrationally regard it as a plot to disable me as a programmer by
;; |  excommunicating my useful tools."
;; |
;; |     :SOURCE "Knee-jerk Anti-LOOPism and other E-mail Phenomena" p 17 
;; `---- :SEE http://ccs.mit.edu/papers/CCSWP150.html


(defpackage #:clime-build-system (:use :common-lisp :asdf))

(in-package #:clime-build-system)

(defsystem :clime
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license "MIT" 
  :description "Copy files with image mime-types across file-system boundaries."
  :version "1.0.0"
  :depends-on (:cl-ppcre
               :cffi
               :osicat
               :magicffi
               :command-line-arguments
               );:buildapp)
  :serial t    
  :components
  ((:file "package"                  )
   (:file "clime-specials"           )
   (:file "clime-bail"               )
   (:file "clime-posix"              )
   (:file "clime-initialize"         )
   (:file "clime-walk-file-dir"      )
   (:file "clime-copy-bytes"         )
   (:file "clime-docs"               )))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
