;;; :FILE-CREATED <Timestamp: #{2011-07-26T17:21:05-04:00Z}#{11302} - by MON>
;;; :FILE clime/clime-bail.lisp
;;; ==============================

;;; ==============================
;;; Establish some error reporting/loggin functions
;;; ==============================


(in-package #:clime)
;; *package*

(defun failed-function-report-and-bail (function-name &key (stream *standard-output*) (exit-status 0))
 (when *IS-BUILDAPP-P* 
   (progn
     (format stream "~&:FUNCTION `~A' -- exiting now" function-name)
     (sb-ext:quit :unix-status exit-status))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
