;;; :FILE-CREATED <Timestamp: #{2011-07-27T12:57:29-04:00Z}#{11303} - by MON>
;;; :FILE clime/clime-posix.lisp
;;; ==============================


(in-package #:clime)
;; *package*

(defun syslog-action (&key (log-message "<EMPTY-SBCL-CLIME-LOG-MESSAGE>")
                      (log-ident "sbcl-clime")
                      (log-priority 6)) ;; 6 indicates informational
  (declare (string log-message log-ident)
           ((mod 8) log-priority))
  (unwind-protect
       (progn
         (sb-posix:openlog log-ident
                           (or 
                            (and (string-equal log-ident "sbcl-slime")
                                 sb-posix:log-pid) 0)
                           sb-posix:log-user)
         (sb-posix:syslog log-priority log-message))
    (sb-posix:closelog)))

;; (defun %syslog-action-osicat (&key 
;;                               (log-message "<EMPTY-SBCL-CLIME-LOG-MESSAGE>")
;;                               (log-ident "sbcl-clime")
;;                               (log-priority 6)) ;; 6 indicates informational
;;   ;; (%syslog-action-osicat)
;;   ;; (%syslog-action-osicat :log-message "A different log message")
;;   ;; (%syslog-action-osicat :log-message "A different log message"  :log-ident "bubba")
;;   (declare (string log-message log-ident)
;;            ((mod 8) log-priority))
;;   (unwind-protect
;;        (progn
;;          (osicat-posix:openlog log-ident 
;;                                (or 
;;                                 (and (string-equal log-ident "sbcl-clime")
;;                                      osicat-posix:log-pid)
;;                                 0)
;;                                :user)
;;          (osicat-posix:syslog log-priority log-message))
;;     (osicat-posix:closelog)))

(defun clime-getenv (name) 
  ;; :EXAMPLE 
  ;; (map 'list #'osicat:pathname-as-directory (cl-ppcre:split ":" (sb-posix:getenv "PATH")))
  #+sbcl (sb-posix:getenv name)
  #-sbcl (osicat:environment-variable name))

;; :SOURCE (URL `git://git.feelingofgreen.ru/executor') :FILE executor/portable-spawn.lisp
(defun clime-setenv (variable value)
  #+ccl
  (ccl:setenv variable value t)
  #+ecl
  (si:setenv variable value)
  #+clisp
  (setf (ext:getenv variable) value)
  ;;
  ;; #+osicat (setf (osicat:environment-variable variable) value)
  ;;
  ;; :NOTE putenv on sbcl unix is unsetenv && setenv
  #+sbcl
  (sb-posix:putenv (concatenate 'string variable "=" value)))

(defun clime-getenv-path-pathnames ()
  ;; Return the pathnmames of directories for current environment varbiable $PATH.~%~@
  ;;:EXAMPLE~%
  ;;  \(clime-getenv-path-pathnames\)
  (let* ((env-path  #+sbcl (clime-getenv "PATH")
                    #-sbcl (osicat:environment-variable "PATH"))
         (env-split (and env-path (cl-ppcre:split ":" env-path))))
    (when env-split
      (setf env-split
            (delete-if #'(lambda (maybe-null-or-wild)
                           (or (null maybe-null-or-wild)
                               (wild-pathname-p maybe-null-or-wild))) env-split))
      (when env-split
        (map 'list #'osicat:pathname-as-directory env-split)))))

;; :NOTE The implementation equivalences are courtesy :FILE pergamum/feet-of-clay.lisp
;; :SEE (URL `git://git.feelingofgreen.ru/pergamum') 
(defun clime-get-posix-working-directory ()
  #-(or sbcl ecl ccl clisp) (osicat-posix::getcwd)
  #+ecl   (si:getcwd)
  #+ccl   (ccl::current-directory-name)
  #+clisp (ext:cd)
  #+sbcl  (sb-posix:getcwd))

;; :NOTE The implementation equivalences are courtesy :FILE pergamum/feet-of-clay.lisp
;; :SEE (URL `git://git.feelingofgreen.ru/pergamum') 
(defun clime-set-posix-working-directory (to-directory &key (return-as-pathname nil))
  (declare (boolean return-as-pathname))
  (values
   (zerop
    #-(or sbcl ecl ccl clisp) (osicat-posix:chdir to-directory)
    #+ecl   (si:chdir pathname)
    ;; :TODO verify if ccl:native-translated-namestring is the way to go here.
    ;; :SEE (URL `http://clozure.com/pipermail/openmcl-devel/2011-May/012812.html')
    ;; :SEE (URL `http://trac.clozure.com/ccl/ticket/632')
    ;; #+ccl   (ccl::%chdir (ccl:native-translated-namestring to-directory))
    #+ccl   (ccl::%chdir (namestring to-directory))
    ;; :NOTE clisp accepts NIL for arg TO-DIRECTORY, filter it? 
    #+clisp (ext:cd to-directory)
    #+sbcl  (sb-posix:chdir to-directory))
   (or 
    (and return-as-pathname 
         (pathname to-directory))
    to-directory)))

(defun (setf clime-get-posix-working-directory) (to-directory)
  (multiple-value-bind (bool set-pth)
      (clime-set-posix-working-directory to-directory)
    (values set-pth bool)))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
