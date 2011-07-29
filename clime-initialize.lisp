;;; :FILE-CREATED <Timestamp: #{2011-07-26T17:31:48-04:00Z}#{11302} - by MON>
;;; :FILE clime/clime-initialize.lisp
;;; ==============================

;;; ==============================
;;; Initialize the environment according to the command-line args.
;;; No file/dir inspection or processing on either of the local or remote host
;;; occurs until the environment is verified sane.
;;; If it isn't, when *IS-BUILDAPP-P* exit immediately, else do something sensible.
;;; ==============================

(in-package #:clime)
;; *package*

(defun get-command-arguments (&key (stream *standard-output*))
  (check-clime-help-argument :stream stream)
  (command-line-arguments:process-command-line-options
   *CLI-SPECIFICATION* 
   (command-line-arguments:get-command-line-arguments)))

(defun check-clime-help-argument (&key (stream *standard-output*))
  (unless (zerop 
           (list-length 
            (intersection '("-h"  "help" "-help" "--help" "usage" "-usage" "--usage") ;; "?" "-?" 
                          sb-ext:*posix-argv* :test #'string-equal)))
    (unwind-protect
         (show-clime-help :stream stream)
      (when *IS-BUILDAPP-P* 
        (sb-ext:quit :unix-status 0)))))

(defun show-clime-help (&key (stream *standard-output*))
  (command-line-arguments:show-option-help *CLI-SPECIFICATION* :stream stream))

(defun verify-local-mount-command-argument (keyword key-val-arglist &key (required nil) 
                                                                         (stream *standard-output*))
  (declare (boolean required))
  (let ((get-val (getf key-val-arglist keyword 'not-present)))
    (when (eq get-val 'not-present)
      (format stream "command line argument ~(`~A'~) not found~%" keyword)
      (or 
       (failed-function-report-and-bail "verify-local-mount-command-argument" :stream stream :exit-status 1)
       (return-from verify-local-mount-command-argument nil)))
    (if get-val
        get-val
        (when required
          (format stream "Required command line argument ~(~A~) not found or no value~%" keyword)
          (or 
           (failed-function-report-and-bail "verify-local-mount-command-argument" :stream stream :exit-status 1)
           (return-from verify-local-mount-command-argument nil))))))

(defun mountpoint-p (putuative-mountpoint &key (stream *standard-output*))
  (declare (string putuative-mountpoint))
  (let ((status (sb-ext:run-program  "mountpoint" (list "-q" putuative-mountpoint) :search t)))
    (if (zerop (sb-ext:process-exit-code status))
        putuative-mountpoint
        ;; Is SB-EXT:QUIT the preferred interface for buildapp/cl-launch?
        (progn 
          (format stream "~%Call to external program `mountpoint` did not find valid mountpoint for: ~a" putuative-mountpoint)
          (or 
           (failed-function-report-and-bail "mountpoint-p" :stream stream :exit-status 1)
           (return-from mountpoint-p nil))))))

(defun verify-mount-namestrings (pathname-or-namestring &key (stream *standard-output*))  
  (unless (or (stringp pathname-or-namestring)
              (pathnamep pathname-or-namestring))
    (format stream "~%Arg PATHNAME-OR-NAMESTRING neither cl:stringp or `cl:pathnamep', got: ~A" pathname-or-namestring)
    (or (failed-function-report-and-bail "verify-mount-namestrings" :stream stream :exit-status 1)
        (return-from verify-mount-namestrings nil)))
  ;; (let* ((make-native (osicat-sys:native-namestring pathname-or-namestring))
  ;;        (native-kind (and make-native (osicat:file-kind make-native :follow-symlinks nil))))
  (let* ((make-native (sb-ext:native-namestring pathname-or-namestring))
         (native-kind (and make-native (sb-impl::native-file-kind make-native))))
    (if (eq native-kind :directory)
        (progn 
          ;; (setf make-native (namestring (cl-fad:pathname-as-directory make-native)))
          (setf make-native (namestring (osicat:pathname-as-directory  make-native)))
          ;; Don't allow top level directories, e.g. "/"
          (if (> (length make-native) 1) 
              (mountpoint-p make-native :stream stream)
              (progn 
                (format stream  "~%Arg PATHNAME-OR-NAMESTRING must have length greather than 1~%got: ~A" pathname-or-namestring)
                (or 
                 (failed-function-report-and-bail "verify-mount-namestrings" :stream stream :exit-status 1)
                 (return-from verify-mount-namestrings nil)))))
        (progn 
          (format stream  "~%Arg PATHNAME-OR-NAMESTRING not a directory, got: ~A" pathname-or-namestring)
          (or 
           (failed-function-report-and-bail "verify-mount-namestrings" :stream stream :exit-status 1)
           (return-from verify-mount-namestrings nil))))))

(defun set-base-mount-parameter-namestring (special-param pathname-or-namestring &key (stream *standard-output*))
  (declare (special special-param))
  (unless (or (pathnamep pathname-or-namestring)
              (stringp pathname-or-namestring))
    (format stream "~%Arg PATHNAME-OR-NAMESTRING either not pathnamep or not cl:stringp, got: ~A" pathname-or-namestring)
    (or (failed-function-report-and-bail "set-base-mount-parameter-namestring" :stream stream :exit-status 1)
        (return-from set-base-mount-parameter-namestring nil)))
  (let ((mount-string (verify-mount-namestrings pathname-or-namestring)))
    (and mount-string (set special-param mount-string))))

(defun parse-mountpoint-directory-components (mountpoint-namestring)
  (declare (string mountpoint-namestring))
  (pathname-directory (sb-ext:parse-native-namestring mountpoint-namestring)))

;; (set-base-mount-parameter-pathname-component '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-MOUNT-NAMESTRING*)
;; (set-base-mount-parameter-pathname-component '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-MOUNT-NAMESTRING*)
(defun set-base-mount-parameter-pathname-component (base-component-param base-mount-param &key (stream *standard-output*))
  (declare (special base-component-param base-mount-param))
  (let* ((bound-and-true (and (boundp base-mount-param)
                              (symbol-value base-mount-param)))
         (bound-and-string (and bound-and-true
                                (stringp bound-and-true)
                                bound-and-true)))
    (unless bound-and-string
      (format stream "~%Arg BASE-MOUNT-PARAM either not cl:boundp or its cl:symbol-value not cl:strinp, got: ~A" base-mount-param)
      (or (failed-function-report-and-bail "set-base-mount-parameter-pathname-component" :stream stream :exit-status 1)
          (return-from set-base-mount-parameter-pathname-component nil)))
    (set base-component-param (parse-mountpoint-directory-components bound-and-string))))

(defun split-subdir-paths (subdir-path-string)
  (remove-if #'(lambda (x) (zerop (length x))) (cl-ppcre:split "/" subdir-path-string )))

(defun set-base-mount-parameter-pathname-sub-component (special-sub-comp-param subdir-path-string)
  ;; (set-base-mount-parameter-pathname-sub-component '*LOCAL-DIRECTORY-SUB-COMPONENTS* "some/sub/dir")
  (let ((split-namestring (split-subdir-paths subdir-path-string)))
    (set special-sub-comp-param split-namestring)))

(defun valid-pathname-token-p (token)
  (unless (stringp token)
    (return-from valid-pathname-token-p nil))
  (labels ((valid-trailing-char (string)
             (declare (string string))
             (case (char string (1- (length string)))
               ;; not completely exaustive ... 
               ((#\/ #\. #\* #\Nul #\Space #\Newline #\TAB) nil)
               (t t)))
           (valid-leading-char (string)
             (declare (string string))
             (case (char string 0)
               ((#\/ #\. #\* #\Nul #\Space #\Newline #\TAB) nil)
               (t t)))
           (invalid-char (char)
             (declare (character char))
             (char= char #\/))
           (valid-interior-char (string)
             (declare (string string))
             (notany #'invalid-char string))
           (valid-path-token-p (token)
             (declare (string token))
             (>= (length token) 1)
             (valid-trailing-char token)
             (valid-leading-char token)
             (valid-interior-char token)))
    (declare (string token))    
    (valid-path-token-p token)))

;;; ==============================
;; :NOTE The check around the local var chk-ldcp is mostly redundant b/c
;; LOCAL-DIRECTORY-COMPONENT-PARAM _should_ have been
;; bound around: `set-base-mount-parameter-pathname-component'
;; `set-base-mount-parameter-namestring'... 
;; However, we go ahead and do it to make sure we're not landing in a root or top level dir
;; b/c it is feasible to have LOCAL-DIRECTORY-COMPONENT-PARAM be bound nil 
;; In which case, there are no subdirectories beneath LOCAL-DIRECTORY-COMPONENT-PARAM
;; this is why we check above that LOCAL-DIRECTORY-COMPONENT-PARAM has > length 1.
(defun verify-directory-base-for-regexp (local-directory-component-param
                                        local-directory-sub-component-param
                                        &key (stream *standard-output*))
  (declare (special local-directory-component-param
                    local-directory-sub-component-param))
  (let* ((get-ldcp (and (boundp local-directory-component-param)
                        (symbol-value local-directory-component-param)))
         (chk-ldcp (and get-ldcp
                        (listp get-ldcp)
                        (eq (car get-ldcp) :ABSOLUTE)
                        (> (length get-ldcp) 1)
                        (and (every #'stringp (cdr get-ldcp)) get-ldcp)))
         (get-sub  (boundp local-directory-sub-component-param))
         (chk-sub  (when get-sub
                     (let ((get-sub-val (symbol-value local-directory-sub-component-param)))
                       (if (null get-sub-val)
                           t
                           (and (every #'stringp get-sub-val)
                                (every #'valid-pathname-token-p get-sub-val)
                                get-sub-val)))))
         (chk-namestring-after '()))
    (unless chk-sub
      (format stream "~&Arg LOCAL-DIRECTORY-SUB-COMPONENT-PARAM either unbound or ~
                        contains elt not `valid-path-token-p'~% got: ~A~%"
              local-directory-sub-component-param)
      (or 
       (failed-function-report-and-bail "verify-directory-base-for-regexp" :stream stream :exit-status 1)
       (return-from verify-directory-base-for-regexp nil)))
    (if (eq chk-sub t)
        (setq chk-namestring-after (namestring  (make-pathname :directory `(,@chk-ldcp))))
        (setq chk-namestring-after (namestring  (make-pathname :directory `(,@chk-ldcp ,@chk-sub)))))
    ;; (unless (eq (osciat:file-kind chk-namestring-after :follow-symlinks ???) :directory)
    (unless (eq (sb-impl::native-file-kind chk-namestring-after) :directory)
      (format stream "Namestring returned from merging path components does not name an existing directory~%~T~
                      generated namestring: ~A~%~T~
                      for LOCAL-DIRECTORY-COMPONENT-PARAM got: ~A~%~T~
                      for LOCAL-DIRECTORY-SUB-COMPONENT-PARAM got: ~A~%"
              chk-namestring-after local-directory-sub-component-param local-directory-sub-component-param)
      (or 
       (failed-function-report-and-bail "verify-directory-base-for-regexp" :stream stream :exit-status 1)
       (return-from verify-directory-base-for-regexp nil)))
    chk-namestring-after))

(defun set-local-directory-base-regexp (local-directory-base-regexp-param
                                        local-directory-component-param-for-regexp
                                        local-directory-sub-component-param-for-regexp
                                        &key (stream *standard-output*))
  (declare (special local-directory-base-regexp-param             
                    local-directory-component-param-for-regexp    
                    local-directory-sub-component-param-for-regexp))
  (let* ((chk-namestring     (verify-directory-base-for-regexp local-directory-component-param-for-regexp    
                                                               local-directory-sub-component-param-for-regexp
                                                               :stream stream))
         (namestring-scanner (and chk-namestring (cl-ppcre:create-scanner chk-namestring))))
    (if namestring-scanner
        (set local-directory-base-regexp-param namestring-scanner)
        (progn
          (format stream "failed to set variable ~A~%~T~
                          with arg LOCAL-DIRECTORY-COMPONENT-PARAM-FOR-REGEXP, got: ~A~%~T~
                          with arg LOCAL-DIRECTORY-SUB-COMPONENT-PARAM-FOR-REGEXP, got: ~A~%"
                  local-directory-base-regexp-param             
                  local-directory-component-param-for-regexp    
                  local-directory-sub-component-param-for-regexp)
          (or 
           (failed-function-report-and-bail "set-local-directory-base-regexp" :stream stream :exit-status 1)
           (return-from set-local-directory-base-regexp nil))))))

(defun set-remote-directory-base-namestring (remote-directory-base-namestring-param
                                             remote-directory-component-param
                                             remote-directory-sub-component-param
                                             &key (stream *standard-output*))
  (declare (special remote-directory-base-namestring-param
                    remote-directory-component-param
                    remote-directory-sub-component-param))
  (let ((chk-namestring (verify-directory-base-for-regexp remote-directory-component-param
                                                          remote-directory-sub-component-param
                                                          :stream stream)))
    ;; This is mostly redundant we should have already errored in `verify-directory-base-for-regexp'
    (if chk-namestring
        (set remote-directory-base-namestring-param chk-namestring)
        (progn
          (format stream "failed to set variable ~A~%~T~
                          with arg REMOTE-DIRECTORY-COMPONENT-PARAM, got: ~A~%~T~
                          with arg REMOTE-DIRECTORY-SUB-COMPONENT-PARAM, got: ~A~%"
                  remote-directory-base-namestring-param 
                  remote-directory-component-param 
                  remote-directory-sub-component-param)
          (or 
           (failed-function-report-and-bail "set-remote-directory-base-namestring" :stream stream :exit-status 1)
           (return-from set-remote-directory-base-namestring nil))))))

(defun set-local-directory-base-regexp-and-remote-namestring (loc-dir-base-re-pram loc-dir-cmpt-re-param loc-dir-sub-re-param
                                                              rmt-dir-base-nm-param rmt-dir-cmpt-param  rmt-dir-sub-param
                                                              &key (stream *standard-output*))
  (set-local-directory-base-regexp      loc-dir-base-re-pram  loc-dir-cmpt-re-param loc-dir-sub-re-param :stream stream)
  (set-remote-directory-base-namestring rmt-dir-base-nm-param rmt-dir-cmpt-param    rmt-dir-sub-param    :stream stream))

(defun set-parameter-spec-with-command-arguments (command-arguments parameter-spec &key (stream *standard-output*))
  (loop 
     with arglist = command-arguments
     for (key req action param) in parameter-spec
     for get-arg = (verify-local-mount-command-argument key arglist :required req :stream stream)
     do (funcall action param get-arg)
     (format stream "with command arg ~(`~A'~) setting variable ~A ~%~T now bound to: ~S~%" 
             (string key) param (symbol-value param)))

  (set-base-mount-parameter-pathname-component '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-MOUNT-NAMESTRING*  :stream stream)

  (set-base-mount-parameter-pathname-component '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-MOUNT-NAMESTRING* :stream stream)

  (set-local-directory-base-regexp-and-remote-namestring  
   '*LOCAL-DIRECTORY-BASE-REGEXP*      '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*
   '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*
   :stream stream)

  (if *IS-BUILDAPP-P*
      (values)
      (loop for sym in (list '*LOCAL-MOUNT-NAMESTRING*
                             '*LOCAL-DIRECTORY-COMPONENT*
                             '*LOCAL-DIRECTORY-SUB-COMPONENTS*
                             '*LOCAL-DIRECTORY-BASE-REGEXP*
                             ;;
                             '*REMOTE-MOUNT-NAMESTRING*
                             '*REMOTE-DIRECTORY-COMPONENT*
                             '*REMOTE-DIRECTORY-SUB-COMPONENTS*
                             '*REMOTE-DIRECTORY-BASE-NAMESTRING*)
         for str = (string sym)
         for val = (symbol-value sym)
         collect (list str val) into report
         finally (return report))))

;;; ==============================
(defun clime-main (argv &key (stream *standard-output*))
  (declare (ignore argv))
  ;; No. Do this from buildapp with --eval (setq clime:*IS-BUILDAPP-P* t)
  ;; (setq clime:*IS-BUILDAPP-P* t) 
  (set-parameter-spec-with-command-arguments (get-command-arguments :stream stream) 
                                             *CLI-TO-VARIABLE-SPEC* 
                                             :stream *standard-output*)
  ;; ( ... do more clime stuff here ... )
  )


;;; ==============================
;; deprecated
(defun verify-local-remote-mountpoints (local-mount remote-mount &key (stream *standard-output*))
  (and 
   (mountpoint-p local-mount  :stream stream)
   (mountpoint-p remote-mount :stream stream)))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
