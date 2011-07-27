;;; :FILE-CREATED <Timestamp: #{2011-07-11T14:35:18-04:00Z}#{11281} - by MON>
;;; :FILE clime/mime-walk-file-dir.lisp
;;; ==============================



(in-package #:clime)
;; *package

;; :TODO It would be nice if we added more ability to specify mime-types other than image charsets other than binary.
;; It isn't clear whether we should provide a generalized interface "with knobs on" or individualized functions...
;; Regardless if filter-files-for-image-mime-type were to become the template
;; for a generalized interface it would require a total refactoring.
;;
;; Currently there is a hardwired check for the return value of
;; magicffi:magic-file that tests for the presence of the following strings:
;;  "image" and "binary" "charset"
;; These checks occur in the locally defined functions binary-check and
;; image-check with image-and-binary-p as their caller:
;;
;; image-and-binary-p
;;  `-> image-check   -- checks for presences of "image" string and verifies its
;;                       associated value is a member of valid-mime-types which defaults to
;;                       *FILE-VALID-IMAGE-MIME-TYPES*
;; `-> binary-check  -- checks for presence of "charset" with "binary" as its value.
;;
;; :NOTE The binary-check is practically mandatory if we intend to allow peeking
;; into a compress/gzip'd archive independent of whether the mime-type is
;; "image/FOO" or some other thingy b/c damn near anything can/is be(ing) compressed...
(defun filter-files-for-image-mime-type (image-file-list &key substitution-fun
                                                              (check-compress nil) 
                                                              (valid-mime-types *FILE-VALID-IMAGE-MIME-TYPES*)
                                                              (file-hashtable *FILE-MIME-TABLE*)                                         
                                                              (stream *standard-output*))
  ;; (declare (special *FILE-VALID-IMAGE-MIME-TYPES*))
  ;; :NOTE The use of CL:LET* will force an immediate exit via GATHER-NATIVE-FILES-IF when *IS-BUILDAPP-P*
  (unless (and (hash-table-p file-hashtable)
               (member (hash-table-test file-hashtable) '(equal equalp)))
    (write-string "Keyword arg FILE-HASHATBLE either not `cl:hash-table-p' or has invalid `cl:hash-table-test'" stream)
    (or 
     (failed-function-report-and-bail "gather-native-files-if" :stream stream :exit-status 0)
     (return-from filter-files-for-image-mime-type nil)))
  (let* ((open-magic-list (list :mime (or (and check-compress :compress) :no-check-compress)))
         (filtered-files  (gather-native-files-if image-file-list :stream stream))
         (scanner1        (cl-ppcre:create-scanner "; "))
         (scanner2        (cl-ppcre:create-scanner "/|=")))
    ;; :NOTE When *IS-BUILDAPP-P* we will have already have exited.
    (unless filtered-files 
      (return-from filter-files-for-image-mime-type nil))
    (labels ((split-colon-delim (outer-split)
               ;; (split-colon-delim "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary")))
               (cl-ppcre:split scanner1 outer-split))
             ;;
             (split-inner-delim (inner-split)
               ;; (mapcar #'split-inner-delim '("image/png" "charset=binary"))
               (cl-ppcre:split scanner2 inner-split))
             ;;
             (map-splits (mapping-splits)
               ;; (map-splits "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary")
               (map 'list #'split-inner-delim (split-colon-delim mapping-splits)))
             ;;
             (image-check (split-pairs)
               ;; (image-check (map-splits "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary"))           
               (let* ((is-image      (assoc "image" split-pairs :test #'string=))
                      (is-image-type (and is-image (stringp (cadr is-image)) (cadr is-image))))
                 (and is-image (member is-image-type valid-mime-types :test #'string-equal) t)))
             ;;
             ;; :NOTE We do an rassoc here because compressed images can return this stuff:
             ;; "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary"
             ;; (binary-check (map-splits "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary"))
             (binary-check (split-pairs)
               (let* ((is-binary  (rassoc "binary" split-pairs :test #'string= :key #'car))
                      (is-charset (and is-binary (string= (car is-binary) "charset")))) ;; paranoia
                 (and is-charset t)))
             ;;
             (image-and-binary-p (split-string)
               ;; (image-and-binary-p "image/png; charset=binary compressed-encoding=application/x-gzip; charset=binary")
               (let ((splits (map-splits split-string)))
                 (and splits
                      (image-check  splits)
                      (binary-check splits)
                      t))))
      ;;
      ;; filter files in open-magic-list for files with mime-type matching:
      ;;  "image/<SOME-IMAGE-TYPE>; charset=binary"
      ;;
      (magicffi:with-open-magic (chk-mgc open-magic-list)
        (magicffi:magic-load chk-mgc)
        (loop 
           for file-magic in filtered-files
           for get-magic      = (magicffi:magic-file chk-mgc file-magic)
           for is-valid-magic = (image-and-binary-p get-magic)
           if is-valid-magic 
            do (setf (gethash file-magic file-hashtable) t)
           else
            do (setf (gethash file-magic file-hashtable) get-magic)
           finally (return (report-if-invalid-mime file-hashtable :stream stream))))))
  (when (and substitution-fun (functionp substitution-fun))
    (map-hashtable-base-remote-paths file-hashtable substitution-fun)))
    
(defun report-if-invalid-mime (hashtable &key (stream *standard-output*))
  (declare (hash-table hashtable))
  (let ((gthr-invalid '()))
    (flet ((chk-invalid (key val)
             (unless (eq val t)
               (push (list key val) gthr-invalid)
               (remhash key hashtable))))
      (maphash #'chk-invalid hashtable))
    (when gthr-invalid
      (format stream "~%Filtered namestrings with non-valid image mime-types:~%")
      (format stream "~{~{~2T:FILE ~A~%~2T:MIME ~A~%~}~^~%~}" gthr-invalid))
    hashtable))

(defun map-hashtable-base-remote-paths (hashtable subfun)
  (declare (hash-table hashtable))
  (flet ((do-sub (key val)
           (when (eq val t)
             (multiple-value-bind (rep-if if-match) (funcall subfun key)
               (if if-match
                   (setf (gethash key hashtable) rep-if)
                   ;; :TODO should report this
                   (remhash key hashtable))))))
    (maphash #'do-sub hashtable)
    hashtable))


;; :NOTE What about `cl:enough-namestring'?
(defun substitute-local-remote-base-paths  (base-path-regexp target-namestring base-remote-namestring)
  ;; *LOCAL-DIRECTORY-BASE-REGEXP* *REMOTE-DIRECTORY-BASE-NAMESTRING*
  (cl-ppcre:regex-replace base-path-regexp target-namestring base-remote-namestring))

;; :TODO in an ideal world this would also check for `cl:wild-pathname-p'.
(defun pathnames-normalize-native (file-list &key (stream *standard-output*))
  (flet ((bail-on-empty (chk-file-list) 
           (if chk-file-list
               chk-file-list
               (progn 
                 (fresh-line)
                 (write-string  "When attempting to normalize pathnames of FILE-LIST got empty list" stream)
                 (or (failed-function-report-and-bail "pathnames-normalize-native" :stream stream :exit-status 1)
                     (return-from pathnames-normalize-native nil)))))
         (string-or-pathname (s-or-p)
           (if (null s-or-p)
               nil
               (or  (stringp s-or-p)
                    (pathnamep s-or-p)))))
    (let* ((filter-non-paths (remove-if-not #'string-or-pathname file-list))
           (nrmlz-if (and (bail-on-empty filter-non-paths)
                          ;; osicat-sys:native-namestring
                          (map 'list #'sb-ext:native-namestring filter-non-paths))))
      (bail-on-empty nrmlz-if))))


;;; ==============================
;;; :NOTE Following are likely deprecated
;;;  Using osicat:with-directory-iterator/osicat:mapdir instead
;;; ==============================
;;
(defun initial-partition-dirs-from-files (file-list)
  (let ((nativize (pathnames-normalize-native file-list))
        (gthr-dir-kind     '())
        (gthr-file-kind    '())
        (gthr-ignored-kind '()))
    (unless nativize
      (return-from initial-partition-dirs-from-files))
    (dolist (ntv nativize
             (progn
               (push :directory gthr-dir-kind)
               (push :file gthr-file-kind)
               (push :ignored gthr-ignored-kind)
               (list gthr-dir-kind gthr-file-kind gthr-ignored-kind)))
      ;; (case (osicat:file-kind ntv :follow-symlinks ???)
      (case (sb-impl::native-file-kind ntv)                    
        (:file      (push ntv gthr-file-kind))
        (:directory (push (namestring (osicat:pathname-as-directory ntv)) gthr-dir-kind))
        ;;
        ;; ((:symlink :symbolic-link)  (push (list ntv :symlink)        gthr-ignored-kind))
        (:symlink   (push (list ntv :symlink)        gthr-ignored-kind))
        ;;
        ;; (:special  :socket :block-device :character-device)  (push (list ntv :special)        gthr-ignored-kind))
        (:special   (push (list ntv :special)        gthr-ignored-kind))
        (t          (push (list ntv :nonfile)        gthr-ignored-kind))))))
;;
(defun gather-native-files-if (file-list &key (stream *standard-output*))
  (let ((gather-results '()))
    (labels ((is-regular-file-p (putative-file)
               ;; osicat-sys:native-namestring
               (let ((gone-native (sb-ext:native-namestring putative-file)))
                 (and gone-native
                      ;; (osicat:file-kind gone-native :follow-symlinks)
                      (eq (sb-impl::native-file-kind gone-native) :file)
                      gone-native))))
      (loop 
         for maybe-file in file-list
         for is-regular = (is-regular-file-p maybe-file)
         if is-regular collect is-regular into ok-file
          else
         collect maybe-file into not-file
         finally (return (setf gather-results (list ok-file not-file)))))
    (when (cadr gather-results)
      (format stream "Filtered non-regular files for pathnames/namestrings:~%")
      (format stream "~{~2T~A~^~%~}" (cadr gather-results)))
    (if (car gather-results)
        (car gather-results)
        (progn
          (format stream "~%No valid pathnames found~%")
          ;; :NOTE Set *IS-BUILDAPP-P* t to enable this for buildapp. 
          (failed-function-report-and-bail "gather-native-files-if"
                                           :stream stream 
                                           :exit-status 0)))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
