;;; :FILE-CREATED <Timestamp: #{2011-07-25T17:08:53-04:00Z}#{11301} - by MON>
;;; :FILE clime/package.lisp
;;; ==============================

;; (in-package #:clime) ;; for Slime
;; *package*


(defpackage #:clime (:use #:common-lisp)
            (:export 
             ;;
           ;; clime-specials.lisp
             #:*IS-BUILDAPP-P*
             ;;
             #:*CLI-SPECIFICATION*
             #:*CLI-TO-VARIABLE-SPEC*
             ;;
             #:*FILE-MIME-TABLE*
             #:*FILE-VALID-IMAGE-MIME-TYPES* 
             ;;
             #:*PSD-SCANNER* 
             #:*JPG-GZ-SCANNER* 
             #:*JPG-SCANNER* 
             #:*BMP-SCANNER* 
             #:*BMP-GZ-SCANNER* 
             #:*NEF-SCANNER* 
             #:*TIFF-SCANNER*
             ;;
             #:*BMP-HASH*  
             #:*BMP-GZ-HASH*
             #:*NEF-HASH*  
             #:*JPG-HASH* 
             #:*JPG-GZ-SCANNER* 
             #:*TIFF-HASH*  
             #:*PSD-HASH*   
             #:*OTHER-HASH*
             ;;
             #:*LOCAL-MOUNT-NAMESTRING*
             #:*LOCAL-DIRECTORY-COMPONENT*
             #:*LOCAL-DIRECTORY-SUB-COMPONENTS*
             #:*LOCAL-DIRECTORY-BASE-REGEXP* 
             ;;
             #:*REMOTE-MOUNT-NAMESTRING*
             #:*REMOTE-DIRECTORY-COMPONENT*
             #:*REMOTE-DIRECTORY-SUB-COMPONENTS*
             #:*REMOTE-DIRECTORY-BASE-NAMESTRING*
             ;;
           ;; clime-bail.lisp
             ;;
             #:failed-function-report-and-bail               ;; sb-ext:quit
             ;;
             ;; clime-posix.lisp
             ;;
             #:syslog-action                                 ;; sb-posix:openlog sb-posix:log-pid sb-posix:log-user sb-posix:syslog sb-posix:closelog
             #:clime-getenv
             #:clime-setenv
             #:clime-getenv-path-pathnames
             #:clime-get-posix-working-directory
             #:clime-set-posix-working-directory
             ;;
           ;; clime-initialize.lisp
             ;;
             #:clime-main
             #:check-clime-help-argument
             #:show-clime-help
             #:get-command-arguments                         ;; command-line-arguments:get-command-line-arguments  command-line-arguments:process-command-line-options
             #:verify-local-mount-command-argument
             #:set-parameter-spec-with-command-arguments
             #:set-parameter-report
             #:split-subdir-paths                            ;; cl-ppcre:split
             #:filter-valid-mime-types-command-args          ;; cl-ppcre:split
             #:verify-valid-mime-types-command-args
             #:set-valid-mime-types-from-command-args
             #:set-base-mount-parameter-pathname-component
             #:verify-mount-namestrings                      ;; sb-ext:native-namestring  sb-impl::native-file-kind  osicat:pathname-as-directory
             #:set-base-mount-parameter-namestring
             #:mountpoint-p                                  ;; sb-ext:run-program  sb-ext:process-exit-code
             #:verify-local-remote-mountpoints
             #:set-base-mount-parameter-pathname-component
             #:parse-mountpoint-directory-components         ;; sb-ext:parse-native-namestring
             #:valid-pathname-token-p
             #:verify-directory-base-for-regexp              ;; sb-impl::native-file-kind
             #:set-local-directory-base-regexp               ;; cl-ppcre:create-scanner
             #:set-remote-directory-base-namestring
             #:substitute-local-remote-base-paths            ;; cl-ppcre:regex-replace
             ;;
           ;; clime-walk-file-dir.lisp
             ;;
             #:pathnames-normalize-native                    ;; sb-ext:native-namestring
             #:filter-files-for-image-mime-type              ;; cl-ppcre:create-scanner cl-ppcre:split magicffi:with-open-magic magicffi:magic-file
             #:report-if-invalid-mime
             #:map-hashtable-base-remote-paths
             #:default-substitute-local-remote-base-paths
             ;; 
             ;; Following are likely deprecated:
             ;; #:initial-partition-dirs-from-files          ;; osicat:pathname-as-directory
             ;; #:gather-native-files-if                     ;; sb-impl::native-file-kind
             ;;
           ;; clime-copy-bytes.lisp
             ;;
             #:verify-element-type-for-copy-byte
             #:copy-byte-stream
             #:copy-byte-file
             )
            )


;; (in-package #:clime)

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
