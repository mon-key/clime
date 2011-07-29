;;; :FILE-CREATED <Timestamp: #{2011-07-23T14:42:34-04:00Z}#{11296} - by MON>
;;; :FILE clime/clime-specials.lisp
;;; ==============================


;;; ==============================
;;; :CLIME-SPECIALS
;;; ==============================


(in-package #:clime)
;; *package

;; Setting this T will cause application to exit if a constraint fails at any time during initialization.
;; This what we want when using the system in buildapp'd configuration.
(defvar *IS-BUILDAPP-P* nil)

(defparameter *FILE-MIME-TABLE* (make-hash-table :test 'equal))

;; (command-line-arguments:show-option-help *CLI-SPECIFICATION*)
(defparameter *CLI-SPECIFICATION*
  ;; (setq *CLI-SPECIFICATION*
  `(("local-mount"
     :type string 
     :documentation #.(format nil "Namestring identifying local base directory which is a mountpoint. ~
                                   The files contained of this mountpoint should reside on the local host. ~
                                   Argument has the form: \"/mnt/local-mount-dir/\"~%"))
    ("remote-mount"
     :type string 
     :documentation #.(format nil "Namestring identifying remote base directory which is a mountpoint. ~
                                   The files contained of this mountpoint should reside on the remote host. ~
                                   Argument has the form: \"/mnt/remote-mount-dir/\"~%"))
    ("local-sub"
     :type string
     :documentation #.(format nil "String identifying pathname-directory components ~
                                   on local host which are located beneath the base local mountpoint.~
                                   String is split at each occurenc of a #\\ character. ~
                                   Leading and trealing occurences are elided, such that a string ~
                                   with the basic form: \"some/local/sub/dir\" will be transformed ~
                                   to the list \(\"some\" \"local\" \"sub\" \"dir\"\).~%"))
     ("remote-sub"
      :type string
      :documentation #.(format nil "String identifying pathname-directory components ~
                                    on local host which are located beneath the remote mountpoint. ~
                                    String is split at each occurenc of a #\\ character. ~
                                    Leading and trealing occurences are elided, ~
                                    such that a string with the basic form: \"some/remote/sub-dir\" ~
                                    will be transformed to the list \(\"some\" \"remote\" \"sub-dir\"\).~%"))
    
    ("arg-file"
     :type string
     :documentation #.(format nil "<<<<CURRENTLY-NOT-IMPLEMENTED>>>>~%
                                   Namestring identifying a pathname which contains file and/or directory names ~
                                   located on the local-host which should be transferred to the remote host.~%"))
    ("help"
     ;; :type nil
     :documentation 
     #.(format nil "Return this help list~%"))))
  

(defvar *CLI-TO-VARIABLE-SPEC*
  ;; <KEYWORD> <REQUIRED> <ACTION> <SPECIAL-VAR>
  ;; <REQUIRED> is a boolean when t if keyword does not find a value signal an error.
  ;; (setq *CLI-TO-VARIABLE-SPEC*
  '((:LOCAL-MOUNT  t set-base-mount-parameter-namestring             *LOCAL-MOUNT-NAMESTRING*)
    (:REMOTE-MOUNT t set-base-mount-parameter-namestring             *REMOTE-MOUNT-NAMESTRING*)
    (:LOCAL-SUB  nil set-base-mount-parameter-pathname-sub-component *LOCAL-DIRECTORY-SUB-COMPONENTS*)  
    (:REMOTE-SUB nil set-base-mount-parameter-pathname-sub-component *REMOTE-DIRECTORY-SUB-COMPONENTS*)
    ;; (:HELP       nil show-clime-help)
    ;; currently unimplemented
    ;; (:ARG-FILE nil set-arg-file-parameter                         *ARG-FILE-ARGUMENTS*)
    ))

;;; ==============================
;; Elements of list are strings, either CL:PATHNAME-TYPES for a valid mime-type
;; or a mime-type as per or *nix `file` command or `libmagic`.
(defparameter *FILE-VALID-IMAGE-MIME-TYPES* 
  (list "tiff" "tif" 
        "jpeg" "jpg" ;; "pjpeg"
        "png" 
        "svg"                               ;; svg+xml
        "bmp" "x-bmp" "x-ms-bmp" "x-MS-bmp" ;; x-win-bitmap
        "dng"                               ;; x-adobe-dng -- Adobe
        "nef" "x-nikon-nef"                 ;; x-niff -- Nikon
        ;; "psd" "x-psd"
        ;; "x-dcraw"
        ;; "crw" "cr2" ;; x-canon-cr2 x-canon-crw -- Cannon 
        ))

;;; ==============================
;; If we decide to do any additional image frobbing (e.g. using convert, exiftool, bmp2tiff etc.) 
;; Add a containter for each external program's pathname used by clime. There is
;; no reason that sb-ext:run-program should search $PATH (or equivalent) each
;; time it invokes a program.
;;
;; (defparameter *RUN-PROGRAM-PATHS* '())
;;  ("convert"    ;; figure out if we intend to use convert or Imagemagick's wand via ffi
;;   "mountpoint" "bmp2tiff" "exiftool" ... )
;;
;; (let ((get-path  (gethash (command "<COMMAND>") *RUN-PROGRAM-PATHS*))) 
;;   (sb-ext:run-program  "<COMMAND>" (list "-q" putuative-mountpoint)))
;;
;; as opposed to:
;;  (sb-ext:run-program  "mountpoint" (list "-q" putuative-mountpoint) :search t)
;; 
;;; ==============================


;;; ==============================
;;; CLIME-RUNTIME-VARIABLES
;;; Following parameters will be bound at runtime according to the value of the
;;; command-line arguments.
;;; ==============================

;; :NOTE As locally configured, When value of *REMOTE-MOUNT-NAMESTRING* is FUSE
;; mountpoint only its owner has read/write permissions. IOW, not even root can
;; easily touch its contents.
(defparameter *REMOTE-MOUNT-NAMESTRING*          '())
(defparameter *REMOTE-DIRECTORY-COMPONENT*       '())
(defparameter *REMOTE-DIRECTORY-SUB-COMPONENTS*  '())
(defparameter *REMOTE-DIRECTORY-BASE-NAMESTRING* '())
;;
(defparameter *LOCAL-MOUNT-NAMESTRING*           '())
(defparameter *LOCAL-DIRECTORY-COMPONENT*        '()) 
(defparameter *LOCAL-DIRECTORY-SUB-COMPONENTS*   '())
(defparameter *LOCAL-DIRECTORY-BASE-REGEXP*      '())



;;; ==============================
;; Where are they now?
;; (loop for sym in (list '*LOCAL-MOUNT-NAMESTRING*
;;                        '*LOCAL-DIRECTORY-COMPONENT*
;;                        '*LOCAL-DIRECTORY-SUB-COMPONENTS*
;;                        '*LOCAL-DIRECTORY-BASE-REGEXP*
;;                        ;;
;;                        '*REMOTE-MOUNT-NAMESTRING*
;;                        '*REMOTE-DIRECTORY-COMPONENT*
;;                        '*REMOTE-DIRECTORY-SUB-COMPONENTS*
;;                        '*REMOTE-DIRECTORY-BASE-NAMESTRING*)
;;    for str = (string sym)
;;    for val = (symbol-value sym)
;;    collect (cons str val))
;;; ==============================

;;; ==============================
;; Forget about em:
;;
;; (dolist (i (list '*LOCAL-MOUNT-NAMESTRING*
;;                  '*LOCAL-DIRECTORY-COMPONENT*
;;                  '*LOCAL-DIRECTORY-SUB-COMPONENTS*
;;                  '*LOCAL-DIRECTORY-BASE-REGEXP*
;;                  ;;
;;                  '*REMOTE-MOUNT-NAMESTRING*
;;                  '*REMOTE-DIRECTORY-COMPONENT*
;;                  '*REMOTE-DIRECTORY-SUB-COMPONENTS*
;;                  '*REMOTE-DIRECTORY-BASE-NAMESTRING*))
;;   (set i nil))
;;; ==============================


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
