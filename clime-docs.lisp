;;; :FILE-CREATED <Timestamp: #{2011-07-26T17:03:09-04:00Z}#{11302} - by MON>
;;; :FILE clime/clime-docs.lisp
;;; ==============================

;; "You know how to lisp, don't you? You just put your lips together and... blow"
;;    -- Lauren Bacall 'To Lisp and Lisp Not'


;;; ==============================
(in-package #:clime)
;; *package*


    
#|

 BUILDAPP/CL-LAUNCH argvs

 --local-mount  <PATHNAME-OR-NAME-STRING> *LOCAL-MOUNT-NAMESTRING*/*LOCAL-DIRECTORY-COMPONENT*              ;; "/mnt/LCL-MNT-POINT/"
 --local-sub   '(<STRING> ... ) ;; *LOCAL-DIRECTORY-SUB-COMPONENTS*                                         ;; "some/local-sub/dir"

 --remote-mount <PATHNAME-OR-NAME-STRING> ;; *REMOTE-MOUNT-NAMESTRING*/*REMOTE-DIRECTORY-COMPONENT*         ;; "/mnt/RMT-MNT-POINT/"
 --remote-sub   '(<STRING> ... ) ;; *REMOTE-DIRECTORY-SUB-COMPONENTS* ;; *REMOTE-DIRECTORY-BASE-NAMESTRING* ;; "some/remote-sub"    


 *CLI-TO-VARIABLE-SPEC*
 ;; <KEYWORD> <REQUIRED> <ACTION> <SPECIAL-VAR>
 ;; <REQUIRED> is a boolean when t if keyword does not find a value signal an error.

 ((:LOCAL-MOUNT  t set-base-mount-parameter-namestring         *LOCAL-MOUNT-NAMESTRING*)
  (:REMOTE-MOUNT t set-base-mount-parameter-namestring         *REMOTE-MOUNT-NAMESTRING*)
  (:LOCAL-SUB  nil set-base-mount-parameter-pathname-component *LOCAL-DIRECTORY-SUB-COMPONENTS*)
  (:REMOTE-SUB nil set-base-mount-parameter-pathname-component *REMOTE-DIRECTORY-SUB-COMPONENTS*))


 The initial parsing and binding of command line arguments:

 Check if we found any help args in sb-ext:*posix-argv*
  If so, print the help docstring with show-clime-help and when *IS-BUILDAPP-P* exit.
 
 Else proceed w/ parsing the command line args:
 (set-parameter-spec-with-command-arguments (get-command-arguments) *CLI-TO-VARIABLE-SPEC*)

  1) for all args in (get-command-arguments)
      `-> verify-local-mount-command-argument <KEYWORD> <KEY-VAL-ARGLIST> <REQUIRED>
  2) 
     a-1) for :LOCAL-MOUNT and :REMOTE-MOUNT
        set-base-mount-parameter-namestring <SPECIAL-PARAM> <PATHNAME-OR-NAMESTRING>
         `-> verify-mount-namestrings <PATHNAME-OR-NAMESTRING>
              `-> mountpoint-p <PUTATIVE-PATHNAME>

          | (set-base-mount-parameter-namestring '*LOCAL-MOUNT-NAMESTRING*  <NAMESTRING>)
          | (set-base-mount-parameter-namestring '*REMOTE-MOUNT-NAMESTRING* <NAMESTRING>)

          verify-mount-namestrings establishes the local namestring for local
          mountpoint making sure it names an existing directory and that
          directory names an existing mountpoint satisfying mountpoint-p

     a-2) set-base-mount-parameter-pathname-component <BASE-COMPONENT-PARAM> <BASE-MOUNT-PARAM>
           `-> parse-mountpoint-directory-components <MOUNTPOINT-NAMESTRING>
         | (set-base-mount-parameter-pathname-component  '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-MOUNT-NAMESTRING*)
         | (set-base-mount-parameter-pathname-component  '*REMOTE-DIRECTORY-COMPONENT* '*LOCAL-MOUNT-NAMESTRING*)

     b) for :LOCAL-SUB and :REMOTE-SUB 
        set-base-mount-parameter-pathname-sub-component <SPECIAL-SUB-COMP-PARAM> <SUBDIR-PATH-STRING>
         `-> split-subdir-paths <SUBDIR-PATH-STRING>
        | (set-base-mount-parameter-pathname-sub-component '*LOCAL-DIRECTORY-SUB-COMPONENTS*  "some/local-sub/dir")
        | (set-base-mount-parameter-pathname-sub-component '*REMOTE-DIRECTORY-SUB-COMPONENTS* "some/remote-sub")
        
         set-base-mount-parameter-pathname-sub-component set the value of
         <SPECIAL-PARAM> to a <SUBDIR-PATH-STRING> as recieved from the argv
         splitting <SUBDIR-PATH-STRING> into individual tokens it with split-subdir-paths

     <<<<CURRENTLY-NOT-FULLY-IMPLEMENTED>>>> 
  c) If :VALID-MIME was found as a command line argument check that it splits it into a list of strings
     If so, set value of SPECIAL-PARAM to the list of strings returned after splitting VALID-MIME-STRING.
     If :VALID-MIME was not a command line argument default is *FILE-VALID-IMAGE-MIME-TYPES*.
      set-valid-mime-types-from-command-args <SPECIAL-PARAM> <VALID-MIME-STRING>
       `-> verify-valid-mime-types-command-args <IF-ARG-ENSURED>
            `-> filter-valid-mime-types-command-args <IF-ARG>
       | (verify-valid-mime-types-command-args nil)             => *FILE-VALID-IMAGE-MIME-TYPES*
       | (verify-valid-mime-types-command-args "tiff,tiff,bmp") => ("tiff" "bmp")
       | (verify-valid-mime-types-command-args "")              => ;report and bail
       | (verify-valid-mime-types-command-args ",")             => ;report and bail


 Following outlines the order functions are called when checking pathnames
 prior to binding their associated variables at runtime.

 When *IS-BUILDAPP-P* is non-nil, at any point in initialization chain, should an error
 occur the program will exit with status 1 and no changes will be made to the
 underlying filesystem:

 1) Set the value of <SPECIAL-PARAM> to a <PATHNAME-OR-NAMESTRING> as recieved
    from the argv after verifying that <PATHNAME-OR-NAMESTRING> identifies an existing
    existing directory and that that directory is an existing mountpoint:

    set-base-mount-parameter-namestring <SPECIAL-PARAM> <PATHNAME-OR-NAMESTRING>

      (verify-mount-namestrings <PATHNAME-OR-NAMESTRING>)
       `-> mountpoint-p <PUTATIVE-PATHNAME>
          `-> (sb-ext:run-program  "mountpoint" (list "-q" <PUTUATIVE-MOUNTPOINT>) :search t)

          | (mountpoint-p "/mnt/LCL-MNT-POINT/")
          | (mountpoint-p "/mnt/RMT-MNT-POINT/")

      | (verify-mount-namestrings "/mnt/LCL-MNT-POINT/")
      | (verify-mount-namestrings "/mnt/RMT-MNT-POINT/")

    | (set-base-mount-parameter-namestring  '*LOCAL-MOUNT-NAMESTRING*  "/mnt/LCL-MNT-POINT/")
    | (set-base-mount-parameter-namestring  '*REMOTE-MOUNT-NAMESTRING* "/mnt/RMT-MNT-POINT/")


 2) If *LOCAL-MOUNT-NAMESTRING* and *REMOTE-MOUNT-NAMESTRING* were successfuly bound. 
    Bind their pathnames-directory components with set-base-mount-parameter-pathname-component

    set-base-mount-parameter-pathname-component <BASE-COMPONENT-PARAM> <BASE-MOUNT-PARAM>

    | (set-base-mount-parameter-pathname-component '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-MOUNT-NAMESTRING*)
    | (set-base-mount-parameter-pathname-component '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-MOUNT-NAMESTRING*)
 
 
 3) If argv contained values for the flags --remote-mount --remote-sub we split
    them into a list of strings tokens with split-subdir-paths. The reason for
    this amount of indirection (as opposed to simply using enough-namestring) is
    to ensure we find _reasoanble_ relative pathnames most of what we do with
    these values will get passed along to a multiple *NIX environments
    potentially on two separates hosts and we need to make sure it happens
    sanely.

    set-base-mount-parameter-pathname-sub-component <SPECIAL-SUB-COMP-PARAM> <SUBDIR-PATH-STRING>)
     `-> split-subdir-paths <SUBDIR-PATH-STRING>
         `-> (cl-ppcre:split "/" <SUBDIR-PATH-STRING>)

         | (split-subdir-paths "some/local-sub/dir") => ("some" "local-sub" "dir")
         | (split-subdir-paths "some/remote-sub")    => ("some" "remote-sub")

       | (set-base-mount-parameter-pathname-sub-component '*LOCAL-DIRECTORY-SUB-COMPONENTS*  "some/local-sub/dir")
       | (set-base-mount-parameter-pathname-component     '*REMOTE-DIRECTORY-SUB-COMPONENTS* "some/remote-sub")


4) Bind the local-regexp and remote-namestring variables after ensuring they exist:

    set-local-directory-base-regexp-and-remote-namestring
    <LOCAL-DIRECTORY-BASE-REGEXP-PARAM>      <LOCAL-DIRECTORY-COMPONENT-PARAM-FOR-REGEXP> <LOCAL-DIRECTORY-SUB-COMPONENT-PARAM-FOR-REGEXP>
    <REMOTE-DIRECTORY-BASE-NAMESTRING-PARAM> <REMOTE-DIRECTORY-COMPONENT-PARAM>           <REMOTE-DIRECTORY-SUB-COMPONENT-PARAM>

   (set-local-directory-base-regexp-and-remote-namestring
    '*LOCAL-DIRECTORY-BASE-REGEXP*      '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*
    '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)

    a) (set-local-directory-base-regexp       '*LOCAL-DIRECTORY-BASE-REGEXP* '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
        `-> (verify-directory-base-for-regexp '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*)

    b) (set-remote-directory-base-namestring  '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)
        `-> (verify-directory-base-for-regexp '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)

  
  (set-remote-directory-base-namestring  '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)
   => "/mnt/RMT-MNT-POINT/"

  (set-local-directory-base-regexp '*LOCAL-DIRECTORY-BASE-REGEXP* '*LOCAL-DIRECTORY-COMPONENT* '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
   => #<CLOSURE (LAMBDA (STRING CL-PPCRE::START CL-PPCRE::END)) {EDEDAC5}>

  set-base-mount-parameter-pathname-sub-component after splitting <SUBDIR-PATH-STRING> with split-subdir-paths:
     (set-base-mount-parameter-pathname-sub-component '<SPECIAL-PARAM>  <SUBDIR-PATH-STRING>)
      ->  split-subdir-paths <SUBDIR-PATH-STRING>

|#


#|
  
 failed-function-report-and-bail
  - sb-ext:quit
 
 syslog-action
  - sb-posix:openlog 
  - sb-posix:log-pid 
  - sb-posix:log-user 
  - sb-posix:syslog 
  - sb-posix:closelog
 
 clime-main
  - set-parameter-spec-with-command-arguments
  - get-command-arguments

 get-command-arguments
  - check-clime-help-argument
  - command-line-arguments:get-command-line-arguments 
  - command-line-arguments:process-command-line-options

 check-clime-help-argument
  - sb-ext:*posix-argv*
  - show-clime-help
  - sb-ext:quit

 show-clime-help
  - command-line-arguments:show-option-help
 
 verify-local-mount-command-argument
  - failed-function-report-and-bail

 filter-valid-mime-types-command-args
  - cl-ppcre:split

 verify-valid-mime-types-command-args
  - filter-valid-mime-types-command-args
  - failed-function-report-and-bail

 set-valid-mime-types-from-command-args
  - verify-valid-mime-types-command-args

 split-subdir-paths
  - cl-ppcre:split
 
 set-parameter-spec-with-command-arguments
  - get-command-arguments
  - *CLI-TO-VARIABLE-SPEC*
  - verify-local-mount-command-argument
  - set-parameter-report
  - set-local-directory-base-regexp-and-remote-namestring

 set-base-mount-parameter-pathname-component
  - split-subdir-paths
 
 verify-mount-namestrings
  - failed-function-report-and-bail
  - sb-ext:native-namestring
  - sb-impl::native-file-kind
  - osicat:pathname-as-directory
 
 set-base-mount-parameter-namestring
  - failed-function-report-and-bail
  - verify-mount-namestrings
 
 mountpoint-p
  - sb-ext:run-program
  - sb-ext:process-exit-code
  - failed-function-report-and-bail
 
 verify-local-remote-mountpoints
  - mountpoint-p
 
 set-base-mount-parameter-pathname-component
  - failed-function-report-and-bail
  - parse-mountpoint-directory-components
 
 parse-mountpoint-directory-components
  - sb-ext:parse-native-namestring 
 
 verify-directory-base-for-regexp
   - valid-pathname-token-p
   - sb-impl::native-file-kind
   - failed-function-report-and-bail
 
 set-local-directory-base-regexp
   - verify-directory-base-for-regexp
   - failed-function-report-and-bail
 
 set-remote-directory-base-namestring
   - verify-directory-base-for-regexp
   - failed-function-report-and-bail
 
 pathnames-normalize-native
   - failed-function-report-and-bail
   - sb-ext:native-namestring
 
 filter-files-for-image-mime-type
   - failed-function-report-and-bail
   - report-if-invalid-mime
   - map-hashtable-base-remote-paths
 
 report-if-invalid-mime
   - 
 
 verify-element-type-for-copy-byte
  - failed-function-report-and-bail
 
 copy-byte-stream
  - verify-element-type-for-copy-byte
 
 copy-byte-file
  - verify-element-type-for-copy-byte

|#




;;; ==============================
;;; :CLIME-DOCUMENTATION
;;; ==============================

;;; ==============================
;; verify-element-type-for-copy-byte
;; Verify that ELEMENT-TYPE is a subtype of type cl:unsigned-byte.
;; When *IS-BUILDAPP-P* is non-nil exit immediately, else signal an error. 
;; Keyword STREAM is a stream to report error on.  :NOTE libmagic doesn't tell
;; us much other than the encoding is binary... this check exists to ensure we
;; are conservative with our handling of byte-streams. Likely we could declare
;; all copy-byte operations to be acting on stream elements of
;; type'(unsigned-byte 8) but maybe the file has some other wacky internal byte
;; fields.


;;; ==============================
;; :NOTE `copy-byte-file' and `copy-byte-stream' fashioned after the functions `copy-file'/`copy-stream'
;; :SOURCE com.informatigo-20110522-git/common-lisp/cesarum/stream.lisp
;; :SOURCE com.informatigo-20110522-git/common-lisp/cesarum/file.lisp
;;
;;; ==============================
;; copy-byte-stream
;; Keyword ELEMENT-TYPE is a type specifier for the temporary array
;; byte-stream-bfr. Default is cl:unsigned-byte. When some other type is
;; provided it must be a subtype of cl:unsigned-byte, and error is signalled if not.
;; Keyword VERIFY-ELEMENT-TYPE when NIL indicates that type specified by
;; ELEMENT-TYPE does not need to be checked with `verify-element-type-for-copy-byte'.
;; Keyword STREAM is a stream to report errors to. Default is *standard-output*.
;;
;;; ==============================
;; copy-byte-file
;; "Copy the contents of file SOURCE-BYTE-FILE to the file at path DEST-BYTE-FILE.
;; SOURCE-BYTE-FILE and DEST-BYTE-FILE are both files contained of "binary data"
;; of type '(unsigned-byte 8).
;; Keyword IF-EXISTS pertains to DEST-BYTE-FILE it is as per CL:WITH-OPEN-FILE. 
;; Default is :supersede (existent destination files are overwritten).
;; Keyword ELEMENT-TYPE pertains to both SOURCE-BYTE-FILE and DEST-BYTE-FILE it is as per CL:WITH-OPEN-FILE. 
;; Default is '(unsigned-byte 8). There should be no reason to change this. If
;; an alternative value is provided it should represent a byte spec,
;; e.g. specifying 'character is likely to leave you with a corrupted
;; DEST-BYTE-FILE.
;; Keyword EXTERNAL-FORMAT is as per WITH-OPEN-FILE. 
;; It defaults to :DEFAULT. On *nix-like systems :DEFAULT should reflect the
;; current locale unless SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* has been manually
;; frobbed.


;;; ==============================
;; failed-function-report-and-bail
;; FUNCTION-NAME is a string or symbol naming a function which failed to satisfy a constraint.
;; Keyword STREAM is a stream to write report to. Default is CL:*STANDARD-OUTPUT*
;; Keyword EXIT-STATUS is an integer value as per the UNIX-STATUS keyword fo SB-EXT:QUIT. Default is 0 (indicating success).
;; :NOTE This function is a noop unless *IS-BUILDAPP-P* is non-nil.
;; :EXAMPLE
;; (failed-function-report-and-bail 'failed-function-report-and-bail :stream nil)
;; (failed-function-report-and-bail "failed-function-report-and-bail" :stream nil)

;;; ==============================
;; check-clime-help-argument
;; If we find something in value of sb-ext:*posix-argv* that looks like a
;; request for help show help and exit when *IS-BUILDAPP-P*.
;;
;; Things that look like a request for help include:
;;  "-h"  "help" "-help" "--help" "usage" "-usage" "--usage"

;;; ==============================
;; show-clime-help
;; Return a description of valid Clime arguments.
;;

;;; ==============================
;; mountpoint-p
;; if PUTUATIVE-MOUNTPOINT is an existing mountpoint return PUTUATIVE-MOUNTPOINT.
;; :EXAMPLE
;;  (mountpoint-p "/")
;;  (mountpoint-p *LOCAL-MOUNT-NAMESTRING*)
;;  (mountpoint-p *REMOTE-MOUNT-NAMESTRING*)
;;  (mountpoint-p "not-a-mount-point")

;;; ==============================
;; set-base-mount-parameter-pathname-component  
;; Set value of special-variable BASE-COMPONENT-PARAM to the cl:pathname-directory
;; component of special-variable BASE-MOUNT-PARAM
;; Both BASE-COMPONENT-PARAM and BASE-MOUNT-PARAM should be passed as quoted symbols.
;; It is assumed that BASE-MOUNT-PARAM has already been processed with
;; `set-base-mount-parameter-namestring' and its value is a cl:namestring.
;; Keyword STREAM is a stream to report output to in the event of an error. Default is *standard-output*.
;; :EXAMPLE
;; (set-base-mount-parameter-pathname-component '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-MOUNT-NAMESTRING*)
;; (set-base-mount-parameter-pathname-component '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-MOUNT-NAMESTRING*)

;;; ==============================
;; set-base-mount-parameter-pathname-sub-component
;; Set value of SPECIAL-PARAM to return value of `split-subdir-paths'.

;;; ==============================
;; set-parameter-spec-with-command-arguments
;; Set values of PARAMTER-SPEC according to key/value pairs of COMMAND-ARGUMENTS
;; Keyword COMMAND-ARGUMENTS is as per return value of `clime:get-command-arguments'
;; Keyword PARAMETER-SPEC is as per `clime:*CLI-TO-VARIABLE-SPEC*', the default when ommitted.
;; PARAMETER-SPEC is a list of lists each element of the form:
;;  <KEYWORD> <REQUIRED> <ACTION> <SPECIAL-VAR>
;; Following two forms are functionaly equivalent:
;; (set-parameter-spec-with-command-arguments)
;; (set-parameter-spec-with-command-arguments (get-command-arguments :stream *standard-output*) 
;;                                            *CLI-TO-VARIABLE-SPEC* 
;;                                            :stream *standard-output*)

;;; ==============================
;; get-command-arguments
;; => (:LOCAL-MOUNT  <NAMESTRING>
;;     :REMOTE-MOUNT <NAMESTRING>
;;     :LOCAL-SUB    [ <SUBDIR-PATH-STRING> | nil ]
;;     :REMOTE-SUB   [ <SUBDIR-PATH-STRING> | nil ]
;;     :ARG-FILE     [ <NAMESTRING> | nil ] )

;;; ==============================
;; *CLI-TO-VARIABLE-SPEC*
;; A list of lists each element having the form:
;; <KEYWORD> <REQUIRED> <ACTION> <SPECIAL-VAR>
;; Each element of this list is processed by `clime:verify-local-mount-command-argument' in
;; `clime:set-parameter-spec-with-command-arguments'.
;; <KEYWORD> is key into the reeturn value of `clime:get-command-arguments'.
;; <REQUIRED> is a boolean. 
;; When t if <KEYWORD> does not find value at runtime signal an error.
;;
;; <ACTION> is symbol identifying a function. 
;; It is invoke with <SPECIAL-VAR> as its first arg and the value of the one
;; associated <KEYWORD> in return value of clime:get-command-arguments as its
;; second argument, e.g.:
;;  (set-base-mount-parameter-namestring '*LOCAL-MOUNT-NAMESTRING* (assoc :LOCAL-MOUNT (get-command-arguments)))


;;; ==============================
;; verify-local-remote-mountpoints
;; check that LOCAL-MOUNT and REMOTE-MOUNT are both mountpoint-p

;;; ==============================
;; split-subdir-paths
;; Used to extract pathaname directory components from command-line-arguments: 
;;  --local-sub --remote-sub
;; 
;; (split-subdir-paths "/" nil)
;; (split-subdir-paths "/" "")
;; (split-subdir-paths "/" "/")
;; (split-subdir-paths "/" "some-path")
;; (split-subdir-paths "/" "some/path/tosub")
;; (split-subdir-paths "/" "//some-path//" )

;;; ==============================
;; filter-valid-mime-types-command-args
;; If IF-ARG is `cl:stringp' destructively modify its value by splitting on
;; #\\, and whitespace boundaries as if by cl-ppcre:split with the :sharedp keyword T.
;; Returned value is a either null or a list of strings filtered for empty
;; strings and duplicates as if by `cl:delete-duplicates' and `cl:delete-if'.
;; :EXAMPLE
;;  (filter-valid-mime-types-command-args "tiff")
;;  (filter-valid-mime-types-command-args "tiff,tif,bmp")
;;  (filter-valid-mime-types-command-args "tiff, tif tiff  bmp  , bmp ")
;;  (filter-valid-mime-types-command-args "")
;;  (filter-valid-mime-types-command-args " ")
;;  (filter-valid-mime-types-command-args ",")
;;  (filter-valid-mime-types-command-args " , ")
;;  (filter-valid-mime-types-command-args nil)
;;  (filter-valid-mime-types-command-args '())


;; set-valid-mime-types-from-command-args
;; Set value of SPECIAL-PARAM to the list of strings returned after splitting VALID-MIME-STRING.
;; :EXAMPLE
;; (set-valid-mime-types-from-command-args '*FILE-VALID-IMAGE-MIME-TYPES* "tiff,tif,bmp")

;;; ==============================
;; verify-valid-mime-types-command-args
;; If --valid-mime was passed on command line verify that IF-ARG-ENSURED splits to something
;; reasonable with for use as value of `clime:*FILE-VALID-IMAGE-MIME-TYPES*'.
;; If `clime:filter-valid-mime-types-command-args' returns a list of strings
;; with IF-ARG-ENSURED as its argument return it, else and *IS-BUILDAPP-P* is
;; non-nil signal an error and exit with status 1.
;;
;; KEYWORD stream is a stream to report to. Default is *standard-output*.
;; We do not want to bind *FILE-VALID-IMAGE-MIME-TYPES* to the empty list should
;; `clime:filter-valid-mime-types-command-args' return null
;; *FILE-VALID-IMAGE-MIME-TYPES* 

;;; ==============================
;; parse-mountpoint-directory-components
;; Return the cl:pathname-directory of MOUNTPOINT-NAMESTRING.
;; MOUNTPOINT-NAMESTRING should already have been processed by
;; `verify-mount-namestrings' and will therefor satisfy `cl:stringp'.

;;; ==============================
;; verify-directory-base-for-regexp
;; Return a namestring comprised of the merging pathname-components
;; of LOCAL-DIRECTORY-COMPONENT-PARAM with the strings of LOCAL-DIRECTORY-SUB-COMPONENT-PARAM
;; Both LOCAL-DIRECTORY-COMPONENT-PARAM and LOCAL-DIRECTORY-SUB-COMPONENT-PARAM are quoted symbols naming a special variable, e.g. 
;; *LOCAL-DIRECTORY-COMPONENT* and *LOCAL-DIRECTORY-SUB-COMPONENTS*  respectively.
;; Prior to returning the namestring generated from the combining the pathname
;; components is verified to point to an exising directory. An error is
;; signalled if not.
;; Keyword STREAM names a stream to report errors to. Default is cl:*standard-output*.
;; :EXAMPLE 
;; (setq *LOCAL-DIRECTORY-BASE-REGEXP* 
;;       (verify-directory-base-for-regexp '*LOCAL-DIRECTORY-COMPONENT* '*LOCAL-DIRECTORY-SUB-COMPONENTS*))
;; 
;; (verify-directory-base-for-regexp '*LOCAL-DIRECTORY-COMPONENT* '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
;; (verify-directory-base-for-regexp '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)

;;; ==============================
;; verify-local-mount-command-argument
;; Find KEYWORD in KEY-VAL-ARGLIST.
;; Keyword REQUIRED when non-nil indicates that KEYWORD must return a value or
;; when `*IS-BUILDAPP-P*' an error is siganaled.
;; If KEYWORD is not present when `*IS-BUILDAPP-P*' signal an error.
;; :EXAMPLE
;; (verify-local-mount-command-argument
;;  :REMOTE-SUB
;;  '(
;;    :REMOTE-MOUNT "/mnt/RMT-MNT-POINT/"
;;    :LOCAL-MOUNT  "/mnt/LCL-MNT-POINT/"
;;    :LOCAL-SUB    "some/local-sub/dir"
;;    :REMOTE-SUB   "some/remote-sub"
;;    :ARG-FILE     "/some/path/to/foo" 
;;  :required t)
;;
;; ARG-FILE names a file to read directories and pathnames from

;;; ==============================
;; set-local-directory-base-regexp
;; Cenerate a cl-ppcre scanner closure which finds a namestring genereted from
;; values of special variables indicated by arguments
;;  LOCAL-DIRECTORY-COMPONENT-PARAM-FOR-REGEXP
;;  LOCAL-DIRECTORY-SUB-COMPONENT-PARAM-FOR-REGEXP
;; and bind it to special variable indicated by LOCAL-DIRECTORY-BASE-REGEXP-PARAM.
;; Namestring is generated with `verify-directory-base-for-regexp' an error is
;; signalled if the namestring it returns does not find an existing directory.
;; Keyword STREAM is a stream to report errors to.
;; :EXAMPLE
;; (set-local-directory-base-regexp '*LOCAL-DIRECTORY-BASE-REGEXP* 
;;                                  '*LOCAL-DIRECTORY-COMPONENT* 
;;                                  '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
;; (let* ((nm (namestring (make-pathname :directory 
;;                                       (append *LOCAL-DIRECTORY-COMPONENT*
;;                                               *LOCAL-DIRECTORY-SUB-COMPONENTS*))))
;;        (nm-len  (length nm)))
;;   (values nm
;;           nm-len
;;           (cl-ppcre:all-matches 
;;            *LOCAL-DIRECTORY-BASE-REGEXP*
;;            (namestring (make-pathname :directory (append *LOCAL-DIRECTORY-COMPONENT* 
;;                                                          *LOCAL-DIRECTORY-SUB-COMPONENTS*))))))


;;; ==============================
;; set-remote-directory-base-namestring
;; Generate a pathname for use as a replacement for the cl-ppcre scanner closure
;; returned from `set-local-directory-base-regexp'.
;; :EXAMPLE
;; (set-remote-directory-base-namestring  '*REMOTE-DIRECTORY-BASE-NAMESTRING*
;;                                        '*REMOTE-DIRECTORY-COMPONENT*
;;                                        '*REMOTE-DIRECTORY-SUB-COMPONENTS*)

;;; ==============================
;; set-local-directory-base-regexp-and-remote-namestring
;; (set-local-directory-base-regexp-and-remote-namestring
;;  '*LOCAL-DIRECTORY-BASE-REGEXP*      '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*
;;  '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)
;;   
;; (set-local-directory-base-regexp       '*LOCAL-DIRECTORY-BASE-REGEXP*      '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
;;  `-> (verify-directory-base-for-regexp '*LOCAL-DIRECTORY-COMPONENT*  '*LOCAL-DIRECTORY-SUB-COMPONENTS*)
;;
;; (set-remote-directory-base-namestring  '*REMOTE-DIRECTORY-BASE-NAMESTRING* '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)
;;  `-> (verify-directory-base-for-regexp '*REMOTE-DIRECTORY-COMPONENT* '*REMOTE-DIRECTORY-SUB-COMPONENTS*)


;;; ==============================
;; valid-pathname-token-p
;; Is token a non-empty string neither begining or ending with the characters:
;;  #\\/ #\\. #\\* #\\Nul #\\Space #\\Newline #\\TAB  
;; nor contained of the interior character #\\.?
;; :EXAMPLE
;; (valid-pathname-token-p 8)
;; (valid-pathname-token-p "")
;; (valid-pathname-token-p ".")
;; (valid-pathname-token-p "*")
;; (valid-pathname-token-p "/")
;; (valid-pathname-token-p "//")
;; (valid-pathname-token-p ".//")
;; (valid-pathname-token-p "./")
;; (valid-pathname-token-p "bubba/interior")
;; (valid-pathname-token-p "bubba-trailed/")
;; (valid-pathname-token-p "/bubba-leading")
;; (valid-pathname-token-p "bubba-is-ok")

;;; ==============================
;; verify-mount-namestrings
;; Check that PATHNAME-OR-NAMESTRING names an existing directory and if so return its CL:NAMESTRING.
;; If PATHNAME-OR-NAMESTRING is neither cl:stringp nor cl:pathnamep or if it
;; does not name an existing directory, if *IS-BUILDAPP-P* bail with exit
;; status 1, else return NIL.

;;; ==============================
;; substitute-local-remote-base-paths
;; Match BASE-PATH-REGEXP in TARGET-NAMESTRING and replaced it with BASE-REMOTE-NAMESTRING.
;; Return two values as if by cl:values:
;;  nth value 0 is either <TARGET-NAMESTRING> | <TARGET-NAMESTRING-WITH-REPLACMENT>
;;  nth value 1 is either T | NIL. T if a replacement in <TARGET-NAMESTRING> occured.
;; BASE-PATH-REGEXP is a regular expression matching a base local path, where the path to be matched has the form:
;;  /base-path/with-trailing-slash/
;; BASE-PATH-REGEXP may be a namestring or closure as per return value of `cl-ppcre:create-scanner'.
;; TARGET-NAMESTRING a namestring. 
;; BASE-REMOTE-NAMESTRING a namestring for the remote path, where path has the form:
;;  /remote-path/with-trailing-slash/
;; :EXAMPLE
;; (substitute-local-remote-base-paths "/base-path/with-trailing-slash/"
;;                                     "/base-path/with-trailing-slash/followed/by/subdir/and.file"
;;                                     "/remote-path/is-longer/with-trailing-slash/")
;; (let ((base-regexp (cl-ppcre:create-scanner "/base-path/with-trailing-slash/")))
;;   (substitute-local-remote-base-paths base-regexp
;;                                       "/base-path/with-trailing-slash/followed/by/subdir/and.file"
;;                                       "/remote-path/is-longer/with-trailing-slash/"))

;;; ==============================
;; filter-files-for-image-mime-type
;; IMAGE-FILE-LIST
;; Keyword STREAM is a stream to report results to. Default is `cl:*standard-output*'.
;; Keyword VALID-MIME-TYPES a list of valid mime types. Each element of list is a string.
;; Keyword SUBSTITUTION-FUN function to rename and filter files gathered into FILE-HASHTABLE
;; Keyword FILE-HASHTABLE a hashtable to gather files into. cl:hash-table-test
;; of FILE-HASHTABLE should return 'equal or 'equalp. Default is *FILE-MIME-TABLE*.
;; Keyword CHECK-COMPRESS when non-nil indicates that when file is a compressed file an attempt
;; to get the mime type of the contained file (presumably by decompressing).
;; :NOTE When images in IMAGE-FILE-LIST list are large decompressing may pose
;; significant performance penalties.

;;; ==============================
;; report-if-invalid-mime
;; Report if any files did not contain valid magic and remove them from HASHTABLE.
;; HASHTABLE names a hash-table holding a list of files, e.g. `*FILE-MIME-TABLE*'.
;; Keyword stream is a stream to report results to. Default is *standard-output*.

;;; ==============================
;; map-hashtable-base-remote-paths
;; Map HASHTABLE with SUBFUN, return HASHTABLE.
;; For each key/value pair in hashtable if SUBFUN's nth value 1 is T replace
;; value in pair with the nth value 0 returned by SUBFUN. When nth value 0 of
;; subfun is NIL key is removed from hashtable.
;; SUBFUN is a function which returns two values. It accpets a single
;; argument, a namestring, and replaces some base portion of that namestring
;; with a remote namestring. If SUBFUN is not implemented with
;; `substitute-local-remote-base-paths' it should share the same semantics.
;; :EXAMPLE
;; (let* ((base-regexp (cl-ppcre:create-scanner "/base-path/with-trailing-slash/"))
;;        (remote-path (cl-ppcre:create-scanner "/remote-path/is-longer/with-trailing-slash/")))
;;   (flet ((rplc (target remote)
;;            (substitute-local-remote-base-paths base-regexp target remote)))
;;     (map-hashtable-base-remote-paths *FILE-MIME-TABLE* #'rplc)))

;;; ==============================
;; default-substitute-local-remote-base-paths
;; Default function for performing local->remote pathname substitutions for
;; files in in hashtable *FILE-MIME-TABLE*.
;; TARGET is a namestring.

;;; ==============================
;; gather-native-files-if
;; Return a native-namestring for each pathname in file-list that is a regular file
;; :EXAMPLE

;;; ==============================
;; syslog-action
;;  "Write a syslog message.
;; Keyword LOG-MESSAGE is a message to write. Default is \"<EMPTY-SBCL-LOG-MESSAGE>\".
;; Keyword LOG-PRIORITY is an integer in the range 0,7. Default is 6 which
;; corresponds to `sb-posix:log-info'. Other values are as follows:~%
;;   0 -- sb-posix:log-emerg      system is unusable
;;   1 -- sb-posix:log-alert      action must be taken immediately
;;   2 -- sb-posix:log-crit       critical conditions
;;   3 -- sb-posix:log-err        error conditions
;;   4 -- sb-posix:log-warning    warning conditions
;;   5 -- sb-posix:log-notice     normal, but significant, condition
;;   6 -- sb-posix:log-info       informational message
;;   7 -- sb-posix:log-debug      debug-level message~%~@
;; Keyword LOG-IDENT is a string identifying the orginator of the message. Default is \"sbcl\".
;; When LOG-IDENT is the default message is logged with the current processes pid.~%~@
;; :EXAMPLE
;;  \(syslog-action\)
;;  \(syslog-action :log-message \"A different log message\"\)
;;  \(syslog-action :log-message \"A different log message\"  :log-ident \"bubba\"\)
;; :SEE-ALSO `sb-posix:syslog', `sb-posix:openlog', `sb-posix:closelog', `sb-posix:log-user'.~%▶▶▶"

;; *FILE-VALID-IMAGE-MIME-TYPES*
;;; ==============================
;; :SEE /usr/share/mime/image
;; :SEE /usr/share/mime/image/x-nikon-nef.xml
;; ,----
;; | Raw files contain, by necessity, the information required to produce a viewable
;; | image from the camera's sensor data. The structure of raw files, including the
;; | ISO standard raw image format ISO 12234-2, TIFF/EP, often follows a common
;; | pattern
;; |   ( ... )
;; | Many raw file formats are based on the TIFF file format.[5] 
;; | These files may deviate from the TIFF standard in a number of ways, including
;; | the use of a non-standard file header, the inclusion of additional image tags
;; | and the encryption of some of the tagged data.
;; | [5] :SEE (URL `http://www.sno.phy.queensu.ca/~phil/exiftool/#supported')
;; `---- :SOURCE (URL `http://en.wikipedia.org/wiki/Raw_image_format#cite_note-4')
;;
;; :NOTE Nikon images with file extension "<FILE>.nef" are currently returning "image/tiff".
;; The first four characters of these files are (in hex):
;; x4d x4d x00 x2a 
;; MM^@*
;; which seems to cause a spurious indication of a tiff Big Endian file.
;; See mime-type definition for tiff format in :FILE /usr/share/mimelnk/magic 
;; 1      2 3          4
;; 0 string MM\x00\x2a image/tiff
;;
;; ,----
;; | NEF files are generated by Nikon cameras.
;; | 
;; | :MIME-TYPE
;; | 
;; | image/x-nikon-nef
;; | 
;; | :Organisation
;; | It is a TIFF file. close to TIFF/EP or DNG.
;; | 
;; | :THUMBNAILS
;; | NEF files have a large JPEG preview. It is either in a subIF of the main IFD
;; | (type == thumbail) or for the older, in the MakerNote IFD.
;; | 
;; | :COMPRESSION
;; | D1 files are not compressed.
;; | 
;; | D1x files are not compressed but have a weird shape: they have a pixel ratio of
;; | 0.5, ie 2 row for one.
;; | 
;; | Some files are packed (Compression = 32769). It is a standard packing with some
;; | padding: if col % 10 == 9, then an extra 8 bits must be skipped.
;; | 
;; | Compression (Compression = 34713) is a Huffman tree and a quantization
;; | table. The quantization tables are at 0x8c and 0x96 tag from the
;; | MakerNote. Strangely dcraw seems to only use the later as it override the value
;; | reading the tags in sequence. See nikon_compressed_load_raw().
;; | 
;; | following has a very insightful article about NEF decoding.
;; | (URL `http://www.majid.info/mylos/weblog/2004/05/02-1.html') 
;; | 
;; | The D100 has a bug that tag uncompressed image as compressed, but not
;; | always. See nikon_is_compressed() in dcraw.c. It appear that most D100 will just
;; | use uncompressed (packed) as the compression froze the camera for 20s
;; | 
;; `---- :SOURCE (URL `http://libopenraw.freedesktop.org/wiki/Nikon_NEF')
;; :SEE (URL `https://bugs.launchpad.net/ubuntu/+source/shared-mime-info/+bug/93026')
;;; ==============================



#|

 SBCL features:
 sb-ext:*posix-argv*
 sb-ext:quit
 sb-ext:run-program
 sb-ext:process-exit-code

|#


#|

 magicffi:+MAGIC-APPLE+
 magicffi:+MAGIC-CHECK+
 magicffi:+MAGIC-COMPRESS+
 magicffi:+MAGIC-CONTINUE+
 magicffi:+MAGIC-DEBUG+
 magicffi:+MAGIC-DEVICES+
 magicffi:+MAGIC-ERROR+
 magicffi:+MAGIC-MIME+
 magicffi:+MAGIC-MIME-ENCODING+
 magicffi:+MAGIC-MIME-TYPE+
 magicffi:+MAGIC-NO-CHECK-APPTYPE+
 magicffi:+MAGIC-NO-CHECK-ASCII+
 magicffi:+MAGIC-NO-CHECK-CDF+
 magicffi:+MAGIC-NO-CHECK-COMPRESS+
 magicffi:+MAGIC-NO-CHECK-ELF+
 magicffi:+MAGIC-NO-CHECK-ENCODING+
 magicffi:+MAGIC-NO-CHECK-FORTRAN+
 magicffi:+MAGIC-NO-CHECK-SOFT+
 magicffi:+MAGIC-NO-CHECK-TAR+
 magicffi:+MAGIC-NO-CHECK-TEXT+
 magicffi:+MAGIC-NO-CHECK-TOKENS+
 magicffi:+MAGIC-NO-CHECK-TROFF+
 magicffi:+MAGIC-NONE+
 magicffi:+MAGIC-PRESERVE-ATIME+
 magicffi:+MAGIC-RAW+
 magicffi:+MAGIC-SYMLINK+

|#



;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:



;;; ==============================
;;; EOF
