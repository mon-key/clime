;;; :FILE-CREATED <Timestamp: #{2011-07-29T11:31:26-04:00Z}#{11305} - by MON>
;;; :FILE clime/clime-command-line-arguments-test.lisp
;; ==============================

;;; ==============================
;;; :NOTE Following fashioned after the tests from:
;;; :FILE command-line-arguments-20101006-git/test.lisp
;;; ==============================


(defpackage #:clime-command-line-arguments-test 
  (:use #:common-lisp)
  (:nicknames #:clime-cli-test))

(in-package #:clime-command-line-arguments-test)
;; *package*
;; (package-nicknames (find-package "CLIME-COMMAND-LINE-ARGUMENTS-TEST"))


(defun tt--args (args &key local-mount remote-mount local-sub remote-sub 
                           valid-mime arg-file help)
  (list :local-mount  local-mount
        :remote-mount remote-mount
        :local-sub    local-sub
        :remote-sub   remote-sub
        :valid-mime   valid-mime
        :arg-file     arg-file
        :help         help
        `(:unprocessed-args ,@args)))

(defun tt--process-args-and-show (&key specification
                                  cli-args-list
                                  proc-fun
                                  (stream *standard-output*)
                                  (pretty t))
  (declare ((and (not null) list)
            specification
            cli-args-list)
           ;; (function proc-fun))
           (boolean pretty))
  (multiple-value-bind (options arguments)
      (command-line-arguments:process-command-line-options 
       ;; <SPECIFICATION>  <COMMAND-LINE>
       specification       cli-args-list) 
    (format stream "~&~%;; CLI-ARGUMENTS~&~%")
    (write cli-args-list :stream stream :pretty pretty) 
    (format stream "~&~%;; PROCESSED~&~%")
    (write (apply proc-fun arguments options) 
           :stream stream
           :pretty pretty)
    (terpri stream)))

;; 
(tt--process-args-and-show :specification  clime:*CLI-SPECIFICATION*
                           :cli-args-list  (list 
                                            "--local-mount" "/mnt/LCL-MNT-POINT/" "--local-sub" "some/local-sub/dir"
                                            "--remote-mount" "/mnt/NOT-A-RMT-MNT-POINT/" "--remote-sub" "some/remote-sub"
                                            "--valid-mime" "tiff,tif,bmp"
                                            "--arg-file"  "/mnt/LCL-MNT-POINT/some-arg-file"
                                            "--help"
                                            "not-an-arg" "--"
                                            )
                           :proc-fun 'tt--args)

(command-line-arguments:show-option-help clime:*CLI-SPECIFICATION* :stream *standard-output*)



;;; ==============================
;;  :NOTE Following are the original forms from:
;;  :FILE command-line-arguments-20101006-git/test.lisp
;; 
;; (defparameter *tt--orig-opt-spec*
;;  '((("all" #\a) :type boolean :documentation "do it all")
;;    ("blah" :type string :initial-value "blob" 
;;     :documentation (concatenate 'string "This is a very long multi line documentation."
;;                                         "The function SHOW-OPTION-HELP should display this properly indented," 
;;                                         "that is all lines should start at the same column."))
;;    (("verbose" #\v) :type boolean :documentation "include debugging output")
;;    (("file" #\f) :type string :documentation "read from file instead of standard input")
;;    (("xml-port" #\x) :type integer :optional t :documentation "specify port for an XML listener")
;;    (("http-port" #\h) :type integer :initial-value 80 :documentation "specify port for an HTTP listener")
;;    ("enable-cache" :type boolean :documentation "enable cache for queries")
;;    ("path" :type string :list t :optional t :documentation "add given directory to the path")
;;    ("port" :type integer :list (:initial-contents (1 2)) :optional t :documentation "add a normal listen on given port")))
;;
;; (defun tt--orig-foo (args &key all verbose file xml-port enable-cache port path http-port blah)
;;   (list args :all all :verbose verbose :file file :xml-port xml-port :http-port http-port :blah blah
;;         :enable-cache enable-cache :port port :path path))
;;
;; (multiple-value-bind (options arguments)
;;     (command-line-arguments:process-command-line-options
;;      *tt--orig-opt-spec*
;;      '("--all" "--no-verbose" "--file" "foo" "-f" "-v" "-v"
;;        "-x" "--disable-cache" "-h" "8080"
;;        "--no-port" "--port" "3" "--port=4"
;;        "--path" "/foo" "--path" "/bar"
;;        ;; :NOTE this fails in ways i'm not comforatble with:
;;        ;; "--path" "/foo" "--path" 
;;        "--" "--foo" "bar" "baz"))
;;   (write arguments :pretty nil) (terpri)
;;   (write options :pretty nil) (terpri)
;;   (write (apply 'tt--orig-foo arguments options) :pretty nil)
;;   (terpri))
;;
;; (command-line-arguments:show-option-help *tt--opt-spec*)

;;; ==============================
;;; EOF
