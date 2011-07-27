;;; :FILE-CREATED <Timestamp: #{2011-07-26T17:00:32-04:00Z}#{11302} - by MON>
;;; :FILE clime/copy-bytes.lisp
;;; ==============================


(in-package #:clime)
;; *package*

(defun verify-element-type-for-copy-byte (element-type &key (stream *standard-output*))
  (when (or (null element-type)
            (not (equal '(t t) (multiple-value-list (subtypep element-type 'unsigned-byte)))))
    (flet ((report-bad-elt-type (format-stream)
             (format format-stream 
                     ":FUNCTION `verify-element-type-for-copy-byte' ~
                      -- keyword ELEMENT-TYPE not a subtype of cl:unsigned-byte~%~T~
                      got type-specifier: ~S~%"
                     element-type)))
      (report-bad-elt-type stream)
      (or 
       (failed-function-report-and-bail "verify-element-type-for-copy-byte" :stream stream :exit-status 1)
       (error (report-bad-elt-type nil))))))

(defun copy-byte-stream (from-byte-stream to-byte-stream &key (element-type 'unsigned-byte) 
                         (report-stream *standard-output*)
                         (verify-element-type t))
  (declare (boolean verify-element-type))
  (when verify-element-type 
    (verify-element-type-for-copy-byte element-type :stream report-stream))
  (let ((byte-stream-bfr (make-array 4096 :element-type element-type)))
    (do ((byte-stream-pos (read-sequence byte-stream-bfr from-byte-stream) 
                          (read-sequence byte-stream-bfr from-byte-stream)))
        ((zerop byte-stream-pos) nil)
      (write-sequence byte-stream-bfr to-byte-stream :end byte-stream-pos))))

;; :NOTE Maybe add a key DEFINITE-OCTET which when non-nil defaults element-type to '(unsigned-byte 8)
;; Would require some function higher up to confirm that certain file-types are
;; known (within reason) to be contained of only (unsigned-byte 8).
(defun copy-byte-file (source-byte-file dest-byte-file &key (if-exists       :supersede) ;; :error 
                                                            (element-type    'unsigned-byte) 
                                                            (external-format :default)
                                                            (report-stream   *standard-output*))
  (verify-element-type-for-copy-byte element-type :stream report-stream)
  (with-open-file (byte-input source-byte-file
                              :direction         :input
                              :if-does-not-exist :error
                              :external-format   external-format ; Is this ever applicable?
                              :element-type      element-type)
    (with-open-file (byte-output dest-byte-file
                                 :direction         :output
                                 :if-does-not-exist :create
                                 :if-exists         if-exists
                                 :external-format   external-format ; Is this ever applicable?
                                 :element-type      element-type)
      (copy-byte-stream byte-input
                        byte-output 
                        :element-type        element-type 
                        :verify-element-type nil
                        :report-stream       report-stream))))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
