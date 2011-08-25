;;; :FILE-CREATED <Timestamp: #{2011-08-25T18:46:57-04:00Z}#{11344} - by MON>
;;; :FILE clime/clime-hash-directory-tree.lisp
;;; ==============================

;; :NOTE Our main function here is `walk-directory-images-to-hash'

(in-package #:clime)
;; *package*

(declaim (inline %ensure-simple-string
                 %walk-directory-filter-ignorables
                 %absolute-existent-file-or-directory
                 clrhash-image-hashes
                 report-image-hash-count))

(defun %ensure-simple-namestring (namething)
  (declare ((or pathname string) namething)
           (optimize (speed 3)))
  (when (simple-string-p namething)
    (return-from %ensure-simple-namestring (the simple-string namething)))
  (let ((string-thing (if (pathnamep namething)
                          (namestring namething)
                          namething)))
    (declare (string string-thing))
    (the simple-string
      (make-array (length string-thing)
                  :element-type 'character
                  :initial-contents string-thing))))

(defun %walk-directory-filter-ignorables (path-or-namestring)
  ;; (%walk-directory-filter-ignorables "lost+found/")
  ;; (%walk-directory-filter-ignorables "lost+found")
  (declare (inline %ensure-simple-namestring)
           (optimize (speed 3)))
  (let ((ensured-simple (%ensure-simple-namestring path-or-namestring)))
    (declare (simple-string ensured-simple))
    (flet ((chk-ignore (maybe-ignore)
             (declare (simple-string maybe-ignore))
             (string= maybe-ignore ensured-simple)))
      (declare (list *walk-directory-ignorables*))
      (notany #'chk-ignore *walk-directory-ignorables*))))

;; :NOTE Should this explicitly pass value of osicat:current-directory to
;; osicat:absolute-pathname? When invoked within the body of
;; osicat:walk-directory *default-pathname-defaults* is already dynamically
;; bound to osicat:current-directory and the extra overhead is likely costly.
(defun %absolute-existent-file-or-directory (maybe-file-or-directory)
  (declare (inline %ensure-simple-namestring
                   %walk-directory-filter-ignorables))
  (let* ((abs      (osicat:absolute-pathname maybe-file-or-directory)) ;; (osicat:current-directory)
         (abs-kind (osicat::get-file-kind abs nil)))
    (and abs-kind 
         (or (eql abs-kind :regular-file)
             (eql abs-kind :directory))
         (%walk-directory-filter-ignorables maybe-file-or-directory)
         t)))
      
(defun %partition-walked-files (rel-file-or-directory-pathname)
  (declare (pathname rel-file-or-directory-pathname) 
           (inline %ensure-simple-namestring) 
           (optimize (speed 3))) 
  (declare (pathname rel-file-or-directory-pathname))
  (let* ((regular-p          (osicat:regular-file-exists-p 
                              (osicat:absolute-pathname rel-file-or-directory-pathname))) ;*default-pathname-defaults*)))
         (regular-namestring (if regular-p
                                 (%ensure-simple-namestring regular-p)
                                 (return-from %partition-walked-files nil))))
    (declare (simple-string regular-namestring))
    ;; (print regular-p *standard-output*) (print regular-namestring *standard-output*)
    (labels ((cache-if-image (scanner hash)
               ;; (declare (function scanner) (hash-table hash))
               (declare (hash-table hash))
               (multiple-value-bind (match-string extension) (cl-ppcre:scan-to-strings scanner regular-namestring :sharedp t)
                 ;; (print scanner *standard-output*)
                 (when match-string
                   ;; (print match-string *standard-output*)
                   ;;
                   ;; Check if were looking at a match from *bmp-gz-scanner*,
                   ;; *jpg-gz-scanner* if so, its exension is bmp.gz, jpg.gz, or
                   ;; jpeg.gz and we need to take the pathname-name of its
                   ;; pathname-name
                   ;; (if gz  (cl-ppcre:register-groups-bind (ext dot gz)  (*extension-gz-scanner* (aref extension 0))
                   (if (string= "gz" (aref extension 0) :start2 (- (length (aref extension 0))  2))
                       (setf (gethash match-string hash)
                             (list (directory-namestring match-string)
                                   (pathname-name (pathname-name match-string))
                                   (aref extension 0)))
                       (setf (gethash match-string hash)
                             (list (directory-namestring match-string)
                                   (pathname-name match-string)
                                   (aref extension 0))))
                   t)))
             (push-other ()
               (and (setf (gethash regular-namestring *other-hash*) 
                          (list (directory-namestring regular-namestring)
                                (pathname-name regular-namestring)
                                (pathname-type regular-namestring)))
                    t))
             (map-pairs ()
               (loop 
                  for (fun . cache) in '((*psd-scanner*    . *psd-hash*)
                                         (*jpg-gz-scanner* . *jpg-gz-hash*)
                                         (*jpg-scanner*    . *jpg-hash*)
                                         (*bmp-scanner*    . *bmp-hash*)
                                         (*bmp-gz-scanner* . *bmp-gz-hash*)
                                         (*nef-scanner*    . *nef-hash*)
                                         (*tiff-scanner*   . *tiff-hash*))
                  for chk = (cache-if-image (symbol-value fun) (symbol-value cache))
                  ;do (print chk *standard-output*)
                  when chk do (loop-finish) ;; (return-from map-pairs chk) ;;do 
                  finally (return
                            (if chk
                                chk
                                (push-other))))))
      (map-pairs)))) 

(defun clrhash-image-hashes ()
  ;; (clrhash-image-hashes)
  (declare (optimize (speed 3)))
  (mapc #'clrhash `(,*nef-hash*
                    ,*bmp-hash*
                    ,*bmp-gz-hash*
                    ,*jpg-hash*
                    ,*jpg-gz-hash*
                    ,*psd-hash*
                    ,*tiff-hash*
                    ,*other-hash*)))

(defun report-image-hash-count ()
  ;; (report-image-hash-count)
  (declare (optimize (speed 3)))
  (flet ((ht-and-count (ht)
           (cons ht (hash-table-count (symbol-value ht)))))
    (mapcar #'ht-and-count '(*nef-hash*
                             *bmp-hash*
                             *bmp-gz-hash*
                             *jpg-hash*
                             *jpg-gz-hash*
                             *psd-hash*
                             *tiff-hash*                    
                             *other-hash*))))
;; Our main function 
(defun walk-directory-images-to-hash (directory-pathname)
  (declare ((or pathname string) directory-pathname)
           (inline 
             %absolute-existent-file-or-directory
             %ensure-simple-namestring             
             report-image-hash-count
             clrhash-image-hashes)
           (optimize (speed 3)))
  (clrhash-image-hashes)
  (osicat:walk-directory (%ensure-simple-namestring directory-pathname)
                         #'%partition-walked-files
                         :directories :breadth-first ;;  :depth-first
                         :test #'%absolute-existent-file-or-directory)
  (report-image-hash-count))

;;; ==============================
;;; EOF
