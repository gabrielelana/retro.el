(require 'cl-lib)

(defmacro with-content-file (name content &rest body)
  "Create a unique file with a given CONTENT, eval BODY and delete it.

Generate a unique file. Store its path in symbol NAME. Write
CONTENT to the file. CONTENT can be a string or a list of strings
that will be concatenated with a newline. BODY will be evaluated,
therefore BODY can access the file through its path in NAME. No
matter what, at the end the file will be deleted."
  (declare (indent defun))
  (cl-assert (and name (symbolp name) t "NAME must be a symbol"))
  `(let ((,name (make-temp-file "wcf-macro")))
     (unwind-protect
         (cl-assert (or (stringp ,content) (seq-every-p 'stringp ,content)) t
                    "CONTENT must be a string or a list of strings")
       (progn (with-temp-file ,name
                (insert (mapconcat 'identity ,content "\n")))
              ,@body)
       (delete-file ,name))))

(provide 'test-helper)
