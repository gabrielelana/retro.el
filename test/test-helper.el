;;; test-helper.el --- helper functions for tests -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; helper functions for tests

;;; Code:

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

;; Local Variables:
;; coding: utf-8
;; End:
;;; test-helper.el ends here
