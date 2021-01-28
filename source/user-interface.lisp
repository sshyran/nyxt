;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Implementations of update methods for user-interface library widgets

(in-package :nyxt)

(defmethod user-interface:update ((element user-interface:ui-element))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer element)
   (ps:ps (let ((element (ps:chain document (get-element-by-id
                                             (ps:lisp (user-interface:id element))))))
            (setf (ps:chain element |innerHTML|) 
                  (ps:lisp (markup:markup* (user-interface:object-expression element))))))))
