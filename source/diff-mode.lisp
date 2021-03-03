;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/diff-mode
    (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for displaying web-buffer diffs."))
(in-package :nyxt/diff-mode)

(define-command nyxt::diff-buffers ()
  "Creates a diff between two buffers."
  (flet ((make-diff-buffer (old-html new-html html-diff-string)
           (let* ((buffer (make-buffer :title "*diff*" :modes '(diff-mode)))
                  (mode (find-submode buffer 'diff-mode)))
             (nyxt::html-set (str:concat  (markup:markup (:style (diff-style mode)))
                                          html-diff-string)
                             buffer)
             (set-current-buffer buffer)
             (setf (old-html mode) old-html)
             (setf (new-html mode) new-html))))
    (let* ((old-html (ffi-buffer-get-document
                      (prompt-minibuffer
                       :input-prompt "Old buffer"
                       :suggestion-function (buffer-suggestion-filter))))
           (new-html (ffi-buffer-get-document
                      (prompt-minibuffer
                       :input-prompt "New buffer"
                       :suggestion-function (buffer-suggestion-filter
                                             :current-is-last-p t))))
           (diff-html (html-diff:html-diff old-html
                                           new-html
                                           :insert-class "nyxt-diff-insert"
                                           :delete-class "nyxt-diff-delete"
                                           :replace-class "nyxt-diff-replace")))
      (make-diff-buffer old-html
                        new-html
                        (princ diff-html)))))

(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  ((keymap-scheme (define-scheme "diff"
                    scheme:cua
                    (list "q" 'delete-current-buffer))
                  :type keymap:scheme)
   (new-html :documentation "TODO")
   (old-html :documentation "TODO")
   (diff-style (cl-css:css
                '((".nyxt-diff-insert"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  ("ins.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  (".nyxt-diff-delete"
                   :text-decoration "none"
                   :background-color "#efcbcf")
                  ("del.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#efcbcf")))
               :documentation "Colours based on the modus-operandi theme by
Protesilaos Stavrou, which follows the highest standard on accessibility.")))
