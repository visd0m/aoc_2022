;;; package --- input
;;; Commentary:
;;; utilities to handle input
;;; Code:

(defun get-file-content (filename)
  "Read `FILENAME content and return it as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(provide 'input)
;;; input.el ends here
