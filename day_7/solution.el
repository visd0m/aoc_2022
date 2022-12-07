;;; package --- solution day 7 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/7
;;; Code:

(load-file "../input.el")

(defun add-last (elem list)
  "Add `ELEM as last element of `LIST."
  (if (not list)
      (cons elem nil)
    (cons (car list) (add-last elem (cdr list)))))

(defun remove-last (list)
  "Remove last element of `LIST."
  (if (not (cdr list))
      nil
    (cons (car list) (remove-last (cdr list)))))

(defun node (name size children)
  "Create a node with given `NAME,`SIZE and `CHILDREN."
  (cons (cons name size) children))

(defun name (node)
  "Get the name of `NODE."
  (car (car node)))

(defun add-child (name node child)
  "Add `CHILD to node with `NAME in `NODE."
  (if (equal name (name node))
      (let ((children (cons child (children node))))
        (node (name node) 0 children))
    (node (name node) (cdr (car node)) (seq-map (lambda (node-child) (add-child name node-child child)) (children node)))))

(defun children (node)
  "Get the children of given `NODE."
  (cdr node))

(defun size (node acc limit comparison)
  "Calculate she size of the given `NODE returning matching sizes in `ACC accroding to `LIMIT and `COMPARISON."
  (let ((children (children node))
        (node-size (cdr (car node))))
    (if (not children)
        (cons node-size acc)
      (let ((dir-size-acc (seq-reduce
                           (lambda (acc size-acc)
                             (cons (+ (car acc) (car size-acc)) (seq-concatenate 'list (cdr acc) (cdr size-acc))))
                           (seq-map (lambda (node-child) (size node-child acc limit comparison)) children)
                           (cons 0 acc))))
        (if (funcall comparison (car dir-size-acc) limit)
            (cons (car dir-size-acc) (cons (car dir-size-acc) (cdr dir-size-acc)))
          (cons (car dir-size-acc) (cdr dir-size-acc)))))))

(defun interpret-output (line fs current-path)
  "Read `LINE and interpret it as output updating `FS accordingly to `CURRENT-PATH."
  (let* ((current-node (string-join current-path))
         (tokens (split-string (string-replace "dir" "0" line) " "))
         (size (string-to-number (nth 0 tokens)))
         (name (nth 1 tokens)))
    (cons (add-child current-node fs (node (format "%s%s" current-node name) size nil)) current-path)))

(defun interpret-command (line fs current-path)
  "Read `LINE and interpret it as command updating `FS accordingly to `CURRENT-PATH."
  (let* ((tokens (split-string line " "))
         (command (nth 1 tokens)))
    (pcase command
      ("ls" (cons fs current-path))
      ("cd" (let ((to-go (nth 2 tokens)))
              (pcase to-go
                (".." (cons fs (remove-last current-path)))
                (_ (cons fs (add-last to-go current-path)))))))))

(defun interpret-line (line fs current-path)
  "Interpret `LINE updating `FS accordingly to `CURRENT-PATH."
  (if (equal (substring line 0 1) "$")
      (interpret-command line fs current-path)
    (interpret-output line fs current-path)))

(defun parse-file-system (input)
  "Parse given `INPUT representing filesystem described."
  (let* ((fs (node "/" 0 nil))
         (current-path nil)
         (input-lines (seq-filter (lambda (line) (not (string-empty-p line))) (split-string input "\n"))))
    (car (seq-reduce (lambda (acc line) (interpret-line line (car acc) (cdr acc))) input-lines (cons fs current-path)))))

(defun solution-part-1 (input)
  "Provide solution for problem presented in day 6 part 1 for a given `INPUT."
  (let* ((fs (parse-file-system input))
         (fs-size-acc (size fs (list) 100000 '<=))
         (sum (seq-reduce '+ (cdr fs-size-acc) 0)))
    sum))

(solution-part-1 (get-file-content "./input.txt"))

(defun solution-part-2 (input)
  "Provide solution for problem presented in day 6 part 2 for a given `INPUT."
  (let* ((fs (parse-file-system input))
         (fs-size-acc (size fs (list) 0 '<))
         (total-used-size (car fs-size-acc))
         (free-space (- 70000000 total-used-size))
         (to-free (- 30000000 free-space))
         (fs-size-acc (size fs (list) to-free '>=))
         (to-delete (seq-min (cdr fs-size-acc))))
    to-delete))

(solution-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
