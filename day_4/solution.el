;;; package --- solution day 4 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/4
;;; Code:

(load-file "../input.el")

(defun parse-ranges (str)
  "Parse ranges given as {from}-{to},{from}-{to} in `STR."
  (let ((ranges-as-str (split-string str ",")))
    (cons (parse-range (nth 0 ranges-as-str)) (parse-range (nth 1 ranges-as-str)))))

(defun make-range (from to)
  "Create a range with given `FROM and `TO."
  (cons (string-to-number from) (string-to-number to)))

(defun parse-range (str)
  "Parse a range from `STR in format {from}-{to}."
  (let ((tokens (split-string str "-")))
    (make-range (nth 0 tokens) (nth 1 tokens))))

(defun sub-range-p (r1 r2)
  "Return if `R1 is subrange of `R2."
  (and (>= (car r1) (car r2)) (<= (cdr r1) (cdr r2))))

(defun solution (input find-ranges-fn)
  "Solve problem presented in day 4 on given `INPUT with given `FIND-RANGES-FN."
  (seq-reduce '+
              (seq-map (lambda (bool) (if bool 1 0))
                       (seq-map (lambda (ranges)
                                  (or (funcall find-ranges-fn (car ranges) (cdr ranges)) (funcall find-ranges-fn (cdr ranges) (car ranges))))
                                (seq-map 'parse-ranges
                                         (seq-filter (lambda (str) (not (string-empty-p str))) (split-string input "\n")))))
              0))

(defun part-1 (input)
  "Solve problem part 1 of problem of day 4 with given `INPUT."
  (solution input 'sub-range-p))

(part-1 (get-file-content "./input.txt"))

(defun overlap-p (r1 r2)
  "Return if `R1 overlaps with `R2."
  (and (>= (car r1) (car r2)) (<= (car r1) (cdr r2))))

(defun part-2 (input)
  "Solve problem part 2 of problem of day 4 with given `INPUT."
  (solution input 'overlap-p))

(part-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
