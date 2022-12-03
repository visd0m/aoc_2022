;;; package --- solution day 3 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/3
;;; Code:

(load-file "../input.el")

(defun solution (input grouping-fn)
  "Given `INPUT and `GROUPING-FN solve problem presented in day 3."
  (seq-reduce `+
              (seq-map 'get-shared-item
                       (funcall grouping-fn
                                (seq-filter (lambda (line) (not (string-empty-p line))) (split-string input "\n"))))
              0))

(defun by-compartments (rucksacks)
  "Map `RUCKSACKS in compartments."
  (seq-map 'get-compartments rucksacks))

(defun get-compartments (rucksack)
  "Get the copartments of the given `RUCKSACK."
  (let* ((rucksack-lenght (length rucksack))
         (first-compartment (string-to-vector (substring rucksack 0 (/ rucksack-lenght 2))))
         (second-compartment (string-to-vector (substring rucksack (/ rucksack-lenght 2) rucksack-lenght))))
    (list first-compartment second-compartment)))

(defun get-shared-item (sets)
  "Get item shared between `SETS."
  (priority (nth 0 (reduce 'seq-intersection sets))))

(defun priority (item)
  "Get priority of given `ITEM."
  (if (>= item 97)
      (- item 96)
    (- item 38)))

(defun part-1 (input)
  "Given `INPUT, solve problem presented in part 1 of day 3."
  (solution input 'by-compartments))

(part-1 (get-file-content "./input.txt"))

(defun by-group-of-three (rucksacks)
  "Group `RUCKSACKS by group of three."
  (seq-partition rucksacks 3))

(defun part-2 (input)
  "Given `INPUT, solve problem presented in part 2 of day 3."
  (solution input 'by-group-of-three))

(part-2 (get-file-content "./input.txt"))

(provide 'solution)
;;; solution.el ends here
