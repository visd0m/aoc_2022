;;; package --- solution day 7 aoc2022
;;; Commentary:
;;; reference https://adventofcode.com/2022/day/7
;;; Code:

(load-file "../input.el")

(defun parse-file-system (input)
  "Parse given `INPUT representing filesystem described."
  (let* ((fs (node "/" 0 nil))
         (current-path nil)
         (input-lines (seq-filter (lambda (line) (not (string-empty-p line))) (split-string input "\n"))))
    (seq-reduce (lambda (acc line) (interpret-line line (car acc) (cdr acc))) input-lines (cons fs current-path))))

(message (format "%s" (parse-file-system "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")))

(defun interpret-line (line fs current-path)
  "Interpret `LINE updating `FS accordingly to `CURRENT-PATH."
  (if (equal (substring line 0 1) "$")
      (interpret-command line fs current-path)
    (interpret-output line fs current-path)))

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

(add-last 1 '(1 2 3))

(defun interpret-output (line fs current-path)
  "Read `LINE and interpret it as output updating `FS accordingly to `CURRENT-PATH."
  (let* ((current-node (nth 0 (last current-path)))
         (tokens (split-string (string-replace "dir" "0" line) " "))
         (size (nth 0 tokens))
         (name (nth 1 tokens)))
    (message "++++++++++++++++++++++")
    (message (format "name: %s" name))
    (message (format "size: %s" size))
    (message (format "current-path: %s" current-path))
    (message (format "current-node: %s" current-node))
    (cons (add-child current-node fs (node name size nil)) current-path)))

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
    (node (name node) 0 (seq-map (lambda (node-child) (add-child name node-child child)) (children node)))))

(defun children (node)
  "Get the children of given `NODE."
  (cdr node))

(defun size (node)
  "Calculate she size of the given `NODE."
  (let ((children (children node))
        (node-size (cdr (car node))))
    (if (not children)
        node-size
      (+ node-size (seq-reduce '+ (seq-map 'size children) 0)))))

(setq fs (node "/" 0 (list (node "foo.txt" 100 nil)
                  (node "foo-dir" 0
                        (list (node "bar" 1 nil) (node "baz" 2 nil))))))

(add-child "bar" fs (node "foobar" 1 nil))

fs

(size (node "/" 0 (list (node "foo.txt" 100 nil)
                  (node "foo-dir" 0
                        (list (node "bar" 1 nil) (node "baz" 2 nil))))))

(provide 'solution)
;;; solution.el ends here
