(defun group-pairs (lst)
    "Group every two consecutive list elements into a list"
    (if (null lst)
        nil
        (if (null (cdr lst))
            (list (list (car lst)))
            (cons (list (car lst) (cadr lst)) (group-pairs (cddr lst))))))

(defun run-group-pairs-test (name input-list expected-list)
    "Run a test for the `group-pairs` function with `input-list` and compare it to `expected-list`."
    (let ((actual-output (group-pairs input-list)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun group-pairs-test ()
    (run-group-pairs-test "Test 1: Common list with add number of elemets" '(a b c d e f g) '((a b) (c d ) (e f ) (g)))
    (run-group-pairs-test "Test 2: List is empty         " '() '())
    (run-group-pairs-test "Test 3: List with single nil  " '(nil) '((nil)))
    (run-group-pairs-test "Test 4: List with single atom " '(a) '((a)))
    (run-group-pairs-test "Test 5: List with nested lists" '(a (b c) d e (f) (g)) '((a (b c)) (d e) ((f) (g))))
    (run-group-pairs-test "Test 6: List with nil         " '(a b nil nil d) '((a b) (nil nil) (d)))
    (run-group-pairs-test "Test 7: Very long list        " 
    '(1 2 3 4 5 6 7 8 9 0 a b c d e f g h i j k l m n o p q r s t u v w x y z)
    '((1 2) (3 4) (5 6) (7 8) (9 0) (a b) (c d) (e f) (g h) (i j) (k l) (m n) (o p) (q r) (s t) (u v) (w x) (y z))))


(defun list-set-union (list1 list2)
    "Compute the union of two lists, returning a new list containing all unique elements from lits1 and list2"
    (cond 
        ((and (null list1) (null list2)) nil )
        ((null list1) 
            (if (member (car list2) (cdr list2))
                (list-set-union list1 (cdr list2))
                (cons (car list2) (list-set-union list1 (cdr list2)))))
        ((null list2)
            (if (member (car list1) (cdr list1))
                (list-set-union (cdr list1) list2)
                (cons (car list1) (list-set-union (cdr list1) list2)))) 
        (t
            (if (or (member (car list1) list2) (member (car list1) (cdr list1)))
                (list-set-union (cdr list1) list2)
                (cons (car list1) (list-set-union (cdr list1) list2))))))

(defun run-list-set-union-test (name input-list-1 input-list-2 expected-list)
    "Run a test for the `list-set-union` function with `input-list-1` and `input-list-2` and compare it to `expected-list`"
    (let ((actual-output (list-set-union input-list-1 input-list-2)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun list-set-union-test ()
    (run-list-set-union-test "Test 1: Common elemets           " '(1 2 3) '(2 3 4) '(1 2 3 4))
    (run-list-set-union-test "Test 2: Empty second list        " '(1 2 3) '()  '(1 2 3))
    (run-list-set-union-test "Test 3: Empty first list         " '() '(2 3 4) '(2 3 4))
    (run-list-set-union-test "Test 4: Both lists empty         " '() '() '())
    (run-list-set-union-test "Test 5: No common elements       " '(1 2) '(3 4) '(1 2 3 4))
    (run-list-set-union-test "Test 6: Dublicates in first list " '(1 2 2 3) '(2 3 4) '(1 2 3 4)) ; is this input legal?
    (run-list-set-union-test "Test 7: Different types          " '(1 "two" 3) '(2 3 4) '(1 "two" 2 3 4)) ; is this input legal?
    (run-list-set-union-test "Test 8: Mixed types              " '(1 nil) '(nil 2 3) '(1 nil 2 3))
    (run-list-set-union-test "Test 9: All dublicates           " '(2 2 2) '(2 2) '(2)))
