(defun sort-exchange-pure (input &key (key #'identity) (test #'>))
    (labels ((exchange-one-pass (input)
                (if (or (null input) (null (cdr input)))
                    input
                    (let ((first-key(funcall key (car input)))
                            (second-key(funcall key (cadr input))))
                    (if (funcall test first-key second-key)
                        (cons (cadr input) (exchange-one-pass (cons (car input) (cddr input))))
                        (cons (car input) (exchange-one-pass (cdr input)))))))
            (exchage-sort (input)
                (let ((sorted (exchange-one-pass input)))
                    (if (equal sorted input)
                        sorted
                        (exchage-sort sorted)))))
    (exchage-sort input)))


(defun run-sort-pure-test (name input-list expected-list &key (key #'identity) (test #'>))
    "Run a test for the `sort-exchange-pure` function with `input-list` and compare it to `expected-list`"
    (let ((actual-output (sort-exchange-pure input-list :key key :test test)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun sort-pure-test ()
    (run-sort-pure-test "Base Test 1: Reverse sorted     " '(5 4 3 2 1) '(1 2 3 4 5))
    (run-sort-pure-test "Base Test 2: Basic case         " '(64 34 25 12 22 11 90) '(11 12 22 25 34 64 90))
    (run-sort-pure-test "Base Test 3: Sorted             " '(1 2 3 4 5) '(1 2 3 4 5))
    (run-sort-pure-test "Base Test 4: Identical elements " '(5 5 5 5 5) '(5 5 5 5 5))
    (run-sort-pure-test "Base Test 5: Single element     " '(42) '(42))
    (run-sort-pure-test "Base Test 6: Empty              " '() '())
    (run-sort-pure-test "Base Test 7: Negative numbers   " '(5 -3 9 -1 0 2) '(-3 -1 0 2 5 9))
    (run-sort-pure-test "Base Test 8: With duplicates    " '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
    
    (run-sort-pure-test "Key-arguments Test 1: With abs                 " '(3 -4 2 -1 5) '(-1 2 3 -4 5) :key #'abs)
    (run-sort-pure-test "Key-arguments Test 2: Not ascending            " '(3 -4 2 -1 5) '(5 3 2 -1 -4) :test #'<)
    (run-sort-pure-test "Key-arguments Test 3: With abs not ascending   " '(3 -4 2 -1 5) '(5 -4 3 2 -1) :key #'abs :test #'<)
    (run-sort-pure-test "Key-arguments Test 4: Strings by length        " '("apple" "kiwi" "banana" "pear")  '("kiwi" "pear" "apple" "banana") :key #'length)
    (run-sort-pure-test "Key-arguments Test 5: Complex structures       " '((:name "Alice" :age 30) (:name "Bob" :age 25) (:name "Charlie" :age 35))
                                                                          '((:name "Bob" :age 25) (:name "Alice" :age 30) (:name "Charlie" :age 35))
                                                                            :key (lambda (x) (getf x :age)))
    (run-sort-pure-test "Key-arguments Test 8: Squared values           " '(-3 1 0.5 -2 2) '(0.5 1 -2 2 -3) :key (lambda (x) (* x x)))
    (run-sort-pure-test "Key-arguments Test 10: By modulus 3            " '(3 7 2 9 4 1) '(3 9 7 4 1 2) :key (lambda (x) (mod x 3))))





(defun rpropagation-reducer (&key (comparator #'<))
 "Returns a function to be used as the first argument in the `reduce` function.

Restrictions:
- `reduce` must be called with the keyword argument `:from-end` set to `t`
  (i.e., the list traversal must occur from the end).
- `reduce` must be called with the keyword argument `:initial-value` set to `nil`.
  This ensures proper initialization of the accumulator (the result list).
Description:
The function takes a keyword parameter `:comparator`, which defaults to `#'<`.
This comparator determines whether the current element is 'better' than the 
next one. If the current element is not better, it is replaced by the value 
of the 'better' element to the right."
  (lambda (current acc-previous)
    (if (null acc-previous)
        (list current)
        (if (funcall comparator current (car acc-previous))
                       (cons current acc-previous)  
                       (cons (car acc-previous) acc-previous)))))

(defun rpropagation-reducer-call (input-list &key comparator)
    (reduce (rpropagation-reducer :comparator comparator) input-list :from-end t :initial-value nil))

(defun run-rpropagation-reducer-test (name input-list expected-list &key (comparator #'<))
    "Run a test for the `sort-exchange-pure` function with `input-list` and compare it to `expected-list`"
    (let ((actual-output (rpropagation-reducer-call input-list :comparator comparator)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun rpropagation-reducer-test ()
    (run-rpropagation-reducer-test "Test 1: Base 1                 " '(3 2 1 2 3) '(1 1 1 2 3))
    (run-rpropagation-reducer-test "Test 2: Base 2                 " '(3 1 4 2) '(1 1 2 2))
    (run-rpropagation-reducer-test "Test 3: Passed comparator      " '(1 2 3 ) '(3 3 3) :comparator #'>)
    (run-rpropagation-reducer-test "Test 4: Empty list             " '() '())
    (run-rpropagation-reducer-test "Test 5: Single element         " '(42) '(42))
    (run-rpropagation-reducer-test "Test 6: All elements the same  " '(5 5 5 5) '(5 5 5 5))
    (run-rpropagation-reducer-test "Test 7: Custom comparator      " '(1 3 2 4) '(2 2 2 4) 
                                                                    :comparator (lambda (a b) (evenp a)))
    (run-rpropagation-reducer-test "Test 8: Repeated pattern       " '(3 2 3 2 3) '(2 2 2 2 3))    
    (run-rpropagation-reducer-test "Test 9: Mixed increase/decrease" '(10 1 4 2 8 3) '(1 1 2 2 3 3))                                                            
    (run-rpropagation-reducer-test "Test 9: Custom comparator 2    " '(4 0.5 0.2 3 13 5) '(4 3 3 3 13 5) 
                                                                    :comparator (lambda (a b) (> (* a a) 1))))



