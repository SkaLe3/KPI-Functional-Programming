<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Парієнко Віктор Володимирович КВ-11<p>
<p align="right">Рік: 2024<p>


## Загальне завдання
Завдання складаєтьсяз двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
    - використати функції вищого порядку для роботи з послідовностями (де це доречно);
    - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: `key` та `test`, що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому `key` має виконатись мінімальну кількість разів
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом. Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Варіант першої частини 18 (2)
Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням

## Лістинг реалізації першої частини завдання

```lisp
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
```
### Тестові набори та утиліти першої частини

```lisp
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
```
### Тестування першої частини

```lisp
* (sort-pure-test)
Base Test 1: Reverse sorted     : Test passed! Output: (1 2 3 4 5)
Base Test 2: Basic case         : Test passed! Output: (11 12 22 25 34 64 90)
Base Test 3: Sorted             : Test passed! Output: (1 2 3 4 5)
Base Test 4: Identical elements : Test passed! Output: (5 5 5 5 5)
Base Test 5: Single element     : Test passed! Output: (42)
Base Test 6: Empty              : Test passed! Output: NIL
Base Test 7: Negative numbers   : Test passed! Output: (-3 -1 0 2 5 9)
Base Test 8: With duplicates    : Test passed! Output: (1 1 2 3 3 4 5 5 5 6 9)
Key-arguments Test 1: With abs                 : Test passed! Output: (-1 2 3
                                                                       -4 5)
Key-arguments Test 2: Not ascending            : Test passed! Output: (5 3 2 -1
                                                                       -4)
Key-arguments Test 3: With abs not ascending   : Test passed! Output: (5 -4 3 2
                                                                       -1)
Key-arguments Test 4: Strings by length        : Test passed! Output: (kiwi
                                                                       pear
                                                                       apple
                                                                       banana)
Key-arguments Test 5: Complex structures       : Test passed! Output: ((NAME
                                                                        Bob AGE
                                                                        25)
                                                                       (NAME
                                                                        Alice
                                                                        AGE 30)
                                                                       (NAME
                                                                        Charlie
                                                                        AGE 35))
Key-arguments Test 8: Squared values           : Test passed! Output: (0.5 1 -2
                                                                       2 -3)
Key-arguments Test 10: By modulus 3            : Test passed! Output: (3 9 7 4
                                                                       1 2)
NIL
```
## Варіант другої частини 18 (6)
Написати функцію `rpropagation-reducer`, яка має один ключовий параметр - функцію `comparator`. `rpropagation-reducer` має повернути функцію, яка при застосуванні в якості першого аргумента `reduce` робить наступне: при обході списку з кінця, якщо елемент списку-аргумента `reduce` не "кращий" за попередній (той, що "справа") згідно з `comparator`, тоді він заміняється на значення попереднього, тобто "кращого", елемента. Якщо ж він "кращий" за попередній елемент згідно `comparator`, тоді заміна не відбувається. Функція `comparator` за замовчуванням має значення `#'<`. Обмеження, які накладаються на використання функції-результату `rpropagation-reducer` при передачі у `reduce` визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції `reduce` `from-end` та `initial-value`).

```lisp
CL-USER> (reduce (rpropagation-reducer)
                '(3 2 1 2 3)
                :from-end ...
                :initial-value ...)
(1 1 1 2 3)
CL-USER> (reduce (rpropagation-reducer)
                '(3 1 4 2)
                :from-end ...
                :initial-value ...)
(1 1 2 2)
CL-USER> (reduce (rpropagation-reducer :comparator #'>)
                '(1 2 3)
                :from-end ...
                :initial-value ...)
(3 3 3)
```

## Лістинг функції `rpropagation-reducer`

```lisp
(defun rpropagation-reducer (&key (comparator #'<))
  (lambda (current acc-previous)
    (if (null acc-previous)
        (list current)
        (let ((new-acc (if (funcall comparator current (car acc-previous))
                       (cons current acc-previous)  
                       (cons (car acc-previous) acc-previous)))) 
            new-acc))))

(defun rpropagation-reducer-call (input-list &key comparator)
    (reduce (rpropagation-reducer :comparator comparator) input-list :from-end t :initial-value nil))
```
### Тестові набори та утиліти другої частини

```lisp
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
```
### Тестування другої частини

```lisp
* (rpropagation-reducer-test)
Test 1: Base 1                 : Test passed! Output: (1 1 1 2 3)
Test 2: Base 2                 : Test passed! Output: (1 1 2 2)
Test 3: Passed comparator      : Test passed! Output: (3 3 3)
Test 4: Empty list             : Test passed! Output: NIL
Test 5: Single element         : Test passed! Output: (42)
Test 6: All elements the same  : Test passed! Output: (5 5 5 5)
Test 7: Custom comparator      : Test passed! Output: (2 2 2 4)
Test 8: Repeated pattern       : Test passed! Output: (2 2 2 2 3)
Test 9: Mixed increase/decrease: Test passed! Output: (1 1 2 2 3 3)
Test 9: Custom comparator 2    : Test passed! Output: (4 3 3 3 13 5)
NIL
```
