<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Парієнко Віктор Володимирович КВ-11<p>
<p align="right">Рік: 2024<p>



## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: деструктивних операцій, циклів, функцій вищого порядку або фнукцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні цклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

## Варіант 18 (2)
Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням

## Лістинг функції `sort-exchange-pure`

```lisp
(defun exchange-one-pass (input)
    (if (or (null input) (null (cdr input)))
        input
        (if (> (car input) (cadr input))
        (cons (cadr input) (exchange-one-pass (cons (car input) (cddr input))))
        (cons (car input) (exchange-one-pass (cdr input))))))

(defun sort-exchange-pure (input)
    (if (null input)
        input   
        (let ((sorted (exchange-one-pass input)))
            (if (equal sorted input)
                sorted
                (sort-exchange-pure sorted)))))
```
### Тестові набори

```lisp
(defun run-sort-pure-test (name input-list expected-list)
    "Run a test for the `sort-exchange-pure` function with `input-list` and compare it to `expected-list`"
    (let ((actual-output (sort-exchange-pure input-list)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun sort-pure-test ()
    (run-sort-pure-test "Test 1: Reverse sorted     " '(5 4 3 2 1) '(1 2 3 4 5))
    (run-sort-pure-test "Test 2: Basic case         " '(64 34 25 12 22 11 90) '(11 12 22 25 34 64 90))
    (run-sort-pure-test "Test 3: Sorted             " '(1 2 3 4 5) '(1 2 3 4 5))
    (run-sort-pure-test "Test 4: Identical elements " '(5 5 5 5 5) '(5 5 5 5 5))
    (run-sort-pure-test "Test 5: Single element     " '(42) '(42))
    (run-sort-pure-test "Test 6: Empty              " '() '())
    (run-sort-pure-test "Test 7: Negative numbers   " '(5 -3 9 -1 0 2) '(-3 -1 0 2 5 9))
    (run-sort-pure-test "Test 8: With duplicates    " '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
    (run-sort-pure-test "Test 9: Floating point     " '(3.14 2.71 1.41 1.61 0.57) '(0.57 1.41 1.61 2.71 3.14))
    (run-sort-pure-test "Test 10: Mixed floats       " '(-1.5 2.3 3.6 -4.2 0.9) '(-4.2 -1.5 0.9 2.3 3.6)))
```
### Тестування
```lisp
* (sort-pure-test)
Test 1: Reverse sorted     : Test passed! Output: (1 2 3 4 5)
Test 2: Basic case         : Test passed! Output: (11 12 22 25 34 64 90)    
Test 3: Sorted             : Test passed! Output: (1 2 3 4 5)
Test 4: Identical elements : Test passed! Output: (5 5 5 5 5)
Test 5: Single element     : Test passed! Output: (42)
Test 6: Empty              : Test passed! Output: NIL
Test 7: Negative numbers   : Test passed! Output: (-3 -1 0 2 5 9)
Test 8: With duplicates    : Test passed! Output: (1 1 2 3 3 4 5 5 5 6 9)   
Test 9: Floating point     : Test passed! Output: (0.57 1.41 1.61 2.71 3.14)
Test 10: Mixed floats       : Test passed! Output: (-4.2 -1.5 0.9 2.3 3.6)  
NIL
```
## Лістинг функції `sort-exchange-destructive`

```lisp
(defun sort-exchange-destructive (input)
    (let ((result (copy-list input)))
        (loop for i from (1- (length result)) downto 1
            do (loop for j from 0 to (1- i)
                do(when (> (elt result j) (elt result (1+ j)))
                    (let ((temp (elt result j)))
                        (setf (elt result j) (elt result (1+ j)))
                        (setf (elt result (1+ j)) temp)))))
    result))
```
### Тестові набори

```lisp
(defun run-sort-destructive-test (name input-list expected-list)
    "Run a test for the `sort-exchange-destructive` function with `input-list` and compare it to `expected-list`"
    (let ((actual-output (sort-exchange-destructive input-list)))
        (if (equal actual-output expected-list)
            (format t "~A: Test passed! Output: ~A~%" name actual-output)
            (format t "~A: Test failed! Expected: ~A, but got: ~A~%" name expected-list actual-output))))

(defun sort-destructive-test ()
    (run-sort-destructive-test "Test 1: Reverse sorted     " '(5 4 3 2 1) '(1 2 3 4 5))
    (run-sort-destructive-test "Test 2: Basic case         " '(64 34 25 12 22 11 90) '(11 12 22 25 34 64 90))
    (run-sort-destructive-test "Test 3: Sorted             " '(1 2 3 4 5) '(1 2 3 4 5))
    (run-sort-destructive-test "Test 4: Identical elements " '(5 5 5 5 5) '(5 5 5 5 5))
    (run-sort-destructive-test "Test 5: Single element     " '(42) '(42))
    (run-sort-destructive-test "Test 6: Empty              " '() '())
    (run-sort-destructive-test "Test 7: Negative numbers   " '(5 -3 9 -1 0 2) '(-3 -1 0 2 5 9))
    (run-sort-destructive-test "Test 8: With duplicates    " '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
    (run-sort-destructive-test "Test 9: Floating point     " '(3.14 2.71 1.41 1.61 0.57) '(0.57 1.41 1.61 2.71 3.14))
    (run-sort-destructive-test "Test 10: Mixed floats       " '(-1.5 2.3 3.6 -4.2 0.9) '(-4.2 -1.5 0.9 2.3 3.6))
   )
```
### Тестування

```lisp
* (sort-destructive-test)
Test 1: Reverse sorted     : Test passed! Output: (1 2 3 4 5)
Test 2: Basic case         : Test passed! Output: (11 12 22 25 34 64 90)
Test 3: Sorted             : Test passed! Output: (1 2 3 4 5)
Test 4: Identical elements : Test passed! Output: (5 5 5 5 5)
Test 5: Single element     : Test passed! Output: (42)
Test 6: Empty              : Test passed! Output: NIL
Test 7: Negative numbers   : Test passed! Output: (-3 -1 0 2 5 9)
Test 8: With duplicates    : Test passed! Output: (1 1 2 3 3 4 5 5 5 6 9)
Test 9: Floating point     : Test passed! Output: (0.57 1.41 1.61 2.71 3.14)
Test 10: Mixed floats       : Test passed! Output: (-4.2 -1.5 0.9 2.3 3.6)
NIL
```