<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Парієнко Віктор Володимирович КВ-11<p>
<p align="right">Рік: 2024<p>



## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно реалізувати, задаються варіантом (п.2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій для роботи зі списками, що не наведені в четвертому розділі навчального посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 18 (3)
1. Написати функцію `group-pairs`, яка групує послідовні пари елементів у списки:
```lisp
(group-pairs '(a b c d e f g))
((A B) (C D) (E F) (G))
```
2. Написати фуннкцію `list-set-union`, яка визначає об'єднання двох множин, заданих списками атомів:
```lisp
(list-set-union '(1 2 3) '(2 3 4))
(1 2 3 4) ; порядок може відрізнятись
```

## Лістинг функції `group-pairs`

```lisp
(defun group-pairs (lst)
    "Group every two consecutive list elements into a list"
    (if (null lst)
        nil
        (if (null (cdr lst))
            (list (list (car lst)))
            (cons (list (car lst) (cadr lst)) (group-pairs (cddr lst))))))
```

### Тестові набори

```lisp
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

```
### Тестування

```lisp
* (group-pairs-test)
Test 1: Common list with add number of elemets: Test passed! Output: ((A B)
                                                                      (C D)
                                                                      (E F) (G))
Test 2: List is empty         : Test passed! Output: NIL
Test 3: List with single nil  : Test passed! Output: ((NIL))
Test 4: List with single atom : Test passed! Output: ((A))
Test 5: List with nested lists: Test passed! Output: ((A (B C)) (D E) ((F) (G)))
Test 6: List with nil         : Test passed! Output: ((A B) (NIL NIL) (D))
Test 7: Very long list        : Test passed! Output: ((1 2) (3 4) (5 6) (7 8)
                                                      (9 0) (A B) (C D) (E F)
                                                      (G H) (I J) (K L) (M N)
                                                      (O P) (Q R) (S T) (U V)
                                                      (W X) (Y Z))
NIL
```
## Лістинг функції `list-set-union`

```lisp
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
```
### Тестові набори

```lisp
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

```
### Тестування

```lisp
* (list-set-union-test)
Test 1: Common elemets           : Test passed! Output: (1 2 3 4)
Test 2: Empty second list        : Test passed! Output: (1 2 3)
Test 3: Empty first list         : Test passed! Output: (2 3 4)
Test 4: Both lists empty         : Test passed! Output: NIL
Test 5: No common elements       : Test passed! Output: (1 2 3 4)
Test 6: Dublicates in first list : Test passed! Output: (1 2 3 4)
Test 7: Different types          : Test passed! Output: (1 two 2 3 4)
Test 8: Mixed types              : Test passed! Output: (1 NIL 2 3)
Test 9: All dublicates           : Test passed! Output: (2)
NIL
```