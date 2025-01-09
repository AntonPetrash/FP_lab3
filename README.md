## МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС

### Звіт з лабораторної роботи 3
 "Функціональний і імперативний підходи до роботи зі списками"
 дисципліни "Вступ до функціонального програмування"

**Студент**: *Петраш Антон Степанович КВ-13*


**Рік**: *2025*

## Завдання:
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: псевдо-функцій, деструктивних операцій,
циклів, функцій вищого порядку або функцій для роботи зі списками/
послідовностями, що використовуються як функції вищого порядку. Також
реалізована функція не має бути функціоналом (тобто приймати на вхід функції в
якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
звіту наведені в п. 3.2.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

### Варіант 13(5):
Варіант 5
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

**Код програми:**
```
(defun bubble-sort-func (lst)
  (labels ((bubble-pass (lst)
             (if (or (null lst) (null (cdr lst))) 
                 (values lst nil) ; Якщо список порожній або один елемент
                 (let ((first (car lst))
                       (second (cadr lst)))
                   (if (> first second)
                       ;; Потрібна заміна
                       (multiple-value-bind (rest flag)
                           (bubble-pass (cons first (cddr lst)))
                         (values (cons second rest) t))
                       ;; Без заміни
                       (multiple-value-bind (rest flag)
                           (bubble-pass (cdr lst))
                         (values (cons first rest) flag)))))))
    ;; Основний рекурсивний цикл
    (labels ((sort-helper (lst)
               (multiple-value-bind (sorted flag)
                   (bubble-pass lst)
                 (if flag
                     (sort-helper sorted)
                     sorted))))
      (sort-helper lst))))

(defun bubble-sort-imp (lst)
  (let ((copy (copy-list lst))
        (flag t))
    ;; Циклічне сортування
    (loop
      while flag
      do (progn
           (setf flag nil)
           (loop for i from 0 below (1- (length copy))
                 do (when (> (nth i copy) (nth (1+ i) copy))
                      ;; Обмін елементів місцями
                      (rotatef (nth i copy) (nth (1+ i) copy))
                      (setf flag t)))))
    copy))

(defun test-bubble-sort ()
  (format t "Functional Sort Tests:~%")
  (format t "Test 1: ~A~%" (equal (bubble-sort-func '(3 1 4 1 5 9)) '(1 1 3 4 5 9)))
  (format t "Test 2: ~A~%" (equal (bubble-sort-func '(5 3 8 6 2 7)) '(2 3 5 6 7 8)))
  (format t "Test 3: ~A~%" (equal (bubble-sort-func '(1 2 1 -3 -2)) '(-3 -2 1 1 2)))
  (format t "Test 4: ~A~%" (equal (bubble-sort-func '(1 2 3 4 5 6)) '(1 2 3 4 5 6)))
  (format t "Test 5: ~A~%" (equal (bubble-sort-func '(1)) '(1)))
  (format t "Test 6: ~A~%" (equal (bubble-sort-func '()) '()))

  (format t "Imperative Sort Tests:~%")
  (format t "Test 1: ~A~%" (equal (bubble-sort-imp '(3 1 4 1 5 9)) '(1 1 3 4 5 9)))
  (format t "Test 2: ~A~%" (equal (bubble-sort-imp '(5 3 8 6 2 7)) '(2 3 5 6 7 8)))
  (format t "Test 3: ~A~%" (equal (bubble-sort-func '(1 2 1 -3 -2)) '(-3 -2 1 1 2)))
  (format t "Test 4: ~A~%" (equal (bubble-sort-func '(1 2 3 4 5 6)) '(1 2 3 4 5 6)))
  (format t "Test 5: ~A~%" (equal (bubble-sort-imp '(1)) '(1)))
  (format t "Test 6: ~A~%" (equal (bubble-sort-imp '()) '())))

(test-bubble-sort)
```
**Результат тестування:**
```
Output:

Functional Sort Tests:
Test 1: T
Test 2: T
Test 3: T
Test 4: T
Test 5: T
Test 6: T
Imperative Sort Tests:
Test 1: T
Test 2: T
Test 3: T
Test 4: T
Test 5: T
Test 6: T
```
