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
