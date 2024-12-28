<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Парієнко Віктор Володимирович КВ-11<p>
<p align="right">Рік: 2024<p>


## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію `select`, яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. ЦЕ може бути ключ, список з якоюсь допоміжною інформацією, функція і т.і. За потреби параметрів може бути кілька. `select` повертає лямбда-вираз, який в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у `select`. При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
    * структури у геш-таблиці
    * геш-таблиці у асоціативні списки
    * асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 18 (6)
* База даних: Проєкти із застосуванням ШІ
* Тип записів: Асоціативний список
* Таблиці:   
    1. Проєкти
    2. Моделі штучного інтелекту
* Опис: База даних моделей штучного інтелекту та проєктів, в яких вони використовуються.

## Лістинг реалізації завдання

```lisp
(defun split-string (str &key delimeter)
    (let ((index (position delimeter str))) ;; Find the delimiter's position
        (if index
            (cons (subseq str 0 index) (split-string (subseq str (1+ index)) :delimeter delimeter)) ;; Split at the delimiter
            (list str))))

(defun trim-string (str)
    (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun parse-csv-line (str)
    (mapcar #'trim-string (split-string str :delimeter #\,)))

(defun create-assoc-list (keys values &optional default-value)
 (when keys
      (cons (cons (first keys) (or (first values) default-value))
            (create-assoc-list (rest keys) (rest values) default-value))))


(defun align-assoc-list (keys assoc-list)
  (when keys
      (cons (or (assoc (first keys) assoc-list) (cons (first keys) nil))
            (align-assoc-list (rest keys) assoc-list))))


(defun assoc-list-to-hash-table (assoc-list)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (entry assoc-list)
      (setf (gethash (string (car entry)) hash) (cdr entry)))
    hash))


(defun load-csv (stream &optional header)
  (let ((current-line (read-line stream nil)))
    (if current-line
        (if (null header)
            (load-csv stream (mapcar #'read-from-string (parse-csv-line (trim-string current-line))))
            (cons (create-assoc-list header (parse-csv-line (trim-string current-line)))
                  (load-csv stream header)))
        nil)))

(defun match-filters (row filters)
(loop for (key allowed) on filters by #'cddr
        always (let ((pair (assoc (read-from-string (symbol-name key)) row))) ;; Find the key-value pair in the row
                 (cond
                   ((null pair) nil)  ;; If no matching pair is found, fail
                   ((listp allowed)
                    (if (member (cdr pair) allowed :test #'equalp)
                        t nil))  ;; Check if value is in allowed list
                   (t (equalp (cdr pair) allowed))))))  ;; If not a list, check direct equality


(defun select (file-path &optional filter)
  (lambda (&rest conditions)
    (with-open-file (stream file-path)
      (let ((result nil)
            (records (load-csv stream)))
        (dolist (record records)
          (when (and (or (null filter) (funcall filter record)) ;; Apply filters if present
                     (match-filters record conditions))  ;; Apply conditions
            (push record result)))
        (nreverse result)))))


(defun format-table-output (format-str stream table &optional header)
  (when table
    (if (null header)
        (let ((new-header (mapcar #'car (first table))))
            (format stream (first format-str) new-header)
            (format-table-output format-str stream table new-header))
            
          (progn
          (format stream (second format-str)
                  (mapcar #'cdr (align-assoc-list header (first table))))
          (format-table-output format-str stream (rest table) header)))))

(defun write-csv (stream table)
    (format-table-output (list "~{~A~^,~}~%" "~{~A~^,~}~%") stream table))

(defun print-table (table)
  (let* ((column-width 28) 
         (separator (make-string (* column-width (length (car table))) :initial-element #\-)))
    (format-table-output
     (list
      (concatenate 'string "~{~" (write-to-string column-width) "A~}" '(#\Newline) separator '(#\Newline)) 
       (concatenate 'string "~{~" (write-to-string column-width) "A~}" '(#\Newline)))
     t table)))
```
### Тестові набори та утиліти 

```lisp
(defun test-table-reading()

(let ((models (select "models.csv"))
    (projects (select "projects.csv")))

    (format t "All models:~%")
    (print-table (funcall models))

    (format t "~%Models only on TensorFlow framework:~%")
    (print-table (funcall models :framework '("TensorFlow")))

    (format t "~%All projects:~%")
    (print-table (funcall projects))

    (format t "~%Projects with TrafficNetV2:~%")
    (let* ((tnv2-row (car (funcall models :name "TrafficNetV2"))) (id-col (assoc 'id tnv2-row)) (tnv2-id (cdr id-col)))
        (print-table (funcall projects :model tnv2-id)))

    (format t "~%Projects with model owned by E-CommerceInc started in 2023:~%")
    (let* ((ecominc-rows (funcall models :owner "E-CommerceInc")) (ecominc-ids (mapcar (lambda (r) (cdr (assoc 'id r))) ecominc-rows)))
        (print-table (funcall projects :model ecominc-ids :year "2023")))

    (format t "~%Ongoing projects using models of type longer than 4 characters after 2023:~%")
    (let*
        ((models-m4c (funcall (select "models.csv" (lambda (r) (> (length (cdr (assoc 'type r))) 4)))))
         (models-ids       (mapcar (lambda (r) (cdr (assoc 'ID r))) models-m4c)))
        (print-table (funcall (select "projects.csv" (lambda (r) (>= (parse-integer (cdr (assoc 'year r))) 2023))) :model models-ids)))))

(defun test-table-writing ()
    (let*
        ((models-m4c (funcall (select "models.csv" (lambda (r) (> (length (cdr (assoc 'type r))) 4)))))
         (models-ids       (mapcar (lambda (r) (cdr (assoc 'id r))) models-m4c))
         (projects-2023        (funcall (select "projects.csv" (lambda (r) (>= (parse-integer (cdr (assoc 'year r))) 2023))) :model models-ids)))
        
        (format t "~%Data to write:~%")
        (print-table projects-2023 )
        
        (with-open-file (s "projects_write.csv" :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-csv s projects-2023 ))

        (format t "~%Content of ~a:~%" "projects_write.csv")
        (print-table (funcall (select "projects_write.csv")))))

(defun test-assoc-list-to-hash-table ()
  (let* ((project (funcall (select "projects.csv") :id "4"))
         (project-hash (assoc-list-to-hash-table (car project))))
    
    (format t "~%Starting assoc-list:~%")
    (print-table project)
    
    (format t "~%Hash table:~%")
    (dolist (key '("ID" "MODEL" "NAME" "YEAR" "STATUS"))
      (format t "    ~A: ~A~%" key (gethash key project-hash)))))

(defun test-all ()
    (test-table-reading)
    (test-table-writing)
    (test-assoc-list-to-hash-table))
```
### Тестування
Файл `models.csv`
```
id,name,type,framework,owner
1,TrafficNetV2,ComputerVision,TensorFlow,CityGovAIDept
2,ChatGPTCustom,NLP,PyTorch,E-CommerceInc
3,DiagNet,DeepLearning,TensorFlow,HealthTechAISolutions
```

Файл `projects.csv`
```
id,model,name,year,status
1,1,SmartCityTraffic,2022,Completed
2,2,CustomerServiceChatbot,2023,Ongoing
3,3,MedicalDiagnosisAssistant,2023,Ongoing
4,1,SmartCityParking,2024,Planned
5,2,MultilingualCustomerSupport,2021,Completed
```

```lisp
All models:
ID                          NAME                        TYPE                        FRAMEWORK                   OWNER
--------------------------------------------------------------------------------------------------------------------------------------------
1                           TrafficNetV2                ComputerVision              TensorFlow                  CityGovAIDept
2                           ChatGPTCustom               NLP                         PyTorch                     E-CommerceInc
3                           DiagNet                     DeepLearning                TensorFlow                  HealthTechAISolutions

Models only on TensorFlow framework:
ID                          NAME                        TYPE                        FRAMEWORK                   OWNER
--------------------------------------------------------------------------------------------------------------------------------------------
1                           TrafficNetV2                ComputerVision              TensorFlow                  CityGovAIDept
3                           DiagNet                     DeepLearning                TensorFlow                  HealthTechAISolutions

All projects:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
1                           1                           SmartCityTraffic            2022                        Completed
2                           2                           CustomerServiceChatbot      2023                        Ongoing
3                           3                           MedicalDiagnosisAssistant   2023                        Ongoing
4                           1                           SmartCityParking            2024                        Planned
5                           2                           MultilingualCustomerSupport 2021                        Completed

Projects with TrafficNetV2:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
1                           1                           SmartCityTraffic            2022                        Completed
4                           1                           SmartCityParking            2024                        Planned

Projects with model owned by E-CommerceInc started in 2023:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
2                           2                           CustomerServiceChatbot      2023                        Ongoing

Ongoing projects using models of type longer than 4 characters after 2023:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
3                           3                           MedicalDiagnosisAssistant   2023                        Ongoing
4                           1                           SmartCityParking            2024                        Planned

Data to write:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
3                           3                           MedicalDiagnosisAssistant   2023                        Ongoing
4                           1                           SmartCityParking            2024                        Planned

Content of projects_write.csv:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
3                           3                           MedicalDiagnosisAssistant   2023                        Ongoing
4                           1                           SmartCityParking            2024                        Planned

Starting assoc-list:
ID                          MODEL                       NAME                        YEAR                        STATUS
--------------------------------------------------------------------------------------------------------------------------------------------
4                           1                           SmartCityParking            2024                        Planned

Hash table:
    ID: 4
    MODEL: 1
    NAME: SmartCityParking
    YEAR: 2024
    STATUS: Planned
NIL                                                       
```