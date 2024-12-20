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
          (progn
            (format stream (first format-str) new-header)
            (format-table-output format-str stream table new-header)))
            
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