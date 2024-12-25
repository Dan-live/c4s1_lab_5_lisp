
(in-package :cl-user)

(defstruct project
  id
  name
  description
  start-date
  end-date
  model-id)

(defstruct model
  id
  name
  type
  description)

(defun project-slots ()
  '(id name description start-date end-date model-id))

(defun model-slots ()
  '(id name type description))

(defun my-split-sequence (delimiter string)
  "Splits STRING by the DELIMITER into a list of substrings."
  (let ((start 0)
        (result '()))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (1+ pos)))
    (push (subseq string start) result)
    (nreverse result)))


(defun create-project-record (row)
  "Converts a CSV row into a PROJECT structure."
  (format t "Parsing row: ~a~%" row)  ; Виводимо рядок перед обробкою
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 6)  
        (let ((id-field (nth 0 fields)))
          (format t "ID field before parsing: ~a~%" id-field)  ; Діагностика
          (make-project
           :id (parse-integer id-field)  ; Пробуємо зчитати ID
           :name (nth 1 fields)
           :description (nth 2 fields)
           :start-date (nth 3 fields)
           :end-date (nth 4 fields)
           :model-id (parse-integer (nth 5 fields))))
        (progn
          (format t "Error: Invalid row format ~a~%" row)
          nil))))  


(defun create-project-record (row)
  "Converts a CSV row into a PROJECT structure."
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 6)
        (make-project
         :id (parse-integer (nth 0 fields) :junk-allowed t)
         :name (nth 1 fields)
         :description (nth 2 fields)
         :start-date (nth 3 fields)
         :end-date (nth 4 fields)
         :model-id (parse-integer (string-trim '(#\Space #\Tab #\Newline) (nth 5 fields)) :junk-allowed t))
        (progn
          (format t "Error: Invalid row format: ~a~%" row)
          nil))))


(defun create-model-record (row)
  "Converts a CSV row into a MODEL structure."
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 4)  
        (make-model
         :id (parse-integer (nth 0 fields))
         :name (nth 1 fields)
         :type (nth 2 fields)
         :description (nth 3 fields))
        (progn
          (format t "Error: Invalid row ~a~%" row)
          nil))))  


(defun read-csv-file (file-path create-record-fn)
  "Reads a CSV file and returns a list of records created using create-record-fn."
  (with-open-file (stream file-path :if-does-not-exist nil)
    (if stream
        (let ((header (read-line stream nil)))
          (if (some (lambda (field) (string= field "ID")) (my-split-sequence #\, header))
              (loop for line = (read-line stream nil)
                    while line
                    do (format t "Processing line: ~a~%" line)
                    collect (funcall create-record-fn line))
              (cons (funcall create-record-fn header)
                    (loop for line = (read-line stream nil)
                          while line
                          do (format t "Processing line: ~a~%" line)
                          collect (funcall create-record-fn line)))))
        (progn
          (format t "Error: Cannot read file ~a~%" file-path)
          nil))))


(defun select (file-path create-record-fn)
  "Returns a lambda that selects records from a CSV file, optionally filtering by field values."
  (let ((records (read-csv-file file-path create-record-fn)))
    (lambda (&rest filters)
      (let ((filtered-records records))
        (dolist (filter filters)
          (let ((field (car filter))
                (value (cdr filter)))
            (format t "Filtering by ~a = ~a~%" field value)
            (setf filtered-records
                  (remove-if-not
                   (lambda (record)
                     (let ((slot-val (slot-value record field)))
                       (format t "Checking record ~a | Field ~a = ~a~%" record field slot-val)
                       (equal value slot-val)))
                   filtered-records))))
        filtered-records))))


;;; для запису вибірки у CSV-файл
(defun write-records-to-csv (file-path records)
  "Writes a list of records to a CSV file."
  (with-open-file (stream file-path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (if stream
        (progn
          (dolist (record records)
            (let ((slots (cond
                          ((typep record 'project) (project-slots))
                          ((typep record 'model) (model-slots)))))
              (format stream "~{~a~^,~}~%" 
                      (mapcar (lambda (slot) (slot-value record slot)) slots))))
          (format t "Data successfully written to ~a~%" file-path))
        (format t "Error: Could not open file ~a~%" file-path))))


;;; Конвертація записів
(defun structure-to-hashtable (record)
  "Converts a structure to a hash table."
  (let ((hash (make-hash-table))
        (slots (cond
                ((typep record 'project) (project-slots))
                ((typep record 'model) (model-slots)))))
    (dolist (slot slots)
      (setf (gethash slot hash) (slot-value record slot)))
    hash))

(defun hashtable-to-alist (hash)
  "Converts a hash table to an alist."
  (loop for key being the hash-keys of hash
        collect (cons key (gethash key hash))))

(defun alist-to-hashtable (alist)
  "Converts an alist to a hash table."
  (let ((hash (make-hash-table)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

;;; "Красивий" вивід записів
(defun pretty-print-records (records)
  "Prints a list of records in a human-readable format."
  (dolist (record records)
    (let ((slots (cond
                  ((typep record 'project) (project-slots))
                  ((typep record 'model) (model-slots)))))
      (format t "Record of type ~a:~%" (type-of record))
      (dolist (slot slots)
        (format t "  ~a: ~a~%" slot (slot-value record slot)))
      (format t "~%"))))

;;; Тестування функцій
; (defvar  *projects* (read-csv-file "projects.csv" #'create-project-record))
; (defvar  *models* (read-csv-file "models.csv" #'create-model-record))
(defvar *projects* (read-csv-file "d:/University/k4s1/func_prog/Lab_5/first_proj/projects.csv" #'create-project-record))
(defvar *models* (read-csv-file "d:/University/k4s1/func_prog/Lab_5/first_proj/models.csv" #'create-model-record))



;; Виводимо всі проєкти перед записом у output.csv
(format t "Records to write to output.csv:~%")
(pretty-print-records *projects*)
(pretty-print-records *models*)

(write-records-to-csv "d:/University/k4s1/func_prog/Lab_5/first_proj/output.csv" *projects*)



(defstruct joined-data
  project
  model)


(defun join-projects-with-models (projects models)
  "З'єднує проєкти з моделями за model-id."
  (mapcar
   (lambda (project)
     (let ((model (find (project-model-id project) models :key #'model-id)))
       (make-instance 'joined-data
                      :project project
                      :model model))) 
   projects))



;Тестування функції select для фільтрації проєктів за model-id 

(let ((select-projects (select "d:/University/k4s1/func_prog/Lab_5/first_proj/projects.csv" #'create-project-record)))
  (let ((filtered-projects (funcall select-projects '(model-id . 5))))
    (if filtered-projects
        (progn
          (format t "Filtered Projects:~%")
          (pretty-print-records filtered-projects)
          (write-records-to-csv "d:/University/k4s1/func_prog/Lab_5/first_proj/filtered_projects.csv" filtered-projects))
        (format t "No projects found for model-id = 1~%"))))