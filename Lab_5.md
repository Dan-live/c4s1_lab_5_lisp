<p align="center"><b>Національний технічний університет України “Київський політехнічний інститут ім. Ігоря Сікорського”</b></p>
<p align="center"><b>Факультет прикладної математики Кафедра системного програмування і спеціалізованих комп’ютерних систем</b></p>
<p align="center"><b>ЛАБОРАТОРНА РОБОТА №5</b></p>
<p align="center"><b>з дисципліни «Вступ до функціонального програмування»</b></p>

<div align="right">
    <p>Студент: Горбик Данііл</p>
    <p>Група: КВ-13</p>
    <p>Рік: 2024</p>
</div>
## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
	- структури у геш-таблиц
	- геш-таблиці у асоціативні списки
	- асоціативні списки у геш-таблиц
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Завдання за варіантом №4

1. Проєкти із застосуванням ШІ Структура

```lisp

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




```

## Таблиці CSV

```
ID,NAME,DESCRIPTION,START-DATE,END-DATE,MODEL-ID
1,AI Research,Description 1,2024-01-01,2024-12-31,5
2,AI Robotics,Description 2,2023-05-10,2023-12-25,4
3,AI Assistant,Description 3,2022-06-01,2023-01-15,2

```

```
ID,NAME,TYPE,DESCRIPTION
1,Model A,Type A,Description A
2,Model B,Type B,Description B
3,Model C,Type C,Description C
4,Model D,Type D,Description D

```

## Результат виконання програми

```
Record of type PROJECT:
  ID: 1
  NAME: AI Research
  DESCRIPTION: Description 1
  START-DATE: 2024-01-01
  END-DATE: 2024-12-31
  MODEL-ID: 5

Record of type MODEL:
  ID: 1
  NAME: Model A
  TYPE: Type A
  DESCRIPTION: Description A

Filtered Projects:
Record of type PROJECT:
  ID: 1
  NAME: AI Research
  DESCRIPTION: Description 1
  START-DATE: 2024-01-01
  END-DATE: 2024-12-31
  MODEL-ID: 5

```
