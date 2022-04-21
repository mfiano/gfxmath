(in-package #:gfxmath)

(u:fn-> ref (math-object u:array-index) u:f64)
(declaim (inline ref))
(defun ref (object index)
  "Get a component of a math object at the given column-major index."
  (aref (components object) index))

(u:fn-> (setf ref) (real math-object u:array-index) u:f64)
(declaim (inline (setf ref)))
(defun (setf ref) (value object index)
  "Set a component of a math object at the given column-major index."
  (setf (aref (components object) index) (float value 1d0)))

(u:fn-> mref (math-object u:array-index u:array-index) u:f64)
(declaim (inline mref))
(defun mref (object row column)
  "Get a component of a math object by row and column indices."
  (aref (components object) (cl:+ (cl:* column (row-count object)) row)))

(u:fn-> (setf mref) (real math-object u:array-index u:array-index) u:f64)
(declaim (inline (setf mref)))
(defun (setf mref) (value object row column)
  "Set a component of a math object by row and column indices."
  (setf (aref (components object) (cl:+ (cl:* column (row-count object)) row)) (float value 1d0)))

(declaim (inline %axis->index))
(defun %axis->index (axis)
  (ecase axis
    (:x 0)
    (:y 1)
    (:z 2)))

(declaim (inline %index->axis))
(defun %index->axis (index)
  (ecase index
    (0 :x)
    (1 :y)
    (2 :z)))

(declaim (inline ~=))
(defun ~= (x y &key (rel 1d-7) (abs rel))
  (cl:< (cl:abs (cl:- x y))
        (cl:max abs (cl:* rel (cl:max (cl:abs x) (cl:abs y))))))

(defun %print-object/horizontal (object label stream)
  (let* ((rows (row-count object))
         (columns (column-count object)))
    (format stream "~a: " label)
    (dotimes (row rows)
      (dotimes (column columns)
        (format stream "~,7f" (mref object row column))
        (when (cl:< column (1- columns))
          (format stream " ┃ "))))))

(defun %print-object/columnar (object stream)
  (let* ((rows (row-count object))
         (columns (column-count object))
         (lengths (map 'list
                       (lambda (x) (length (format nil "~,7f" x)))
                       (components object)))
         (max-lengths (loop :for rest :on lengths :by (lambda (x) (nthcdr rows x))
                            :collect (loop :for i :in rest :repeat rows :maximize i))))
    (format stream "Matrix ~dx~d:~%  " rows columns)
    (dotimes (row rows)
      (dotimes (column columns)
        (let* ((value (format nil "~,7f" (mref object row column)))
               (padding (make-string (cl:- (elt max-lengths column) (length value))
                                     :initial-element #\space)))
          (format stream "~a~a" padding value)
          (when (cl:< column (1- columns))
            (format stream " ┃ "))))
      (when (cl:< row (1- rows))
        (format stream "~%  ")))))

(defun %make-random (object min max)
  (let ((diff (cl:- max min)))
    (%with-each (object x i)
      (setf (ref object i) (cl:+ min (random diff))))
    object))
