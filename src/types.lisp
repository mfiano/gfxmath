(in-package #:gfxmath)

(defclass math-object ()
  ((%components
    :type (u:f64a (cl:*))
    :accessor components
    :initarg :components)
   (%components/single
    :type (u:f32a (cl:*))
    :accessor components/single
    :initarg :components/single)
   (%row-count
    :type (integer 1 4)
    :reader row-count
    :initarg :row-count)
   (%column-count
    :type (integer 1 4)
    :reader column-count
    :initarg :column-count))
  (:documentation "The base type that all math types are derived from."))

(defmethod make-load-form ((object math-object) &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; Vector types

(defclass vector (math-object) ()
  (:documentation "A column vector that all vector types are derived from."))

(u:define-printer (vector stream :type nil)
  (%print-object/columnar vector stream))

(%generate-type vector2 ()
                :base vector
                :rows 2
                :description "2-dimensional vector"
                :documentation "A 2-dimensional column vector.")

(%generate-type vector3 ()
                :base vector
                :rows 3
                :description "3-dimensional vector"
                :documentation "A 3-dimensional column vector.")

(%generate-type vector4 ()
                :base vector
                :rows 4
                :description "4-dimensional vector"
                :documentation "A 4-dimensional column vector.")

;;; Matrix types

(defclass matrix (math-object) ()
  (:documentation "A square matrix that all matrix types are derived from."))

(u:define-printer (matrix stream :type nil)
  (%print-object/columnar matrix stream))

(%generate-type matrix2 ()
  :base matrix
  :rows 2
  :columns 2
  :description "2x2 matrix"
  :documentation "A 2x2 matrix representing a 2-dimensional rotation.")

(%generate-type matrix3 ()
  :base matrix
  :rows 3
  :columns 3
  :description "3x3 matrix"
  :documentation "A 3x3 matrix representing either a 2-dimensional transformation or a ~
3-dimensional rotation.")

(%generate-type matrix4 ()
  :base matrix
  :rows 4
  :columns 4
  :description "4x4 matrix"
  :documentation "A 4x4 matrix representing a 3-dimensional transformation.")

;;; Quaternion type

(%generate-type quaternion ()
  :columns 4
  :description "quaternion"
  :documentation "A quaternion representing a 3-dimensional rotation.")

(u:define-printer (quaternion stream :type nil)
  (%print-object/horizontal quaternion "Quaternion" stream))
