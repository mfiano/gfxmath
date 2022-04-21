(in-package #:gfxmath)

;;; Utilities

(defun %mat/from-vecs (matrix &rest vectors)
  (loop :with components := (components matrix)
        :with rows := (row-count matrix)
        :for column :below (column-count matrix)
        :for start := (cl:* column rows)
        :for vector :in vectors
        :do (replace components (components vector) :start1 start)
        :finally (return matrix)))

(defmacro %matrix* (size matrix1 matrix2 out)
  (let ((result nil))
    `(progn
       (psetf
        ,@(dotimes (c size (nreverse result))
            (dotimes (r size)
              (push `(mref ,out ,r ,c) result)
              (push `(cl:+ ,@(loop :for i :below size
                                   :collect `(cl:* (mref ,matrix1 ,r ,i) (mref ,matrix2 ,i ,c))))
                    result))))
       ,out)))

(declaim (inline %get-column))
(defun %get-column (object out index)
  (%with-columns (object i :limit (row-count out))
    (setf (mref out i 0) (mref object i index)))
  out)

(declaim (inline %set-column))
(defun %set-column (object1 object2 out index bound)
  (let ((rows (row-count object1)))
    (unless (cl:< index rows)
      (error "Invalid column index ~d for ~dx~d matrix." index rows rows))
    (let* ((start (cl:* index rows))
           (end (cl:+ start (cl:min rows (row-count object2)))))
      (copy! object1 out)
      (ecase bound
        (1 (replace (components out) (components object2) :start1 start :end1 end))
        (2 (replace (components out) (components object2) :start2 start :end2 end)))
      out)))

(declaim (inline %get-row))
(defun %get-row (object index out)
  (%with-rows (out i)
    (setf (mref out i 0) (mref object index i)))
  out)

(defun %invert/generic-4x4 (matrix out)
  (macrolet ((invert (a1 a2 a3 b1 b2 b3 c1 c2 c3 d1 d2 d3 e1 e2 e3 f1 f2 f3)
               `(cl:/ (cl:- (cl:+ (cl:* ,a1 ,a2 ,a3) (cl:* ,b1 ,b2 ,b3) (cl:* ,c1 ,c2 ,c3))
                            (cl:* ,d1 ,d2 ,d3) (cl:* ,e1 ,e2 ,e3) (cl:* ,f1 ,f2 ,f3))
                      det)))
    (let ((det (determinant matrix)))
      (when (~= det 0d0)
        (warn "Skipped inverting matrix because it has a determinant of zero:~%~s" matrix)
        (return-from %invert/generic-4x4 (copy! matrix out)))
      (with-matrix ((4 m matrix)) ()
        (with-matrix ((4 o out)) (:read-only nil)
          (setf o00 (invert m11 m22 m33 m12 m23 m31 m13 m21 m32 m11 m23 m32 m12 m21 m33 m13 m22 m31)
                o01 (invert m01 m23 m32 m02 m21 m33 m03 m22 m31 m01 m22 m33 m02 m23 m31 m03 m21 m32)
                o02 (invert m01 m12 m33 m02 m13 m31 m03 m11 m32 m01 m13 m32 m02 m11 m33 m03 m12 m31)
                o03 (invert m01 m13 m22 m02 m11 m23 m03 m12 m21 m01 m12 m23 m02 m13 m21 m03 m11 m22)
                o10 (invert m10 m23 m32 m12 m20 m33 m13 m22 m30 m10 m22 m33 m12 m23 m30 m13 m20 m32)
                o11 (invert m00 m22 m33 m02 m23 m30 m03 m20 m32 m00 m23 m32 m02 m20 m33 m03 m22 m30)
                o12 (invert m00 m13 m32 m02 m10 m33 m03 m12 m30 m00 m12 m33 m02 m13 m30 m03 m10 m32)
                o13 (invert m00 m12 m23 m02 m13 m20 m03 m10 m22 m00 m13 m22 m02 m10 m23 m03 m12 m20)
                o20 (invert m10 m21 m33 m11 m23 m30 m13 m20 m31 m10 m23 m31 m11 m20 m33 m13 m21 m30)
                o21 (invert m00 m23 m31 m01 m20 m33 m03 m21 m30 m00 m21 m33 m01 m23 m30 m03 m20 m31)
                o22 (invert m00 m11 m33 m01 m13 m30 m03 m10 m31 m00 m13 m31 m01 m10 m33 m03 m11 m30)
                o23 (invert m00 m13 m21 m01 m10 m23 m03 m11 m20 m00 m11 m23 m01 m13 m20 m03 m10 m21)
                o30 (invert m10 m22 m31 m11 m20 m32 m12 m21 m30 m10 m21 m32 m11 m22 m30 m12 m20 m31)
                o31 (invert m00 m21 m32 m01 m22 m30 m02 m20 m31 m00 m22 m31 m01 m20 m32 m02 m21 m30)
                o32 (invert m00 m12 m31 m01 m10 m32 m02 m11 m30 m00 m11 m32 m01 m12 m30 m02 m10 m31)
                o33 (invert m00 m11 m22 m01 m12 m20 m02 m10 m21 m00 m12 m21 m01 m10 m22 m02 m11 m20))
          out)))))

;;; Constructors

(u:eval-always
  (defun mat (size)
    "Construct a matrix of the given SIZE, with each component set to 0."
    (let ((type (u:format-symbol (symbol-package 'mat) "MATRIX~d" size)))
      (if (find-class type nil)
          (make-instance type)
          (error "Invalid matrix size: ~dx~d." size size)))))

(define-compiler-macro mat (&whole whole size)
  (if (constantp size)
      (let ((type (u:format-symbol (symbol-package 'mat) "MATRIX~d" (eval size))))
        (if (find-class type nil)
            `(make-instance ',type)
            (error "Invalid matrix size: ~dx~d." size size)))
      whole))

(u:eval-always
  (defun mat/id (size)
    "Construct an identity matrix with a dimensionality equal to SIZE."
    (id! (mat size))))

(define-compiler-macro mat/id (&whole whole size)
  (if (constantp size)
      `(id! (mat ,(eval size)))
      whole))

(defun mat/from-vecs (&rest vectors)
  "Construct a matrix from the given VECTORS. The vectors are written into the columns of the ~
resulting matrix. The number of vectors given determines the number of rows and columns of the ~
resulting matrix."
  (let ((matrix (mat/id (length vectors))))
    (apply #'%mat/from-vecs matrix vectors)))

(define-compiler-macro mat/from-vecs (&rest vectors)
  `(%mat/from-vecs (mat/id ,(length vectors)) ,@vectors))

(defun mat/from-mat (size matrix)
  "Construct a matrix of the given SIZE, by copying the components of the given MATRIX of any size ~
into it. If MATRIX has fewer rows or columns than SIZE, any remaining components along that axis ~
are set to zero, unless that component falls along the main diagonal, in which case it is set to ~
1. If MATRIX has more rows or columns than SIZE, any remaining components along that axis are ~
dropped."
  (let ((out (mat/id size)))
    (%with-each/2d (out x r c :row-limit (row-count matrix) :column-limit (column-count matrix))
      (setf (mref out r c) (mref matrix r c)))
    out))

(define-compiler-macro mat/from-mat (&whole whole size matrix)
  (u:with-gensyms (out)
    (if (constantp size)
        `(let ((,out (mat/id ,(eval size))))
           (%with-each/2d (,out x r c :row-limit (row-count ,matrix)
                                      :column-limit (column-count ,matrix))
             (setf (mref ,out r c) (mref matrix r c)))
           ,out)
        whole)))

(defun mat/random (size &key (min 0d0) (max 1d0))
  "Construct a matrix of the given SIZE with each component set to a random value bounded by MIN ~
and MAX."
  (%make-random (mat size) min max))

(defun mat/rotation (size axis angle)
  "Construct a rotation matrix of the given SIZE that represents a rotation of ANGLE radians ~
around the given AXIS. AXIS may be :Z when SIZE is 2, or :X, :Y, or :Z when SIZE is 3 or 4."
  (ecase axis
    (:x (rotation/x! (mat size) angle))
    (:y (rotation/y! (mat size) angle))
    (:z (rotation/z! (mat size) angle))))

(define-compiler-macro mat/rotation (&whole whole size axis angle)
  (if (and (constantp size) (constantp axis))
      (let ((*package* (symbol-package 'mat/rotation)))
        `(,(u:symbolicate '#:rotation/ (eval axis) '#:!) (mat ,(eval size)) ,angle))
      whole))

;;; Operations

(define-op (* :extend t) ((object1 :*) (object2 :*)) (matrix)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in a new ~
{OBJECT1:DESC}."
  (*! object1 object2 (default object1)))

(define-op (*! :extend t) ((object1 :*) (object2 :*) (out :*)) (matrix2)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (%matrix* 2 object1 object2 out))

(define-op (*! :extend t) ((object1 :*) (object2 :*) (out :*)) (matrix3)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (%matrix* 3 object1 object2 out))

(define-op (*! :extend t) ((object1 :*) (object2 :*) (out :*)) (matrix4)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (%matrix* 4 object1 object2 out))

(define-op (* :extend t) ((object1 :*) (object2 :row-sized-vector)) (matrix)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in a ~
new {OBJECT2:DESC}."
  (*! object1 object2 (default object2)))

(define-op (*! :extend t) ((object1 :*) (object2 :row-sized-vector) (out :row-sized-vector))
    (matrix)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (let ((row (default out)))
    (%with-rows (object1 i)
      (get-row! object1 i row)
      (setf (ref out i) (dot row object2)))
    out))

(define-op anti-diagonal ((matrix :*)) (matrix)
  "Compute the anti-diagonal of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:DESC}."
  (anti-diagonal! matrix (vec/zero (row-count matrix))))

(define-op anti-diagonal! ((matrix :*) (out :row-sized-vector)) (matrix)
  "Compute the anti-diagonal of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} ~
OUT."
  (%with-each/2d (matrix x r c)
    (when (cl:= (cl:+ r c) (1- (row-count matrix)))
      (setf (ref out r) (mref matrix c r))))
  out)

(define-op (default :extend t) ((object :*)) (matrix)
  "Construct a default matrix of the same dimensions as OBJECT. Each math type has this method ~
defined, and for matrices this creates an identity matrix."
  (id object))

(define-op determinant ((matrix :*)) (matrix3)
  "Compute the determinant of the {MATRIX:DESC} MATRIX, producing a scalar value."
  (with-matrix ((3 m matrix)) ()
    (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m10 m21 m02) (cl:* m20 m01 m12))
          (cl:* m00 m21 m12)
          (cl:* m20 m11 m02)
          (cl:* m10 m01 m22))))

(define-op (determinant :extend t) ((matrix :*)) (matrix4)
  "Compute the determinant of the {MATRIX:DESC} MATRIX, producing a scalar value."
  (with-matrix ((4 m matrix)) ()
    (cl:- (cl:+ (cl:* m00 m11 m22 m33) (cl:* m00 m12 m23 m31) (cl:* m00 m13 m21 m32)
                (cl:* m01 m10 m23 m32) (cl:* m01 m12 m20 m33) (cl:* m01 m13 m22 m30)
                (cl:* m02 m10 m21 m33) (cl:* m02 m11 m23 m30) (cl:* m02 m13 m20 m31)
                (cl:* m03 m10 m22 m31) (cl:* m03 m11 m20 m32) (cl:* m03 m12 m21 m30))
          (cl:* m00 m11 m23 m32) (cl:* m00 m12 m21 m33) (cl:* m00 m13 m22 m31)
          (cl:* m01 m10 m22 m33) (cl:* m01 m12 m23 m30) (cl:* m01 m13 m20 m32)
          (cl:* m02 m10 m23 m31) (cl:* m02 m11 m20 m33) (cl:* m02 m13 m21 m30)
          (cl:* m03 m10 m21 m32) (cl:* m03 m11 m22 m30) (cl:* m03 m12 m20 m31))))

(define-op diagonal? ((matrix :*)) (matrix)
  "Check whether the {MATRIX:DESC} MATRIX is a diagonal matrix."
  (%with-each/2d (matrix x r c)
    (unless (or (cl:= r c) (~= x 0d0))
      (return-from diagonal? nil)))
  t)

(define-op get-axis ((matrix :*) (axis (eql :x :y))) (matrix)
  "Get the {AXIS:DESC} rotation axis of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:AXIS-DESC}."
  (get-axis! matrix axis (vec/zero (cl:max (1- (row-count matrix)) 2))))

(define-op (get-axis :extend t) ((matrix :*) (axis (eql :z))) (matrix4)
  "Get the {AXIS:DESC} rotation axis of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:AXIS-DESC}."
  (get-axis! matrix axis (vec/zero 3)))

(define-op get-axis! ((matrix :*) (axis (eql :x :y)) (out :axis-sized-vector)) (matrix)
  "Get the {AXIS:DESC} rotation axis of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC} OUT."
  (%get-column matrix out (%axis->index axis)))

(define-op (get-axis! :extend t) ((matrix :*) (axis (eql :z)) (out :axis-sized-vector))
    (matrix3 matrix4)
  "Get the {AXIS:DESC} rotation axis of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC} OUT."
  (%get-column matrix out (%axis->index axis)))

(define-op get-column ((matrix :*) (index (eql 0 1))) (matrix)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:COLUMN-DESC}."
  (get-column! matrix index (vec/zero (row-count matrix))))

(define-op (get-column :extend t) ((matrix :*) (index (eql 2))) (matrix3 matrix4)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:COLUMN-DESC}."
  (get-column! matrix index (vec/zero (row-count matrix))))

(define-op (get-column :extend t) ((matrix :*) (index (eql 3))) (matrix4)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:COLUMN-DESC}."
  (get-column! matrix index (vec/zero 4)))

(define-op get-column! ((matrix :*) (index (eql 0 1)) (out :column-sized-vector)) (matrix)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC} OUT."
  (%get-column matrix out index))

(define-op (get-column! :extend t) ((matrix :*) (index (eql 2)) (out :column-sized-vector))
    (matrix3 matrix4)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC} OUT."
  (%get-column matrix out index))

(define-op (get-column! :extend t) ((matrix :*) (index (eql 3)) (out vector4)) (matrix4)
  "Get the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC} OUT."
  (%get-column matrix out index))

(define-op get-rotation ((matrix :*)) (matrix3 matrix4)
  "Get the rotation sub-matrix of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:DESC}."
  (let ((size (1- (row-count matrix))))
    (get-rotation! matrix (mat size))))

(define-op get-rotation! ((matrix :*) (out :sub-matrix)) (matrix3 matrix4)
  "Get the rotation sub-matrix of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} ~
OUT."
  (let ((limit (1- (row-count matrix))))
    (%with-each/2d (matrix x r c :row-limit limit :column-limit limit)
      (setf (mref out r c) x))
    out))

(define-op get-row ((matrix :*) (index (eql 0 1))) (matrix)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:ROW-DESC}.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (get-row! matrix index (vec/zero (column-count matrix))))

(define-op (get-row :extend t) ((matrix :*) (index (eql 2))) (matrix3 matrix4)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:ROW-DESC}.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (get-row! matrix index (vec/zero (column-count matrix))))

(define-op (get-row :extend t) ((matrix :*) (index (eql 3))) (matrix4)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:ROW-DESC}.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (get-row! matrix index (vec/zero 4)))

(define-op get-row! ((matrix :*) (index (eql 0 1)) (out :row-sized-vector)) (matrix)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (%get-row matrix index out))

(define-op (get-row! :extend t) ((matrix :*) (index (eql 2)) (out :row-sized-vector))
    (matrix3 matrix4)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (%get-row matrix index out))

(define-op (get-row! :extend t) ((matrix :*) (index (eql 3)) (out vector4)) (matrix4)
  "Get the {INDEX:ORD} row of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT.

Note: The resulting vector does not change its shape; vectors are always column vectors."
  (%get-row matrix index out))

(define-op get-scale ((matrix :*)) (matrix2 matrix4)
  "Get the scale of the {MATRIX:DESC} MATRIX, storing the result in a new {MATRIX:DESC}."
  (get-scale! matrix (vec/zero (cl:max (1- (row-count matrix)) 2))))

(define-op get-scale! ((matrix :*) (out :axis-sized-vector)) (matrix2 matrix4)
  "Get the scale of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT."
  (%with-rows (matrix i :limit (row-count out))
    (setf (ref out i) (magnitude (get-axis matrix (%index->axis i)))))
  out)

(define-op get-translation ((matrix :*)) (matrix3 matrix4)
  "Get the translation of the {MATRIX:DESC} MATRIX, storing the result in a new {MATRIX:DESC}."
  (get-translation! matrix (vec/zero (1- (row-count matrix)))))

(define-op get-translation! ((matrix :*) (out :axis-sized-vector)) (matrix3 matrix4)
  "Get the translation of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT."
  (%with-each (out x i)
    (setf (ref out i) (mref matrix i (row-count out))))
  out)

(define-op (id :extend t) ((object :*)) (matrix)
  "Construct a new identity matrix of the same dimensions as OBJECT."
  (id! (copy object)))

(define-op (id! :extend t) ((object :*)) (matrix)
  "Modify the {OBJECT:DESC} OBJECT to be an identity matrix."
  (%with-each/2d (object x r c)
    (setf (mref object r c) (if (cl:= r c) 1d0 0d0)))
  object)

(define-op (id? :extend t) ((object :*)) (matrix)
  "Check if the {OBJECT:DESC} OBJECT is an identity matrix."
  (%with-each/2d (object x r c)
    (unless (~= x (if (cl:= r c) 1d0 0d0))
      (return-from id? nil)))
  t)

(define-op (invert :extend t) ((object :*)) (matrix3 matrix4)
  "Invert the {OBJECT:DESC} OBJECT, storing the result in a new {MATRIX:DESC}."
  (invert! object (default object)))

(define-op (invert! :extend t) ((object :*) (out :*)) (matrix3)
  "Invert the {OBJECT:DESC} OBJECT, storing the result in the {OUT:DESC} OUT."
  (macrolet ((invert (a b c d)
               `(cl:/ (cl:- (cl:* ,a ,b) (cl:* ,c ,d)) det)))
    (with-matrix ((3 m object)) ()
      (with-matrix ((3 o out)) (:read-only nil)
        (zero! out)
        (when (id? object)
          (return-from invert! (id! out)))
        (let ((det (determinant object)))
          (when (~= det 0d0)
            (warn "Skipped inverting matrix because it has a determinant of zero:~%~s" object)
            (return-from invert! (copy! object out)))
          (setf o00 (invert m11 m22 m12 m21)
                o01 (invert m02 m21 m01 m22)
                o02 (invert m01 m12 m02 m11)
                o10 (invert m12 m20 m10 m22)
                o11 (invert m00 m22 m02 m20)
                o12 (invert m02 m10 m00 m12)
                o20 (invert m10 m21 m11 m20)
                o21 (invert m01 m20 m00 m21)
                o22 (invert m00 m11 m01 m10))
          out)))))

(define-op (invert! :extend t) ((object :*) (out :*)) (matrix4)
  "Invert the {OBJECT:DESC} OBJECT, storing the result in the {OUT:DESC} OUT."
  (macrolet ((invert (a b c d)
               `(cl:/ (cl:- (cl:* ,a ,b) (cl:* ,c ,d)) det)))
    (flet ((sub-determinant (matrix)
             (with-matrix ((3 m matrix)) ()
               (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m10 m21 m02) (cl:* m20 m01 m12))
                     (cl:* m00 m21 m12)
                     (cl:* m20 m11 m02)
                     (cl:* m10 m01 m22)))))
      (with-matrix ((4 m object)) ()
        (with-matrix ((4 o out)) (:read-only nil)
          (zero! out)
          (when (id? object)
            (return-from invert! (id! out)))
          (unless (and (~= m30 0d0) (~= m31 0d0) (~= m32 0d0) (~= m33 1d0))
            (return-from invert! (%invert/generic-4x4 object out)))
          (let ((det (sub-determinant object)))
            (if (~= det 0d0)
                (%with-each/2d (object x r c :row-limit 3 :column-limit 3)
                  (unless (~= x 0d0)
                    (return-from invert! (%invert/generic-4x4 object out))))
                (setf o00 (invert m11 m22 m12 m21)
                      o01 (invert m02 m21 m01 m22)
                      o02 (invert m01 m12 m02 m11)
                      o10 (invert m12 m20 m10 m22)
                      o11 (invert m00 m22 m02 m20)
                      o12 (invert m02 m10 m00 m12)
                      o20 (invert m10 m21 m11 m20)
                      o21 (invert m01 m20 m00 m21)
                      o22 (invert m00 m11 m01 m10)))
            (%with-rows (out r :limit 3)
              (setf (mref out r 3) (cl:- (cl:+ (cl:* m03 (mref out r 0))
                                               (cl:* m13 (mref out r 1))
                                               (cl:* m23 (mref out r 2))))))
            (setf o33 1d0)
            out))))))

(define-op look-at ((eye vector3) (target vector3) (up vector3)) (matrix4)
  "Construct a view matrix where the camera is located in 3-dimensional space at EYE, and rotated ~
to look at the target point TARGET, with the camera's up direction given as UP, storing the result ~
in a new {MATRIX:DESC}."
  (look-at! eye target up (mat 4)))

(define-op look-at! ((eye vector3) (target vector3) (up vector3) (out :*)) (matrix4)
  "Modify the {OUT:DESC} OUT to be a view matrix where the camera is located in 3-dimensional ~
space at EYE, and rotated to look at the target point TARGET, with the camera's up direction given ~
as UP."
  (let* ((z (- target eye))
         (x (cross z up))
         (y (cross x z)))
    (normalize! x x)
    (normalize! y y)
    (normalize! z z)
    (with-matrix ((4 o out)) (:read-only nil)
      (with-vector ((3 x x) (3 y y) (3 z z)) ()
        (setf o00 xx o01 xy o02 xz o03 (cl:- (dot x eye))
              o10 yx o11 yy o12 yz o13 (cl:- (dot y eye))
              o20 (cl:- zx) o21 (cl:- zy) o22 (cl:- zz) o23 (dot z eye)
              o30 0d0 o31 0d0 o32 0d0 o33 1d0)))
    out))

(define-op main-diagonal ((matrix :*)) (matrix)
  "Compute the main diagonal of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:DESC}."
  (main-diagonal! matrix (vec/zero (row-count matrix))))

(define-op main-diagonal! ((matrix :*) (out :row-sized-vector)) (matrix)
  "Compute the main diagonal of the {MATRIX:DESC} MATRIX, storing the result in the {OUT:DESC} OUT."
  (%with-each/2d (matrix x r c)
    (when (cl:= r c)
      (setf (ref out r) x)))
  out)

(define-op normalize-rotation ((matrix :*)) (matrix)
  "Normalize each of the rotation axes of the {MATRIX:DESC} MATRIX to be of unit length, storing ~
the result in a new {MATRIX:DESC}."
  (normalize-rotation! matrix (default matrix)))

(define-op normalize-rotation! ((matrix :*) (out :*)) (matrix)
  "Normalize each of the rotation axes of the {MATRIX:DESC} MATRIX to be of unit length, storing ~
the result in the {OUT:DESC} OUT."
  (let* ((axis-size (cl:max (1- (row-count matrix)) 2))
         (vector (vec/zero axis-size)))
    (copy! matrix out)
    (%with-columns (matrix i :limit axis-size)
      (let ((axis (%index->axis i)))
        (get-axis! matrix axis vector)
        (normalize! vector vector)
        (set-axis! out vector axis out)))
    out))

(define-op ortho ((left real) (right real) (bottom real) (top real) (near real) (far real))
    (matrix4)
  "Construct an orthographic projection matrix, storing the result in a new 4x4 matrix."
  (ortho! left right bottom top near far (mat 4)))

(define-op ortho! ((left real) (right real) (bottom real) (top real) (near real) (far real)
                   (out :*))
    (matrix4)
  "Modify the {OUT:DESC} matrix to be an orthographic projection matrix."
  (let ((rl (cl:- right left))
        (tb (cl:- top bottom))
        (fn (cl:- far near)))
    (id! out)
    (setf (mref out 0 0) (cl:/ 2d0 rl)
          (mref out 0 3) (cl:- (cl:/ (cl:+ right left) rl))
          (mref out 1 1) (cl:/ 2d0 tb)
          (mref out 1 3) (cl:- (cl:/ (cl:+ top bottom) tb))
          (mref out 2 2) (cl:/ -2d0 fn)
          (mref out 2 3) (cl:- (cl:/ (cl:+ far near) fn)))
    out))

(define-op orthogonal? ((matrix :*)) (matrix)
  "Check whether the {MATRIX:DESC} MATRIX is orthogonal."
  (id? (* matrix (transpose matrix))))

(define-op orthonormalize ((matrix :*)) (matrix3 matrix4)
  "Normalize the rotation sub-matrix of the {MATRIX:DESC} MATRIX using the Gram-Schmidt process, ~
storing the result in a new {MATRIX:DESC}."
  (orthonormalize! matrix (default matrix)))

(define-op orthonormalize! ((matrix :*) (out :*)) (matrix3 matrix4)
  "Normalize the rotation sub-matrix of the {MATRIX:DESC} MATRIX using the Gram-Schmidt process, ~
storing the result in the {OUT:DESC} OUT."
  (let ((x (get-axis matrix :x))
        (y (get-axis matrix :y))
        (z (get-axis matrix :z)))
    (copy! matrix out)
    (normalize! x x)
    (normalize! (- y (* x (dot y x))) y)
    (cross! x y z)
    (set-axis! out x :x out)
    (set-axis! out y :y out)
    (set-axis! out z :z out)
    out))

(define-op perspective ((fov real) (aspect real) (near real) (far real)) (matrix4)
  "Construct a perspective projection matrix, storing the result in a new 4x4 matrix."
  (perspective! fov aspect near far (mat 4)))

(define-op perspective! ((fov real) (aspect real) (near real) (far real) (out :*)) (matrix4)
  "Modify the {OUT:DESC} OUT to be a perspective projection matrix."
  (let ((f (cl:/ (cl:tan (cl:/ fov 2))))
        (z (cl:- near far)))
    (zero! out)
    (setf (mref out 0 0) (cl:* f (cl:/ aspect))
          (mref out 1 1) f
          (mref out 2 2) (cl:/ (cl:+ near far) z)
          (mref out 2 3) (cl:/ (cl:* 2d0 near far) z)
          (mref out 3 2) -1d0)
    out))

(define-op (rotate :extend t) ((object1 :*) (object2 real) &key (space :local)) (matrix2 matrix3)
  "Perform a matrix rotation by multiplying the {OBJECT1:DESC} OBJECT by a rotation matrix that ~
denotes a rotation around the Z axis by an angle in radians of OBJECT2, storing the result in a ~
new {OBJECT1:DESC}. If SPACE is :WORLD instead of the default of :LOCAL, the rotation matrix is ~
multiplying by OBJECT2."
  (rotate! object1 object2 (default object1) :space space))

(define-op (rotate :extend t) ((object1 :*) (object2 vector3) &key (space :local)) (matrix4)
  "Perform a matrix rotation by multiplying the {OBJECT1:DESC} OBJECT by a a series of rotation ~
matrices around each of the axes and angles in radians denoted by the {OBJECT2:DESC} OBJECT2, ~
storing the result in a new {OBJECT1:DESC}. If SPACE is :WORLD instead of the default of :LOCAL, ~
the rotation matrices are multiplying by OBJECT1."
  (rotate! object1 object2 (default object1) :space space))

(define-op (rotate! :extend t) ((object1 :*) (object2 real) (out :*) &key (space :local))
    (matrix2 matrix3)
  "Perform a matrix rotation by multiplying the {OBJECT1:DESC} OBJECT by a rotation matrix that ~
denotes a rotation around the Z axis by an angle in radians of OBJECT2, storing the result in the ~
{OUT:DESC} OUT. If SPACE is :WORLD instead of the default of :LOCAL, the rotation matrix is ~
multiplying by OBJECT2."
  (let ((z (zero object1)))
    (copy! object1 out)
    (rotation/z! z object2)
    (ecase space
      (:local (*! out z out))
      (:world (*! z out out)))
    out))

(define-op (rotate! :extend t) ((object1 :*) (object2 vector3) (out :*) &key (space :local))
    (matrix4)
  "Perform a matrix rotation by multiplying the {OBJECT1:DESC} OBJECT by a a series of rotation ~
matrices around each of the axes and angles in radians denoted by the {OBJECT2:DESC} OBJECT2, ~
storing the result in the {OUT:DESC} OUT. If SPACE is :WORLD instead of the default of :LOCAL, the ~
rotation matrices are multiplying by OBJECT1."
  (macrolet ((%rotate (space)
               (u:with-gensyms (r angle)
                 `(let ((,r (mat/id 4)))
                    ,@(loop :for axis :in '(x z y)
                            :for axis-keyword := (u:make-keyword axis)
                            :collect `(let ((,angle (,axis object2)))
                                        (unless (~= (,axis object2) 0d0)
                                          (,(u:symbolicate '#:rotation/ axis '#:!) ,r ,angle)
                                          (*! ,@(if (eq space :world)
                                                    `(,r object1)
                                                    `(object1 ,r))
                                              out))))))))
    (copy! object1 out)
    (ecase space
      (:local
       (%rotate :local))
      (:world
       (%rotate :world))))
  out)


(define-op rotation/x! ((out :*) (angle real)) (matrix3 matrix4)
  "Modify the {OUT:DESC} OUT to represent a rotation around the X axis by ANGLE radians."
  (let ((s (cl:sin angle))
        (c (cl:cos angle)))
    (id! out)
    (with-matrix ((4 m out)) (:read-only nil)
      (setf m11 c m12 (cl:- s) m21 s m22 c))
    out))

(define-op rotation/y! ((matrix :*) (angle real)) (matrix3 matrix4)
  "Modify the {OUT:DESC} OUT to represent a rotation around the Y axis by ANGLE radians."
  (let ((s (cl:sin angle))
        (c (cl:cos angle)))
    (id! matrix)
    (with-matrix ((4 m matrix)) (:read-only nil)
      (setf m00 c m02 s m20 (cl:- s) m22 c))
    matrix))

(define-op rotation/z! ((matrix :*) (angle real)) (matrix)
  "Modify the {OUT:DESC} OUT to represent a rotation around the Z axis by ANGLE radians."
  (let ((s (cl:sin angle))
        (c (cl:cos angle)))
    (id! matrix)
    (with-matrix ((4 m matrix)) (:read-only nil)
      (setf m00 c m01 (cl:- s) m10 s m11 c))
    matrix))

(define-op scale ((matrix :*) (vector :axis-sized-vector)) (matrix)
  "Scale the {MATRIX:DESC} MATRIX, by the {VECTOR:DESC} VECTOR, storing the result in a new ~
{MATRIX:DESC}."
  (scale! matrix vector (default matrix)))

(define-op scale! ((matrix :*) (vector :axis-sized-vector) (out :*)) (matrix)
  "Scale the {MATRIX:DESC} MATRIX, by the {VECTOR:DESC} VECTOR, storing the result in the ~
{OUT:DESC} OUT."
  (copy! matrix out)
  (%with-each/2d (matrix x r c :row-limit (cl:max (1- (row-count matrix)) 2))
    (setf (mref out r c) (cl:* x (ref vector r))))
  out)

(define-op set-axis ((matrix :*) (vector :axis-sized-vector) (axis (eql :x :y))) (matrix2 matrix3)
  "Set the {AXIS:DESC} axis of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{VECTOR:DESC} VECTOR into the corresponding locations of the matrix, storing the result in a new ~
{MATRIX:DESC}."
  (set-axis! matrix vector axis (default matrix)))

(define-op (set-axis :extend t) ((matrix :*) (vector :axis-sized-vector) (axis (eql :x :y :z)))
    (matrix4)
  "Set the {AXIS:DESC} axis of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{VECTOR:DESC} VECTOR into the corresponding locations of the matrix, storing the result in a new ~
{MATRIX:DESC}."
  (set-axis! matrix vector axis (default matrix)))

(define-op (set-axis! :extend t) ((matrix :*) (vector vector2) (axis (eql :x :y)) (out :*))
    (matrix2 matrix3)
  "Set the {AXIS:DESC} axis of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{VECTOR:DESC} VECTOR into the corresponding locations of the matrix, storing the result in the ~
{OUT:DESC} OUT."
  (%set-column matrix vector out (%axis->index axis) 1))

(define-op (set-axis! :extend t) ((matrix :*) (vector vector3) (axis (eql :x :y :z)) (out :*))
    (matrix4)
  "Set the {AXIS:DESC} axis of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{VECTOR:DESC} VECTOR into the corresponding locations of the matrix, storing the result in the ~
{OUT:DESC} OUT."
  (%set-column matrix vector out (%axis->index axis) 1))

(define-op set-column ((matrix :*) (column :column-sized-vector) (index (eql 0 1))) (matrix)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in a new ~
{MATRIX:DESC}."
  (set-column! matrix column index (default matrix)))

(define-op (set-column :extend t) ((matrix :*) (column :column-sized-vector) (index (eql 2)))
    (matrix3 matrix4)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in a new ~
{MATRIX:DESC}."
  (set-column! matrix column index (default matrix)))

(define-op (set-column :extend t) ((matrix :*) (column vector4) (index (eql 3))) (matrix4)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in a new ~
{MATRIX:DESC}."
  (set-column! matrix column index (mat/id 4)))

(define-op set-column! ((matrix :*) (column :column-sized-vector) (index (eql 0 1)) (out :*))
    (matrix)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in the ~
{OUT:DESC} OUT."
  (%set-column matrix column out index 1))

(define-op set-column! ((matrix :*) (column :column-sized-vector) (index (eql 2)) (out :*))
    (matrix3 matrix4)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in the ~
{OUT:DESC} OUT."
  (%set-column matrix column out index 1))

(define-op set-column! ((matrix :*) (column vector4) (index (eql 3)) (out :*)) (matrix4)
  "Set the {INDEX:ORD} column of the {MATRIX:DESC} MATRIX, by copying the components of the ~
{COLUMN:DESC} COLUMN into the corresponding locations of the matrix, storing the result in the ~
{OUT:DESC} OUT."
  (%set-column matrix column out index 1))

(define-op set-scale ((matrix :*) (vector :axis-sized-vector)) (matrix)
  "Set the scale of the {MATRIX:DESC} MATRIX, by copying the components of the {VECTOR:DESC} ~
VECTOR into the corresponding locations of the matrix, storing the result in a new {MATRIX:DESC}."
  (set-scale! matrix vector (default matrix)))

(define-op set-scale! ((matrix :*) (vector :axis-sized-vector) (out :*)) (matrix)
  "Set the scale of the {MATRIX:DESC} MATRIX, by copying the components of the {VECTOR:DESC} ~
VECTOR into the corresponding locations of the matrix, storing the result in the {OUT:DESC} OUT."
  (copy! matrix out)
  (%with-each (vector x i)
    (setf (mref out i i) x))
  out)

(define-op set-diagonal ((matrix :*) (vector :row-sized-vector)) (matrix)
  "Set the diagonal of the {MATRIX:DESC} MATRIX, by copying the components of the {VECTOR:DESC} ~
VECTOR into the corresponding locations of the matrix, storing the result in a new {MATRIX:DESC}."
  (set-diagonal! matrix vector (default matrix)))

(define-op set-diagonal! ((matrix :*) (vector :row-sized-vector) (out :*)) (matrix)
  "Set the diagonal of the {MATRIX:DESC} MATRIX, by copying the components of the {VECTOR:DESC} ~
VECTOR into the corresponding locations of the matrix, storing the result in the {OUT:DESC} OUT."
  (copy! matrix out)
  (%with-each (vector x i)
    (setf (mref out i i) x))
  out)

(define-op set-translation ((matrix :*) (vector :axis-sized-vector)) (matrix3 matrix4)
  "Set the translation of the {MATRIX:DESC} MATRIX from the {VECTOR:DESC} VECTOR, storing the ~
result in a new {MATRIX:DESC}."
  (set-translation! matrix vector (default matrix)))

(define-op set-translation! ((matrix :*) (vector :axis-sized-vector) (out :*)) (matrix3 matrix4)
  "Set the translation of the {MATRIX:DESC} MATRIX from the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (let ((column-index (row-count vector)))
    (copy! matrix out)
    (%with-each (vector x i)
      (setf (mref out i column-index) x))
    out))

(define-op trace ((matrix :*)) (matrix)
  "Compute the trace of the {MATRIX:DESC}, producing a scalar value."
  (let ((result 0))
    (%with-each/2d (matrix x r c)
      (when (cl:= r c)
        (incf result x)))
    result))

(define-op translate ((matrix :*) (vector :axis-sized-vector)) (matrix3 matrix4)
  "Translate the {MATRIX:DESC} MATRIX by the {VECTOR:DESC} VECTOR, storing the result in a new ~
{MATRIX:DESC}."
  (translate! matrix vector (default matrix)))

(define-op (translate! :extend t) ((matrix :*) (vector :axis-sized-vector) (out :*))
    (matrix3 matrix4)
  "Translate the {MATRIX:DESC} MATRIX by the {VECTOR:DESC} VECTOR, storing the result in the ~
{OUT:DESC} OUT."
  (copy! matrix out)
  (%with-rows (matrix r :row-count rows)
    (let ((acc 0))
      (dotimes (i (row-count vector))
        (incf acc (cl:* (mref matrix r i) (ref vector i))))
      (setf (mref out r (1- rows)) (incf acc (mref matrix r (1- rows))))))
  out)

(define-op transpose ((matrix :*)) (matrix)
  "Transpose the rows and columns of the {MATRIX:DESC} MATRIX, storing the result in a new ~
{MATRIX:DESC}."
  (transpose! matrix (default matrix)))

(define-op transpose! ((matrix :*) (out :*)) (matrix)
  "Transpose the rows and columns of the {MATRIX:DESC} MATRIX, storing the result in the ~
{OUT:DESC}."
  (copy! matrix out)
  (%with-rows (matrix r)
    (loop :for c :from (1+ r) :below (column-count matrix)
          :do (rotatef (mref out r c) (mref out c r))))
  out)

;;; Constants

(u:define-constant +m2-zero+ (mat 2)
  :test #'=
  :documentation "A 2x2 matrix with each component set to 0.")

(u:define-constant +m2-id+ (mat/id 2)
  :test #'=
  :documentation "A 2x2 identity matrix.")

(u:define-constant +m3-zero+ (mat 3)
  :test #'=
  :documentation "A 3x3 matrix with each component set to 0.")

(u:define-constant +m3-id+ (mat/id 3)
  :test #'=
  :documentation "A 3x3 identity matrix.")

(u:define-constant +m4-zero+ (mat 4)
  :test #'=
  :documentation "A 4x4 matrix with each component set to 0.")

(u:define-constant +m4-id+ (mat/id 4)
  :test #'=
  :documentation "A 4x4 identity matrix.")
