(in-package #:gfxmath)

;;; Protocol for operations that have different semantics across types

(u:eval-always
  (defgeneric * (object1 object2))
  (defgeneric *! (object1 object2 out))
  (defgeneric / (object1 object2))
  (defgeneric /! (object1 object2 out))
  (defgeneric default (object))
  (defgeneric id (object))
  (defgeneric id! (object))
  (defgeneric id? (object))
  (defgeneric interpolate (object1 object2 parameter))
  (defgeneric interpolate! (object1 object2 parameter out))
  (defgeneric invert (object))
  (defgeneric invert! (object out))
  (defgeneric rotate (object1 object2 &key ((:space space))))
  (defgeneric rotate! (object1 object2 out &key ((:space space)))))

;;; Operations shared between all math objects

(define-op = ((object1 :*) (object2 :*) &key (rel 1d-7) (abs rel)) :all
  "Check if all components of the given {OBJECT1:DESC}s are equal. REL and ABS are the relative ~
and absolute error tolerances used to determine equality."
  (%with-each/parallel (((object1 x) (object2 y)))
    (unless (~= x y :rel rel :abs abs)
      (return-from = nil)))
  t)

(define-op + ((object1 :*) (object2 :*)) :all
  "Perform component-wise addition by adding each component of the {OBJECT1:DESC} OBJECT1 to te ~
corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in a new {OBJECT1:DESC}."
  (+! object1 object2 (default object1)))

(define-op +! ((object1 :*) (object2 :*) (out :*)) :all
  "Perform component-wise addition by adding each component of the {OBJECT1:DESC} OBJECT1 to the ~
corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((object1 x) (object2 y)) :index i)
    (setf (ref out i) (cl:+ x y)))
  out)

(define-op (+ :extend t) ((object1 :*) (object2 real)) :all
  "Perform scalar addition by adding the scalar OBJECT2 to each component of the {OBJECT1:DESC} ~
OBJECT1, storing the result in a new {OBJECT1:DESC}."
  (+! object1 object2 (default object1)))

(define-op (+! :extend t) ((object1 :*) (object2 real) (out :*)) :all
  "Perform scalar addition by adding the scalar OBJECT2 to each component of the {OBJECT1:DESC} ~
OBJECT1, storing the result in the {OUT:DESC} OUT."
  (%with-each (object1 x i)
    (setf (ref out i) (cl:+ x object2)))
  out)

(define-op - ((object1 :*) (object2 :*)) :all
  "Perform component-wise subtraction by subtracting each component of the {OBJECT2:DESC} OBJECT2 ~
from the corresponding component of the {OBJECT1:DESC} OBJECT1, storing the result in a new ~
{OBJECT1:DESC}."
  (-! object1 object2 (default object1)))

(define-op -! ((object1 :*) (object2 :*) (out :*)) :all
  "Perform component-wise subtraction by subtracting each component of the {OBJECT2:DESC} OBJECT2 ~
from the corresponding component of the {OBJECT1:DESC} OBJECT1, storing the result in the
{OUT:DESC} OUT."
  (%with-each/parallel (((object1 x) (object2 y)) :index i)
    (setf (ref out i) (cl:- x y)))
  out)

(define-op (- :extend t) ((object1 :*) (object2 real)) :all
  "Perform scalar subtraction by subtracting the scalar OBJECT2 from each component of the ~
{OBJECT1:DESC} OBJECT1, storing the result in a new {OBJECT1:DESC}."
  (-! object1 object2 (default object1)))

(define-op (-! :extend t) ((object1 :*) (object2 real) (out :*)) :all
  "Perform scalar subtraction by subtracting the scalar OBJECT2 from each component of the ~
{OBJECT1:DESC} OBJECT1, storing the result in the {OUT:DESC} OUT."
  (%with-each (object1 x i)
    (setf (ref out i) (cl:- x object2)))
  out)

(define-op copy ((object :*)) :all
  "Create a fresh copy of the {OBJECT:DESC} OBJECT."
  (let ((out (make-instance (class-name (class-of object)))))
    (setf (components out) (u:copy-array (components object))
          (components/single out) (u:copy-array (components/single object)))
    out))

(define-op copy! ((object :*) (out :*)) :all
  "Copy the components of the {OBJECT:DESC} OBJECT into the {OUT:DESC} OUT."
  (%with-each (object x i)
    (setf (ref out i) x))
  out)

(define-op to-array ((object :*) (element-type (eql :single-float))) :all
  "Construct a freshly-allocated 1-dimensional array of single-floats from the components of the ~
{OBJECT:DESC} OBJECT."
  (u:copy-array (to-array! object element-type)))

(define-op to-array! ((object :*) (element-type (eql :single-float))) :all
  "Get a reference to a 1-dimensional array of single-floats containing the components of the ~
{OBJECT:DESC} OBJECT.

NOTE: This object is not freshly-allocated and should not be stored. Its memory will be ~
overwritten on subsequent calls to either TO-ARRAY or TO-ARRAY!. It is intended to be used for ~
once-off operations, such as uploading of GPU uniform variables."
  (let ((array (components/single object)))
    (%with-each (object x i)
      (setf (aref array i) (float x 1f0)))
    array))

(define-op to-array ((object :*) (element-type (eql :double-float))) :all
  "Construct a freshly-allocated 1-dimensional array of double-floats from the components of the ~
{OBJECT:DESC} OBJECT."
  (u:copy-array (to-array! object element-type)))

(define-op to-array! ((object :*) (element-type (eql :double-float))) :all
  "Get a reference to a 1-dimensional array of double-floats containing the components of the ~
{OBJECT:DESC} OBJECT.

NOTE: This object is not freshly-allocated and should not be stored. Its memory will be ~
overwritten on subsequent calls to either TO-ARRAY or TO-ARRAY!. It is intended to be used for ~
once-off operations, such as uploading of GPU uniform variables."
  (components object))

;;; Vector and matrix operations

(define-op clamp ((object :*) (min :*) (max :*)) (vector matrix)
  "Clamp each component of the {OBJECT:DESC} OBJECT to be within the range bounded by the scalars ~
MIN and MAX, storing the result in a new {OBJECT:DESC}."
  (clamp! object min max (default object)))

(define-op clamp! ((object :*) (min :*) (max :*) (out :*)) (vector matrix)
  "Clamp each component of the {OBJECT:DESC} OBJECT to be within the range bounded by the scalars ~
MIN and MAX, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((object x) (min y) (max z)) :index i)
    (setf (ref out i) (u:clamp x y z)))
  out)

(define-op (clamp :extend t) ((object :*) (min real) (max real)) (vector matrix)
  "Clamp each component of the {OBJECT:DESC} OBJECT to be within the range bounded by the ~
corresponding components of the {OBJECT:DESC}s MIN and MAX, storing the result in a new ~
{OBJECT:DESC}."
  (clamp! object min max (default object)))

(define-op (clamp! :extend t) ((object :*) (min real) (max real) (out :*)) (vector matrix)
  "Clamp each component of the {OBJECT:DESC} OBJECT to be within the range bounded by the ~
corresponding components of the {OBJECT:DESC}s MIN and MAX, storing the result in the {OUT:DESC} ~
OUT."
  (%with-each (object x i)
    (setf (ref out i) (u:clamp x min max)))
  out)

(define-op random! ((object :*) &key (min 0d0) (max 1d0)) (vector matrix)
  "Set each component of the {OBJECT:DESC} OBJECT to a random value bounded by the scalars MIN ~
and MAX, storing the result back into OBJECT."
  (%make-random object min max))

(define-op zero ((object :*)) (vector matrix)
  "Construct a fresh {OBJECT:DESC} with every component set to zero. This is a convenience ~
function that is useful when you already have a reference an object to construct a new type from."
  (zero! (copy object)))

(define-op zero! ((object :*)) (vector matrix)
  "Set each component of the {OBJECT:DESC} OBJECT to zero, storing the result back into OBJECT."
  (fill (components object) 0d0)
  object)

(define-op zero? ((object :*)) (vector matrix)
  "Check if every component of the {OBJECT:DESC} OBJECT is zero."
  (every (lambda (x) (~= x 0d0)) (components object)))

;;; Vector and quaternion operations

(defgeneric x (object))
(defgeneric (setf x) (value object))
(defgeneric y (object))
(defgeneric (setf y) (value object))
(defgeneric z (object))
(defgeneric (setf z) (value object))
(defgeneric w (object))
(defgeneric (setf w) (value object))

(define-op (* :extend t) ((object1 :*) (object2 real)) (vector quaternion)
  "Perform scalar multiplication by multiplying each component of the {OBJECT1:DESC} OBJECT1 by ~
the scalar OBJECT2, storing the result in a new {OBJECT1:DESC}."
  (*! object1 object2 (default object1)))

(define-op (*! :extend t) ((object1 :*) (object2 real) (out :*)) (vector quaternion)
  "Perform scalar multiplication by multiplying each component of the {OBJECT1:DESC} OBJECT1 by ~
the scalar OBJECT2, storing the result in the {OUT:DESC} OUT."
  (%with-each (object1 x i)
    (setf (ref out i) (cl:* x object2)))
  out)

(define-op (/ :extend t) ((object1 :*) (object2 real)) (vector quaternion)
  "Perform scalar division by dividing each component of the {OBJECT1:DESC} OBJECT1 by the ~
scalar OBJECT2, storing the result in a new {OBJECT1:DESC}."
  (/! object1 object2 (default object1)))

(define-op (/! :extend t) ((object1 :*) (object2 real) (out :*)) (vector quaternion)
  "Perform scalar division by dividing each component of the {OBJECT1:DESC} OBJECT1 by the ~
scalar OBJECT2, storing the result in the {OUT:DESC} OUT."
  (%with-each (object1 x i)
    (setf (ref out i) (if (zerop object2) 0d0 (cl:/ x object2))))
  out)

(define-op dot ((object1 :*) (object2 :*)) (vector quaternion)
  "Compute the dot product between the {OBJECT1:DESC} OBJECT1 and the {OBJECT2:DESC} OBJECT2, ~
producing a scalar value."
  (let ((value 0d0))
    (%with-each/parallel (((object1 x) (object2 y)))
      (incf value (cl:* x y)))
    value))

(define-op magnitude ((object :*)) (vector quaternion)
  "Compute the magnitude of the {OBJECT:DESC} OBJECT, producing a scalar value."
  (cl:sqrt (magnitude-squared object)))

(define-op magnitude-squared ((object :*)) (vector quaternion)
  "Compute the squared magnitude of the {OBJECT:DESC} OBJECT, producing a scalar value."
  (dot object object))

(define-op negate ((object :*)) (vector quaternion)
  "Negate each component of the {OBJECT:DESC} OBJECT, storing the result in a new {OBJECT:DESC}."
  (negate! object (default object)))

(define-op negate! ((object :*) (out :*)) (vector quaternion)
  "Negate each component of the {OBJECT:DESC} OBJECT, storing the result in the {OUT:DESC} OUT."
  (%with-each (object x i)
    (setf (ref out i) (cl:- x)))
  out)

(define-op normalize ((object :*)) (vector quaternion)
  "Normalize the {OBJECT:DESC} OBJECT to be of unit length, storing the result in a new ~
{OBJECT:DESC}."
  (normalize! object (default object)))

(define-op normalize! ((object :*) (out :*)) (vector quaternion)
  "Normalize the {OBJECT:DESC} OBJECT to be of unit length, storing the result in the {OUT:DESC} ~
OUT."
  (let ((magnitude (magnitude object)))
    (unless (zerop magnitude)
      (*! object (cl:/ magnitude) out))
    out))
