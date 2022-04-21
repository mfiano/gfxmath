(in-package #:gfxmath)

;;; Accessors

(%generate-accessors vector2 x y)
(%generate-accessors vector3 x y z)
(%generate-accessors vector4 x y z w)

;;; Constructors

(u:eval-always
  (defun vec (&rest components)
    (doc "Construct a vector with a dimensionality equal to the number of reals given, and its ~
components set to the values of the arguments.")
    (let* ((size (length components))
           (type (u:format-symbol (symbol-package 'vec) "VECTOR~d" size)))
      (if (find-class type nil)
          (let ((vector (make-instance type)))
            (loop :for component :in components
                  :for i :from 0
                  :do (setf (ref vector i) component))
            vector)
          (error "Invalid vector size: ~d." size)))))

(define-compiler-macro vec (&rest components)
  (u:with-gensyms (vector)
    (let* ((size (length components))
           (type (u:format-symbol (symbol-package 'vec) "VECTOR~d" size)))
      (if (find-class type nil)
          `(let ((,vector (make-instance ',type)))
             (setf
              ,@(loop :for component :in components
                      :for i :from 0
                      :collect `(ref ,vector ,i)
                      :collect component))
             ,vector)
          (error "Invalid vector size: ~d." size)))))

(u:eval-always
  (defun vec/zero (size)
    "Construct a zero vector of the given size."
    (apply #'vec (make-list size :initial-element 0d0))))

(define-compiler-macro vec/zero (&whole whole size)
  (if (constantp size)
      `(vec ,@(make-list (eval size) :initial-element 0d0))
      whole))

(defun vec/from-vec (size vector)
  (doc "Construct a vector of the given SIZE, by copying the components of the given VECTOR of any ~
 size into it. If VECTOR has fewer components than SIZE, any remaining components are set to zero. ~
If VECTOR has more components than SIZE, any remaining components of VECTOR are dropped.")
  (let ((out (vec/zero size)))
    (replace (components out) (components vector))
    out))

(defun vec/random (size &key (min 0d0) (max 1d0))
  (doc "Construct a vector of the given SIZE with each component set to a random value bounded by ~
MIN and MAX.")
  (%make-random (vec/zero size) min max))

(defun vec/velocity (axis rate)
  (doc "Constructs a vector designating a velocity following the right-hand rule, with a direction ~
parallel to AXIS, and a magnitude of RATE units per second.")
  (velocity! axis rate (default axis)))

;;; Operations

(define-op (* :extend t) ((object1 :*) (object2 :*)) (vector)
  "Perform component-wise multiplication by multiplying each component of the {OBJECT1:DESC} ~
OBJECT1 by the corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in a new ~
{OBJECT1:DESC}."
  (*! object1 object2 (default object1)))

(define-op (*! :extend t) ((object1 :*) (object2 :*) (out :*)) (vector)
  "Perform component-wise multiplication by multiplying each component of the {OBJECT1:DESC} ~
OBJECT1 by the corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (%with-each/parallel (((object1 x) (object2 y)) :index i)
    (setf (ref out i) (cl:* x y)))
  out)

(define-op (/ :extend t) ((object1 :*) (object2 :*)) (vector)
  "Perform component-wise division by dividing each component of the {OBJECT1:DESC} OBJECT1 by the ~
corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in a new {OBJECT1:DESC}."
  (/! object1 object2 (default object1)))

(define-op (/! :extend t) ((object1 :*) (object2 :*) (out :*)) (vector)
  "Perform component-wise division by dividing each component of the {OBJECT1:DESC} OBJECT1 by the ~
corresponding component of the {OBJECT2:DESC} OBJECT2, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((object1 x) (object2 y)) :index i)
    (setf (ref out i) (if (zerop y) 0d0 (cl:/ x y))))
  out)

(define-op < ((vector1 :*) (vector2 :*)) (vector)
  "Check if each component of the {VECTOR1:DESC} VECTOR1 is less than the corresponding component ~
of the {VECTOR2:DESC} VECTOR2."
  (%with-each/parallel (((vector1 x) (vector2 y)))
    (unless (and (not (~= x y)) (minusp (cl:- x y)))
      (return-from < nil)))
  t)

(define-op <= ((vector1 :*) (vector2 :*)) (vector)
  "Check if each component of the {VECTOR1:DESC} VECTOR1 is less than or equal to the ~
corresponding component of the {VECTOR2:DESC} VECTOR2."
  (%with-each/parallel (((vector1 x) (vector2 y)))
    (unless (or (~= x y) (minusp (cl:- x y)))
      (return-from <= nil)))
  t)

(define-op > ((vector1 :*) (vector2 :*)) (vector)
  "Check if each component of the {VECTOR1:DESC} VECTOR1 is greater than the corresponding ~
component of the {VECTOR2:DESC} VECTOR2."
  (%with-each/parallel (((vector1 x) (vector2 y)))
    (unless (and (not (~= x y)) (plusp (cl:- x y)))
      (return-from > nil)))
  t)

(define-op >= ((vector1 :*) (vector2 :*)) (vector)
  "Check if each component of the {VECTOR1:DESC} VECTOR1 is greater than or equal to the ~
corresponding component of the {VECTOR2:DESC} VECTOR2."
  (%with-each/parallel (((vector1 x) (vector2 y)))
    (unless (or (~= x y) (plusp (cl:- x y)))
      (return-from >= nil)))
  t)

(define-op abs ((vector :*)) (vector)
  "Compute the absolute value of each component of the {VECTOR:DESC} VECTOR, storing the result in ~
a new {VECTOR:DESC}."
  (abs! vector (default vector)))

(define-op abs! ((vector :*) (out :*)) (vector)
  "Compute the absolute value of each component of the {VECTOR:DESC} VECTOR, storing the result in ~
a the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:abs x)))
  out)

(define-op acos ((vector :*)) (vector)
  "Compute the trigonometric arccosine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (acos! vector (default vector)))

(define-op acos! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric arccosine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:acos (u:clamp x -1d0 1d0))))
  out)

(define-op angle ((vector1 :*) (vector2 :*)) (vector)
  "Compute the angle in radians between the {VECTOR1:DESC}s VECTOR1 and VECTOR2. "
  (if (= vector1 vector2)
      0d0
      (let ((m*m (cl:* (magnitude vector1) (magnitude vector2))))
        (if (~= m*m 0d0)
            0d0
            (cl:acos (cl:/ (dot vector1 vector2) m*m))))))

(define-op asin ((vector :*)) (vector)
  "Compute the trigonometric arcsine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (asin! vector (default vector)))

(define-op asin! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric arcsine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:asin (u:clamp x -1d0 1d0))))
  out)

(define-op atan ((vector :*)) (vector)
  "Compute the trigonometric arctangent of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (atan! vector (default vector)))

(define-op atan! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric arctangent of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:atan x)))
  out)

(define-op ceiling ((vector :*)) (vector)
  "Take the ceiling of each component of the {VECTOR:DESC} VECTOR, storing the result in a new ~
{VECTOR:DESC}."
  (ceiling! vector (default vector)))

(define-op ceiling! ((vector :*) (out :*)) (vector)
  "Take the ceiling of each component of the {VECTOR:DESC} VECTOR, storing the result in the ~
{OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (fceiling x)))
  out)

(define-op cos ((vector :*)) (vector)
  "Compute the trigonometric cosine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (cos! vector (default vector)))

(define-op cos! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric cosine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:cos x)))
  out)

(define-op cross ((vector1 :*) (vector2 :*)) (vector3)
  "Compute the cross product of the {VECTOR:DESC}s VECTOR1 and VECTOR2, storing the result in a ~
new {VECTOR1:DESC}."
  (cross! vector1 vector2 (vec/zero 3)))

(define-op cross! ((vector1 :*) (vector2 :*) (dest :*)) (vector3)
  "Compute the cross product of the {VECTOR:DESC}s VECTOR1 and VECTOR2, storing the result in the ~
{OUT:DESC} OUT."
  (with-vector ((3 v1 vector1) (3 v2 vector2)) ()
    (setf (x dest) (cl:- (cl:* v1y v2z) (cl:* v1z v2y))
          (y dest) (cl:- (cl:* v1z v2x) (cl:* v1x v2z))
          (z dest) (cl:- (cl:* v1x v2y) (cl:* v1y v2x))))
  dest)

(define-op (default :extend t) ((object :*)) (vector)
  "Construct a default vector of the same dimensions as OBJECT. Each math type has this method ~
defined, and for vectors this creates a zero vector."
  (zero! (copy object)))

(define-op degrees->radians ((vector :*)) (vector)
  "Convert each component of the {VECTOR:DESC} VECTOR, which are assumed to be in degrees, to ~
radians, storing the result in a new {VECTOR:DESC}."
  (degrees->radians! vector (default vector)))

(define-op degrees->radians! ((vector :*) (out :*)) (vector)
  "Convert each component of the {VECTOR:DESC} VECTOR, which are assumed to be in degrees, to ~
radians, storing the result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:* x (cl:/ pi 180))))
  out)

(define-op expt ((vector :*) (power real)) (vector)
  "Raise each component of the {VECTOR:DESC} VECTOR to the power of POWER, storing the result in a ~
new {VECTOR:DESC}."
  (expt! vector power (default vector)))

(define-op expt! ((vector :*) (power real) (out :*)) (vector)
  "Raise each component of the {VECTOR:DESC} VECTOR to the power of POWER, storing the result in ~
the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:expt x power)))
  out)

(define-op floor ((vector :*)) (vector)
  "Take the floor of each component of the {VECTOR:DESC} VECTOR, storing the result in a new ~
{VECTOR:DESC}."
  (floor! vector (default vector)))

(define-op floor! ((vector :*) (out :*)) (vector)
  "Take the floor of each component of the {VECTOR:DESC} VECTOR, storing the result in the ~
{OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (ffloor x)))
  out)

(define-op fract ((vector :*)) (vector)
  "Extract the fractional component of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}.

Note: This is computed as n - floor(n)."
  (fract! vector (default vector)))

(define-op fract! ((vector :*) (out :*)) (vector)
  "Extract the fractional component of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT.

Note: This is computed as n - floor(n)."
  (%with-each (vector x i)
    (setf (ref out i) (cl:- x (ffloor x))))
  out)

(define-op (interpolate :extend t) ((object1 :*) (object2 :*) (parameter real)) (vector)
  "Perform a suitable interpolation between the {OBJECT1:DESC}s OBJECT1 and OBJECT2, storing the ~
result in a new {OBJECT1:DESC}. This is the same as LERP for vectors and SLERP for quaternions. ~
It exists to conveniently dispatch to the appropriate interpolation method given the types of the ~
objects."
  (lerp object1 object2 parameter))

(define-op (interpolate! :extend t) ((object1 :*) (object2 :*) (parameter real) (out :*)) (vector)
  "Perform a suitable interpolation between the {OBJECT1:DESC}s OBJECT1 and OBJECT2, storing the ~
result in the {OUT:DESC} OUT. This is the same as LERP! for vectors and SLERP! for quaternions. ~
It exists to conveniently dispatch to the appropriate interpolation method given the types of the ~
objects."
  (lerp! object1 object2 parameter out))

(define-op (invert :extend t) ((vector :*)) (vector)
  "Invert each component of the {VECTOR:DESC} VECTOR (1/n), storing the result in a new ~
{VECTOR:DESC}."
  (invert! vector (default vector)))

(define-op (invert! :extend t) ((vector :*) (out :*)) (vector)
  "Invert each component of the {VECTOR:DESC} VECTOR (1/n), storing the result in the {OUT:DESC} ~
OUT."
  (%with-each (vector x i)
    (setf (ref out i) (if (zerop x) 0d0 (cl:/ x))))
  out)

(define-op lerp ((vector1 :*) (vector2 :*) (parameter real)) (vector)
  "Perform a linear interpolation between the {VECTOR1:DESC}s VECTOR1 and VECTOR2 by the parameter ~
PARAMETER, storing the result in a new {VECTOR1:DESC}."
  (lerp! vector1 vector2 parameter (default vector1)))

(define-op lerp! ((vector1 :*) (vector2 :*) (parameter real) (out :*)) (vector)
  "Perform a linear interpolation between the {VECTOR1:DESC}s VECTOR1 and VECTOR2 by the parameter ~
PARAMETER, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((vector1 x) (vector2 y)) :index i)
    (setf (ref out i) (u:lerp parameter x y)))
  out)

(define-op max ((vector1 :*) (vector2 :*)) (vector)
  "Take the maximum value of each of the corresponding components of the {VECTOR:DESC}s VECTOR1 and ~
VECTOR2, storing the result in a new {VECTOR1:DESC}."
  (max! vector1 vector2 (default vector1)))

(define-op max! ((vector1 :*) (vector2 :*) (out :*)) (vector)
  "Take the maximum value of each of the corresponding components of the {VECTOR:DESC}s VECTOR1 and ~
VECTOR2, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((vector1 x) (vector2 y)) :index i)
    (setf (ref out i) (cl:max x y)))
  out)

(define-op min ((vector1 :*) (vector2 :*)) (vector)
  "Take the minimum value of each of the corresponding components of the {VECTOR:DESC}s VECTOR1 and ~
VECTOR2, storing the result in a new {VECTOR1:DESC}."
  (min! vector1 vector2 (default vector1)))

(define-op min! ((vector1 :*) (vector2 :*) (out :*)) (vector)
  "Take the minimum value of each of the corresponding components of the {VECTOR:DESC}s VECTOR1 and ~
VECTOR2, storing the result in the {OUT:DESC} OUT."
  (%with-each/parallel (((vector1 x) (vector2 y)) :index i)
    (setf (ref out i) (cl:min x y)))
  out)

(define-op mod ((vector :*) (divisor real)) (vector)
  "For each component of the {VECTOR:DESC} compute the modulo of DIVISOR, storing the result in a ~
new {VECTOR:DESC}."
  (mod! vector divisor (default vector)))

(define-op mod! ((vector :*) (divisor real) (out :*)) (vector)
  "For each component of the {VECTOR:DESC} compute the modulo of DIVISOR, storing the result in ~
the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (ffloor x divisor)))
  out)

(define-op ones ((vector :*)) (vector)
  "Construct a new vector from the {VECTOR:DESC} VECTOR with each of its components set to one."
  (ones! (copy vector)))

(define-op ones! ((vector :*)) (vector)
  "Modify the {VECTOR:DESC} VECTOR to have each of its components set to one."
  (fill (components vector) 1d0)
  vector)

(define-op parallel? ((vector1 :*) (vector2 :*)) (vector)
  "Check whether the {VECTOR1:DESC}s VECTOR1 and VECTOR2 are parallel to each other."
  (~= (cl:abs (dot (normalize vector1) (normalize vector2))) 1f0))

(define-op radians->degrees ((vector :*)) (vector)
  "Convert each component of the {VECTOR:DESC} VECTOR, which are assumed to be in radians, to ~
to degrees, storing the result in a new {VECTOR:DESC}."
  (radians->degrees! vector (default vector)))

(define-op radians->degrees! ((vector :*) (out :*)) (vector)
  "Convert each component of the {VECTOR:DESC} VECTOR, which are assumed to be in radians, to ~
to degrees, storing the result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:* x (cl:/ 180 pi))))
  out)

(define-op round ((vector :*)) (vector)
  "Round each component of the {VECTOR:DESC} VECTOR, storing the result in a new {VECTOR:DESC}."
  (round! vector (default vector)))

(define-op round! ((vector :*) (out :*)) (vector)
  "Round each component of the {VECTOR:DESC} VECTOR, storing the result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (fround x)))
  out)

(define-op same-direction? ((vector1 :*) (vector2 :*)) (vector)
  "Check if the {VECTOR1:DESC}s VECTOR1 and VECTOR2 have the same direction."
  (~= (dot (normalize vector1) (normalize vector2)) 1f0))

(define-op sign ((vector :*)) (vector)
  "Take the sign of each component of the {VECTOR:DESC} VECTOR, storing the result in a new ~
{VECTOR:DESC}. All values less than 0 become -1, and all values greater than 0 become +1."
  (sign! vector (default vector)))

(define-op sign! ((vector :*) (out :*)) (vector)
  "Take the sign of each component of the {VECTOR:DESC} VECTOR, storing the result in the ~
{OUT:DESC} OUT. All values less than 0 become -1, and all values greater than 0 become +1."
  (%with-each (vector x i)
    (setf (ref out i) (signum x)))
  out)

(define-op sin ((vector :*)) (vector)
  "Compute the trigonometric sine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (sin! vector (default vector)))

(define-op sin! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric sine of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:sin x)))
  out)

(define-op sqrt ((vector :*)) (vector)
  "Compute the square root of each component in the {VECTOR:DESC} VECTOR, storing the result in a ~
new {VECTOR:DESC}."
  (sqrt! vector (default vector)))

(define-op sqrt! ((vector :*) (out :*)) (vector)
  "Compute the square root of each component in the {VECTOR:DESC} VECTOR, storing the result in ~
the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (if (minusp x) 0d0 (cl:sqrt x))))
  out)

(define-op tan ((vector :*)) (vector)
  "Compute the trigonometric tangent of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in a new {VECTOR:DESC}."
  (tan! vector (default vector)))

(define-op tan! ((vector :*) (out :*)) (vector)
  "Compute the trigonometric tangent of each component of the {VECTOR:DESC} VECTOR, storing the ~
result in the {OUT:DESC} OUT."
  (%with-each (vector x i)
    (setf (ref out i) (cl:tan x)))
  out)

(define-op velocity! ((axis :*) (rate real) (out :*)) (vector)
  "Modify the {OUT:DESC} OUT, to represent a velocity following the right-hand rule, with a ~
direction parallel to the {AXIS:DESC} AXIS, and a magnitude of RATE units per second."
  (copy! axis out)
  (normalize! out out)
  (*! out rate out))

;;; Constants

(u:define-constant +v2-zero+ (vec/zero 2)
  :test #'=
  :documentation "A 2-dimensional zero vector.")

(u:define-constant +v2-ones+ (vec 1 1)
  :test #'=
  :documentation "A 2-dimensional vector with each component set to 1.")

(u:define-constant +v2+x+ (vec 1 0)
  :test #'=
  :documentation "A 2-dimensional vector representing a direction along the positive X axis.")

(u:define-constant +v2-x+ (vec -1 0)
  :test #'=
  :documentation "A 2-dimensional vector representing a direction along the negative X axis.")

(u:define-constant +v2+y+ (vec 0 1)
  :test #'=
  :documentation "A 2-dimensional vector representing a direction along the positive Y axis.")

(u:define-constant +v2-y+ (vec 0 -1)
  :test #'=
  :documentation "A 2-dimensional vector representing a direction along the negative Y axis.")

(u:define-constant +v3-zero+ (vec/zero 3)
  :test #'=
  :documentation "A 3-dimensional zero vector.")

(u:define-constant +v3-ones+ (vec 1 1 1)
  :test #'=
  :documentation "A 3-dimensional vector with each component set to 1.")

(u:define-constant +v3+x+ (vec 1 0 0)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the positive X axis.")

(u:define-constant +v3-x+ (vec -1 0 0)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the negative X axis.")

(u:define-constant +v3+y+ (vec 0 1 0)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the positive Y axis.")

(u:define-constant +v3-y+ (vec 0 -1 0)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the negative Y axis.")

(u:define-constant +v3+z+ (vec 0 0 1)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the positive Z axis.")

(u:define-constant +v3-z+ (vec 0 0 -1)
  :test #'=
  :documentation "A 3-dimensional vector representing a direction along the negative Z axis.")

(u:define-constant +v4-zero+ (vec/zero 4)
  :test #'=
  :documentation "A 4-dimensional zero vector.")

(u:define-constant +v4-ones+ (vec 1 1 1 1)
  :test #'=
  :documentation "A 4-dimensional vector with each component set to 1.")

(u:define-constant +v4+x+ (vec 1 0 0 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the positive X axis.")

(u:define-constant +v4-x+ (vec -1 0 0 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the negative X axis.")

(u:define-constant +v4+y+ (vec 0 1 0 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the positive Y axis.")

(u:define-constant +v4-y+ (vec 0 -1 0 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the negative Y axis.")

(u:define-constant +v4+z+ (vec 0 0 1 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the positive Z axis.")

(u:define-constant +v4-z+ (vec 0 0 -1 0)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the negative Z axis.")

(u:define-constant +v4+w+ (vec 0 0 0 1)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the positive W axis.")

(u:define-constant +v4-w+ (vec 0 0 0 -1)
  :test #'=
  :documentation "A 4-dimensional vector representing a direction along the negative W axis.")
