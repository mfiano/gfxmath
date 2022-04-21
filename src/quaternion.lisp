(in-package #:gfxmath)

;;; Utilities

(defun %quaternion-to-matrix (quaternion out)
  (with-quaternion ((q quaternion)) ()
    (let ((xx (cl:* qx qx))
          (xy (cl:* qx qy))
          (xz (cl:* qx qz))
          (xw (cl:* qx qw))
          (yy (cl:* qy qy))
          (yz (cl:* qy qz))
          (yw (cl:* qy qw))
          (zz (cl:* qz qz))
          (zw (cl:* qz qw)))
      (setf (mref out 0 0) (cl:- 1 (cl:* (cl:+ yy zz) 2d0))
            (mref out 1 0) (cl:* (cl:+ xy zw) 2d0)
            (mref out 2 0) (cl:* (cl:- xz yw) 2d0)
            (mref out 0 1) (cl:* (cl:- xy zw) 2d0)
            (mref out 1 1) (cl:- 1 (cl:* (cl:+ xx zz) 2d0))
            (mref out 2 1) (cl:* (cl:+ yz xw) 2d0)
            (mref out 0 2) (cl:* (cl:+ xz yw) 2d0)
            (mref out 1 2) (cl:* (cl:- yz xw) 2d0)
            (mref out 2 2) (cl:- 1 (cl:* (cl:+ xx yy) 2d0)))
      out)))

;;; Accessors

(%generate-accessors quaternion w x y z)

;;; Constructors

(u:eval-always
  (defun quat (w x y z)
    "Construct a quaternion with its components set to the corresponding argument."
    (let ((quaternion (make-instance 'quaternion)))
      (setf (ref quaternion 0) w
            (ref quaternion 1) x
            (ref quaternion 2) y
            (ref quaternion 3) z)
      quaternion)))

(u:eval-always
  (defun quat/id ()
    "Construct an identity quaternion."
    (id! (make-instance 'quaternion))))

(defun quat/from-axis-angle (axis angle)
  "Construct a quaternion from the ANGLE in radians around the axis denoted by the 3-dimensional ~
vector AXIS. "
  (from-axis-angle! axis angle (quat/id)))

(defun quat/from-mat (matrix)
  "Construct a quaternion from the given matrix. MATRIX may be either a 3x3 or 4x4 matrix."
  (from-matrix! matrix (quat/id)))

(defun quat/from-velocity (velocity delta)
  "Construct a unit quaternion representing an angular velocity rotation in DELTA units of time, ~
from the 3-dimensional vector VELOCITY, a vector with its magnitude representing a radians per ~
second rotation around its axis."
  (from-velocity! velocity delta (quat/id)))

(defun quat/oriented (space &rest axes/angles)
  "Construct a quaternion representing a series of rotations around the axes and angles given. ~
AXES/ANGLES are pairs of axes and angles, with an axis being either one of the symbols :X, :Y, or ~
:Z, or a 3-dimensional vector representing an arbitrary axis, and angle being any real number ~
representing the angle in radians around its paired axis."
  (apply #'orient! space (quat/id) axes/angles))

;;; Operations

(define-op (* :extend t) ((object1 :*) (object2 :*)) (quaternion)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in a new ~
{OBJECT1:DESC}."
  (*! object1 object2 (quat/id)))

(define-op (*! :extend t) ((object1 :*) (object2 :*) (out :*)) (quaternion)
  "Multiply the {OBJECT1:DESC} OBJECT1 by the {OBJECT2:DESC} OBJECT2, storing the result in the ~
{OUT:DESC} OUT."
  (with-quaternion ((q1 object1) (q2 object2)) ()
    (psetf (w out) (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))
           (x out) (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z)) (cl:* q1z q2y))
           (y out) (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x)) (cl:* q1x q2z))
           (z out) (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y)) (cl:* q1y q2x)))
    out))

(define-op conjugate ((quaternion :*)) (quaternion)
  "Compute the conjugate of the {QUATERNION:DESC} QUATERNION, storing the result in a new ~
{QUATERNION:DESC}."
  (conjugate! quaternion (quat/id)))

(define-op conjugate! ((quaternion :*) (out :*)) (quaternion)
  "Compute the conjugate of the {QUATERNION:DESC} QUATERNION, storing the result in the {OUT:DESC}."
  (%with-each (quaternion x i :from 1)
    (setf (ref out i) (cl:- x)))
  (setf (w out) (w quaternion))
  out)

(define-op (default :extend t) ((object :*)) (quaternion)
  "Construct a default quaternion. Each math type has this method defined, and for quaternions ~
this creates an identity quaternion."
  (id object))

(define-op from-axis-angle! ((axis vector3) (angle real) (out :*)) (quaternion)
  "Modify the quaternion OUT to be oriented around the axis denoted by the {AXIS:DESC} AXIS by ~
ANGLE radians."
  (let* ((half-angle (cl:* angle 0.5d0))
         (axis (* (normalize axis) (cl:sin half-angle))))
    (setf (w out) (cl:cos half-angle)
          (x out) (x axis)
          (y out) (y axis)
          (z out) (z axis))
    out))

(define-op from-matrix ((matrix :*) (out quaternion)) (matrix3 matrix4)
  "Convert the rotation of the {MATRIX:DESC} MATRIX to a quaternion, storing the result in a new ~
quaternion"
  (from-matrix! matrix out))

(define-op from-matrix! ((matrix :*) (out quaternion)) (matrix3 matrix4)
  "Convert the rotation of the {MATRIX:DESC} MATRIX to a quaternion, storing the result in the ~
quaternion OUT."
  (with-matrix ((3 m matrix)) ()
    (setf (w out) (cl:* (cl:sqrt (cl:max 0d0 (cl:+ m00 m11 m22 1d0))) 0.5d0)
          (x out) (cl:* (cl:sqrt (cl:max 0d0 (cl:- (cl:+ m00 1d0) m11 m22)))
                        (signum (cl:- m21 m12))
                        0.5d0)
          (y out) (cl:* (cl:sqrt (cl:max 0d0 (cl:- (cl:+ (cl:- m00 1d0) m11) m22)))
                        (signum (cl:- m02 m20))
                        0.5d0)
          (z out) (cl:* (cl:sqrt (cl:max 0d0 (cl:+ (cl:- 1d0 m00 m11) m22)))
                        (signum (cl:- m10 m01))
                        0.5d0))
    out))

(define-op from-velocity! ((velocity vector3) (delta real) (out :*)) (quaternion)
  "Construct a unit quaternion representing an angular velocity rotation in DELTA units of time, ~
from the {VELOCITY:DESC} VELOCITY, a vector with its magnitude representing a radians per second ~
rotation around its axis, storing the result in the quaternion OUT."
  (from-axis-angle! (normalize velocity) (cl:* (magnitude velocity) delta) out)
  (normalize! out out))

(define-op (id :extend t) ((object :*)) (quaternion)
  "Construct an identity quaternion."
  (id! (copy object)))

(define-op (id! :extend t) ((object :*)) (quaternion)
  "Modify the quaternion OBJECT to be an identity quaternion."
  (setf (w object) 1d0 (x object) 0d0 (y object) 0d0 (z object) 0d0)
  object)

(define-op (id? :extend t) ((object :*)) (quaternion)
  "Check whether the quaternion OBJECT is an identity quaternion."
  (and (~= (w object) 1d0) (~= (x object) 0d0) (~= (y object) 0d0) (~= (z object) 0d0)))

(define-op (interpolate :extend t) ((object1 :*) (object2 :*) (parameter real)) (quaternion)
  "Perform a suitable interpolation between the {OBJECT1:DESC}s OBJECT1 and OBJECT2, storing the ~
result in a new {OBJECT1:DESC}. This is the same as LERP for vectors and SLERP for quaternions. ~
It exists to conveniently dispatch to the appropriate interpolation method given the types of the ~
objects."
  (slerp object1 object2 parameter))

(define-op (interpolate! :extend t) ((object1 :*) (object2 :*) (parameter real) (out :*))
    (quaternion)
  "Perform a suitable interpolation between the {OBJECT1:DESC}s OBJECT1 and OBJECT2, storing the ~
result in the {OUT:DESC} OUT. This is the same as LERP! for vectors and SLERP! for quaternions. ~
It exists to conveniently dispatch to the appropriate interpolation method given the types of the ~
objects."
  (slerp! object1 object2 parameter out))

(define-op inverse ((quaternion :*)) (quaternion)
  "Compute the inverse of the quaternion QUATERNION, storing the result in a new quaternion."
  (inverse! quaternion (quat/id)))

(define-op inverse! ((quaternion :*) (out :*)) (quaternion)
  "Compute the inverse of the quaternion QUATERNION, storing the result in the quaternion OUT."
  (conjugate! quaternion out)
  (*! out (cl:/ (magnitude-squared quaternion)) out))

(define-op orient! (space (out :*) &rest axes/angles) (quaternion)
  "Construct a quaternion representing a series of rotations around the axes and angles given, ~
storing the result in the quaternion OUT. AXES/ANGLES are pairs of axes and angles, with an axis ~
being either one of the symbols :X, :Y, or :Z, or a 3-dimensional vector representing an arbitrary ~
axis, and angle being any real number representing the angle in radians around its paired axis."
  (loop :with orientation := (copy out)
        :for (axis angle) :on axes/angles :by #'cddr
        :for vector := (case axis
                         (:x +v3+x+)
                         (:y +v3+y+)
                         (:z +v3+z+)
                         (t (normalize! axis axis)))
        :do (from-axis-angle! vector angle orientation)
            (ecase space
              (:local (*! orientation out out))
              (:world (*! out orientation out)))
            (normalize! out out))
  out)

(define-op (rotate :extend t) ((object1 :*) (object2 :*) &key (space :local)) (quaternion)
  "Perform a quaternion rotation by multiplying the quaternion OBJECT1 by the quaternion OBJECT2, ~
storing the result in a new quaternion. If SPACE is :WORLD instead of the default of :LOCAL, ~
OBJECT2 is multiplied by OBJECT1."
  (rotate! object1 object2 (quat/id) :space space))

(define-op (rotate! :extend t) ((object1 :*) (object2 :*) (out :*) &key (space :local))
    (quaternion)
  "Perform a quaternion rotation by multiplying the quaternion OBJECT1 by the quaternion OBJECT2, ~
storing the result in the quaternion OUT. If SPACE is :WORLD instead of the default of :LOCAL, ~
OBJECT2 is multiplied by OBJECT1."
  (ecase space
    (:local
     (*! object1 object2 out))
    (:world
     (*! object2 object1 out)))
  (normalize! out out))

(define-op (rotate :extend t) ((object1 :*) (object2 vector3) &key (space :local)) (quaternion)
  "Rotate the quaternion OBJECT1 by the {OBJECT2:DESC} OBJECT2 denoting Euler angles in radians, ~
storing the result in a new quaternion. If SPACE is :WORLD instead of the default of :LOCAL, the ~
inverse rotation is performed."
  (rotate! object1 object2 (quat/id) :space space))

(define-op (rotate! :extend t) ((object1 :*) (object2 vector3) (out :*) &key (space :local))
    (quaternion)
  "Rotate the quaternion OBJECT1 by the {OBJECT2:DESC} OBJECT2 denoting Euler angles in radians, ~
storing the result in the quaternion OUT. If SPACE is :WORLD instead of the default of :LOCAL, the ~
inverse rotation is performed."
  (let ((half-angles (* object2 0.5d0)))
    (with-vector ((3 s (sin half-angles)) (3 c (cos half-angles))) ()
      (setf (w out) (cl:- (cl:* cx cy cz) (cl:* sx sy sz))
            (x out) (cl:+ (cl:* sx cy cz) (cl:* cx sy sz))
            (y out) (cl:- (cl:* sy cz sz) (cl:* sx cy sz))
            (z out) (cl:+ (cl:* sx sy cz) (cl:* cx cy sz))))
    (rotate! object1 out out :space space)))

(define-op slerp ((quaternion1 :*) (quaternion2 :*) (parameter real)) (quaternion)
  "Perform a spherical linear interpolation between the quaternions QUATERNION1 and QUATERNION2 by ~
the parameter PARAMETER, storing the result in a new quaternion."
  (slerp! quaternion1 quaternion2 parameter (quat/id)))

(define-op slerp! ((quaternion1 :*) (quaternion2 :*) (parameter real) (out :*))
    (quaternion)
  "Perform a spherical linear interpolation between the quaternions QUATERNION1 and QUATERNION2 by ~
the parameter PARAMETER, storing the result in the quaternion OUT."
  (let ((dot (dot quaternion1 quaternion2)))
    (copy! quaternion2 out)
    (when (minusp dot)
      (setf quaternion2 (negate quaternion2)
            dot (cl:- dot)))
    (if (cl:> (cl:abs dot) 0.9995d0)
        (%with-each/parallel (((quaternion1 x) (quaternion2 y)) :index i)
          (setf (ref out i) (u:lerp parameter x y)))
        (let* ((angle (cl:acos dot))
               (sin-angle (cl:sin angle))
               (scale1 (cl:/ (cl:sin (cl:* angle (cl:- 1 parameter))) sin-angle))
               (scale2 (cl:/ (cl:sin (cl:* parameter angle)) sin-angle)))
          (%with-each/parallel (((quaternion1 x) (quaternion2 y)) :index i)
            (setf (ref out i) (cl:+ (cl:* x scale1) (cl:* y scale2))))))
    out))

(define-op to-euler-angles ((quaternion :*)) (quaternion)
  "Convert the quaternion QUATERNION to a 3-dimensional vector of Euler angles in radians, storing ~
the result in a new 3-dimensional vector."
  (to-euler-angles! quaternion (vec/zero 3)))

(define-op to-euler-angles! ((quaternion :*) (out vector3)) (quaternion)
  "Convert the quaternion QUATERNION to a 3-dimensional vector of Euler angles in radians, storing ~
the result in the {OUT:DESC} OUT.."
  (macrolet ((op (op a b c d)
               `(cl:* (,op (cl:* ,a ,b) (cl:* ,c ,d)) 2d0)))
    (with-quaternion ((q quaternion)) ()
      (let ((s (op cl:- qw qy qz qx)))
        (setf (x out) (cl:atan (op cl:+ qw qx qy qz) (cl:- 1 (op cl:+ qx qx qy qy)))
              (y out) (if (cl:>= (cl:abs s) 1d0) (cl:* (cl:/ pi 2) (signum s)) (cl:asin s))
              (z out) (cl:atan (op cl:+ qw qz qx qy) (cl:- 1 (op cl:+ qy qy qz qz)))))
      out)))

(define-op to-matrix3 ((quaternion :*)) (quaternion)
  "Convert the quaternion QUATERNION to a 3x3 matrix. storing the result in a new 3x3 matrix."
  (to-matrix3! quaternion (mat/id 3)))

(define-op to-matrix3! ((quaternion :*) (out matrix3)) (quaternion)
  "Convert the quaternion QUATERNION to a 3x3 matrix. storing the result in a the {OUT:DESC} OUT."
  (%quaternion-to-matrix quaternion out))

(define-op to-matrix4 ((quaternion :*)) (quaternion)
  "Convert the quaternion QUATERNION to a 4x4 matrix. storing the result in a new 4x4 matrix."
  (to-matrix4! quaternion (mat/id 4)))

(define-op to-matrix4! ((quaternion :*) (out matrix4)) (quaternion)
  "Convert the quaternion QUATERNION to a 4x4 matrix. storing the result in a the {OUT:DESC} OUT."
  (%quaternion-to-matrix quaternion out))

;;; Constants

(u:define-constant +q-id+ (quat/id)
  :test #'=
  :documentation "An identity quaternion.")
