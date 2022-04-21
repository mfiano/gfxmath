(in-package #:gfxmath.test)

(defun v (&rest args)
  (apply #'m:vec args))

(defun v0 (size)
  (m:vec/zero size))

(defun m (&rest args)
  (apply #'m:mat/from-vecs args))

(defun m0 (size)
  (m:mat size))

(defun mid (size)
  (m:mat/id size))

(defun q (w x y z)
  (m:quat w x y z))

(defun qid ()
  (m:quat/id))

(defun ~= (key1 key2)
  (lambda (x y)
    (every (lambda (c1 c2) (m::~= c1 c2 :rel 1d-6))
           (funcall key1 x)
           (funcall key2 y))))

(defun o= (object expected desc)
  (is object expected desc :test (~= #'m::components #'m::components)))

(defun o/= (object expected desc)
  (isnt object expected desc :test (~= #'m::components #'m::components)))

(defun oa= (object expected desc)
  (is object expected desc :test (~= #'m::components #'identity)))

(defun a= (object expected desc)
  (is object expected desc :test (~= #'identity #'identity)))

(defun s= (object expected desc)
  (is object expected desc :test #'m::~=))
