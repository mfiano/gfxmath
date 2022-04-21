(in-package #:gfxmath)

;;; Metadata for macro expansions

(defvar *types* nil)
(defvar *descriptions* nil)
(defvar *rows->vector* nil)
(defvar *columns->vector* nil)
(defvar *axis-length->vector* nil)
(defvar *matrix->rotation* nil)

;;; Type generation

(defmacro %generate-type (type () &key (base 'math-object) (rows 1) (columns 1)
                                    (description "math object") documentation)
  (let ((size (cl:* rows columns)))
    `(u:eval-always
       (defclass ,type (,base) ()
         (:default-initargs
          :row-count ,rows
          :column-count ,columns
          :components (u:make-f64-array ,size)
          :components/single (u:make-f32-array ,size))
         ,@(when documentation `((:documentation ,(format nil documentation)))))
       (pushnew ',type *types*)
       (pushnew (cons ',type ,description) *descriptions* :test #'equalp)
       ,@(when (eq base 'matrix)
           (let* ((*package* (symbol-package '%generate-type))
                  (vector-row-type (u:format-symbol *package* "VECTOR~d" rows))
                  (vector-column-type (u:format-symbol *package* "VECTOR~d" columns))
                  (vector-axis-type (u:format-symbol *package* "VECTOR~d" (cl:max (1- rows) 2)))
                  (rotation-type (u:format-symbol *package* "MATRIX~d" (cl:max (1- rows) 2))))
             `((pushnew (cons ',type ',vector-row-type) *rows->vector* :test #'equal)
               (pushnew (cons ',type ',vector-column-type) *columns->vector* :test #'equal)
               (pushnew (cons ',type ',vector-axis-type) *axis-length->vector* :test #'equal)
               (pushnew (cons ',type ',rotation-type) *matrix->rotation* :test #'equal))))
       nil)))

(defmacro %generate-accessors (type &body component-names)
  `(u:eval-always
     ,@(loop :for name :in component-names
             :for i :from 0
             :collect `(define-op (,name :extend t) ((object :*)) (,type)
                         ,(format nil "Get the ~a component of the given {OBJECT:DESC}." name)
                         (ref object ,i))
             :collect `(define-op ((setf ,name) :extend t) ((value real) (object :*)) (,type)
                         ,(format nil "Set the ~a component of the given {OBJECT:DESC}." name)
                         (setf (ref object ,i) value)))))

;;; Component iteration

(defmacro %with-rows ((object index &key limit row-count) &body body)
  (let ((rows (or row-count (u:make-gensym '#:rows))))
    `(let ((,rows ,(if limit
                       `(u:clamp ,limit 1 (row-count ,object))
                       `(row-count ,object))))
       (dotimes (,index ,rows)
         (declare (ignorable ,index))
         ,@body))))

(defmacro %with-columns ((object index &key limit column-count) &body body)
  (let ((columns (or column-count (u:make-gensym '#:columns))))
    `(let ((,columns ,(if limit
                          `(u:clamp ,limit 1 (column-count ,object))
                          `(column-count ,object))))
       (dotimes (,index ,columns)
         (declare (ignorable ,index))
         ,@body))))

(defmacro %with-each/2d ((object component row-index column-index &key row-limit column-limit)
                         &body body)
  (u:once-only (object)
    (u:with-gensyms (row-limit-symbol column-limit-symbol)
      `(let (,@(when row-limit `((,row-limit-symbol ,row-limit)))
             ,@(when column-limit `((,column-limit-symbol ,column-limit))))
         (%with-rows (,object ,row-index :limit ,(when row-limit row-limit-symbol))
           (%with-columns (,object ,column-index :limit ,(when column-limit column-limit-symbol))
             (let ((,component (mref ,object ,row-index ,column-index)))
               (declare (ignorable ,component))
               ,@body)))))))

(defmacro %with-each ((object component index &key (from 0)) &body body)
  (u:with-gensyms (components)
    `(loop :with ,components := (components ,object)
           :for ,index :from ,from :below (length ,components)
           :for ,component := (aref ,components ,index)
           :do (progn ,@body))))

(defmacro %with-each/parallel ((specs &key index (from 0)) &body body)
  (let ((index (or index (gensym "INDEX"))))
    `(loop :for ,index :from ,from :below (length (components ,(caar specs)))
           ,@(loop :for (object component) :in specs
                   :append `(:for ,component := (ref ,object ,index)))
           :do (progn ,@body))))

(defun %generate-bindings/1d (specs &key quaternion)
  (flet ((%make-symbol (prefix accessor)
           (u:format-symbol (symbol-package prefix) "~a~a" prefix accessor)))
    (loop :with accessors := (if quaternion '(w x y z) '(x y z w))
          :for (size prefix matrix) :in specs
          :append `((,prefix ,matrix))
          :append (loop :for i :below size
                        :for accessor :in accessors
                        :append `((,(%make-symbol prefix accessor)
                                   (ref ,prefix ,i)))))))

(defun %generate-bindings/2d (specs)
  (flet ((%make-symbol (prefix row column)
           (u:format-symbol (symbol-package prefix) "~a~d~d" prefix row column)))
    (loop :for (size prefix matrix) :in specs
          :append `((,prefix ,matrix))
          :append (loop :for column :in (u:iota size)
                        :append (loop :for row :in (u:iota size)
                                      :append `((,(%make-symbol prefix row column)
                                                 (mref ,prefix ,row ,column))))))))

(defmacro %with-components ((specs bindings (&key (read-only t))) &body body)
  (flet ((find-variables (var)
           (find (first var) specs :key #'second)))
    (let* ((type-bindings (remove-if-not #'find-variables bindings))
           (component-bindings (remove-if #'find-variables bindings)))
      (if read-only
          `(let* ,bindings
             (declare (ignorable ,@(mapcar #'first bindings)))
             ,@body)
          `(let (,@type-bindings)
             (declare (ignorable ,@(mapcar #'first type-bindings)))
             (symbol-macrolet ,component-bindings
               ,@body))))))

(defmacro with-vector (specs (&key (read-only t)) &body body)
  "Lexically bind each component of the given vectors by prepending PREFIX to the component name. ~
SPECS is a list of specifications, with each being of the form (SIZE PREFIX OBJECT) where:

SIZE is the dimensionality of the vector: 2, 3, or 4.

PREFIX is a symbol to prefix each component name with. For example, `V` will produce a binding ~
named `VY` for the Y component of the vector.

OBJECT is the name of a variable bound to a vector of the given SIZE.

If :READ-ONLY is nil, each binding is SETF-able."
  (let ((bindings (%generate-bindings/1d specs)))
    `(%with-components (,specs ,bindings (:read-only ,read-only))
       ,@body)))

(defmacro with-matrix (specs (&key (read-only t)) &body body)
  "Lexically bind each component of the given matrices by prepending PREFIX to the to the row and ~
column indices. SPECS is a list of specifications, with each being of the form ~
(SIZE PREFIX OBJECT) where:

SIZE is the dimensionality of the matrix: 2, 3, or 4.

PREFIX is a symbol to prefix each row and column with: For example, `M` will produce a binding ~
named `M12` for the component located at row 1 and column 2 of the matrix.

OBJECT is the name of a variable bound to a matrix of the given SIZE.

If :READ-ONLY is nil, each binding is SETF-able."
  (let ((bindings (%generate-bindings/2d specs)))
    `(%with-components (,specs ,bindings (:read-only ,read-only))
       ,@body)))

(defmacro with-quaternion (specs (&key (read-only t)) &body body)
  "Lexically bind each component of the given quaternions by prepending PREFIX to the component ~
name. SPECS is a list of specifications, with each being of the form ~ (PREFIX OBJECT) where: ~

PREFIX is a symbol to prefix each component name with. For example, `Q` will produce a binding ~
named `QY` for the Y component of the quaternion.

OBJECT is the name of a variable bound to a quaternion.

If :READ-ONLY is nil, each binding is SETF-able."
  (let* ((specs (mapcar (lambda (x) (cons 4 x)) specs))
         (bindings (%generate-bindings/1d specs :quaternion t)))
    `(%with-components (,specs ,bindings (:read-only ,read-only))
       ,@body)))

;;; Operation definition

(defun %parse-required-parameters (parameters)
  (u:when-let* ((method-required (u:parse-ordinary-lambda-list parameters :allow-specializers t))
                (generic-required (mapcar (lambda (x) (first (u:ensure-list x))) method-required)))
    (values generic-required method-required)))

(defun %parse-optional-parameters (parameters)
  (flet ((%parse-method-parameters (specs)
           (mapcar
            (lambda (x)
              (destructuring-bind (var value supplied) x
                `(,var ,value ,@(when supplied `(,supplied)))))
            specs)))
    (u:when-let* ((specs (nth-value 1 (u:parse-ordinary-lambda-list parameters
                                                                    :allow-specializers t)))
                  (generic-parameters (mapcar #'first specs))
                  (method-parameters (%parse-method-parameters specs)))
      (values (list* '&optional generic-parameters)
              (list* '&optional method-parameters)))))

(defun %parse-keyword-parameters (parameters)
  (flet ((%parse-method-parameters (specs)
           (mapcar
            (lambda (x)
              (destructuring-bind (key/var value supplied) x
                `(,key/var ,value ,@(when supplied `(,supplied)))))
            specs)))
    (u:when-let* ((specs (nth-value 3 (u:parse-ordinary-lambda-list parameters
                                                                    :allow-specializers t)))
                  (generic-parameters (mapcar (lambda (x) (list (first x))) specs))
                  (method-parameters (%parse-method-parameters specs)))
      (values (list* '&key generic-parameters)
              (list* '&key method-parameters)))))

(defun %parse-rest-parameter (parameters)
  (u:when-let ((rest (nth-value 2 (u:parse-ordinary-lambda-list parameters :allow-specializers t))))
    (list '&rest rest)))

(defun %parse-parameters (parameters)
  (u:mvlet* ((generic-required method-required (%parse-required-parameters parameters))
             (generic-optionals method-optionals (%parse-optional-parameters parameters))
             (generic-keywords method-keywords (%parse-keyword-parameters parameters))
             (rest (%parse-rest-parameter parameters)))
    (list (cons generic-required method-required)
          (cons generic-keywords method-keywords)
          rest)))

(defun %parse-specialized-specs (type specs)
  (mapcar
   (lambda (x)
     (destructuring-bind (var &optional specializer) (u:ensure-list x)
       (cond
         ((eq specializer :*)
          `(,var ,type))
         ((listp x)
          `(,var ,specializer))
         (t `(,var t)))))
   specs))

(defun %parse-extended-eql-specializers (specs)
  (flet ((collect ()
           (let ((result nil)
                 (positions nil)
                 (index 0))
             (dolist (x specs)
               (let ((specializer (second (u:ensure-list x))))
                 (destructuring-bind (dispatch-type &rest values) (u:ensure-list specializer)
                   (when (and (eq dispatch-type 'eql) values)
                     (push index positions)
                     (push values result))
                   (incf index))))
             (values (nreverse positions)
                     (nreverse result)))))
    (u:mvlet ((positions values (collect)))
      (when (and values (some (lambda (x) (cl:> (length x) 1)) values))
        ;; There are extended EQL specializers, so extract their product to construct additional
        ;;  method lambda lists in the caller.
        (u:mappend
         (lambda (x)
           (let ((modified (copy-tree specs)))
             (u:do-plist (index value x)
               (setf (cdr (nth index modified)) `((eql ,value))))
             (list modified)))
         (apply #'u:map-product (lambda (&rest args) (u:interleave positions args)) values))))))

(defun %parse-extended-class-specializers (type specs)
  (mapcar
   (lambda (spec)
     (destructuring-bind (var specializer) spec
       (case specializer
         (:row-sized-vector
          `(,var ,(cdr (assoc type *rows->vector*))))
         (:column-sized-vector
          `(,var ,(cdr (assoc type *columns->vector*))))
         (:axis-sized-vector
          `(,var ,(cdr (assoc type *axis-length->vector*))))
         (:sub-matrix
          `(,var ,(cdr (assoc type *matrix->rotation*))))
         (t spec))))
   specs))

(defun %parse-extended-specializers (type required)
  (let* ((specs (%parse-specialized-specs type required))
         (class (%parse-extended-class-specializers type specs))
         (eql (%parse-extended-eql-specializers class)))
    (or eql (list class))))

(defun %filter-types (filter)
  (if (eq filter :all)
      *types*
      (nreverse
       (remove-if-not
        (lambda (x)
          (some
           (lambda (y)
             (u:string-starts-with-p x (symbol-name y)))
           filter))
        *types*
        :key #'symbol-name))))

(defun %process-documentation (string spec)
  (flet ((%desc (name type string)
           (str:replace-all (format nil "\{~a:DESC\}" name)
                            (format nil "~a" (or (cdr (assoc type *descriptions*)) type))
                            string))
         (%row-desc (name type string)
           (str:replace-all (format nil "\{~a:ROW-DESC\}" name)
                            (cdr (assoc (cdr (assoc type *rows->vector*)) *descriptions*))
                            string))
         (%column-desc (name type string)
           (str:replace-all (format nil "\{~a:COLUMN-DESC\}" name)
                            (cdr (assoc (cdr (assoc type *columns->vector*)) *descriptions*))
                            string))
         (%axis-desc (name type string)
           (str:replace-all (format nil "\{~a:AXIS-DESC\}" name)
                            (cdr (assoc (cdr (assoc type *axis-length->vector*)) *descriptions*))
                            string))
         (%rotation-desc (name type string)
           (str:replace-all (format nil "\{~a:ROTATION-DESC\}" name)
                            (cdr (assoc (cdr (assoc type *matrix->rotation*)) *descriptions*))
                            string))
         (%ordinal (name type string)
           (if (numberp type)
               (str:replace-all (format nil "\{~a:ORD\}" name)
                                (format nil "~d~:[~[th~;st~;nd~;rd~:;th~]~;th~]" type
                                        (cl:< (cl:mod (cl:- type 10) 100) 10) (cl:mod type 10))
                                string)
               string)))
    (dolist (x spec)
      (destructuring-bind (name specializer) x
        (let ((type (car (remove 'eql (u:ensure-list specializer)))))
          (setf string (%desc name type string)
                string (%row-desc name type string)
                string (%column-desc name type string)
                string (%axis-desc name type string)
                string (%rotation-desc name type string)
                string (%ordinal name type string)))))
    (format nil (str:replace-all "matrixs" "matrices" string))))

(defmacro doc (string)
  (format nil string))

(defmacro define-op (op lambda-list filter &body body)
  (u:mvlet ((body decls doc (u:parse-body body :documentation t))
            (op (if (eq (car (u:ensure-list op)) 'setf) (list op) (u:ensure-list op))))
    (destructuring-bind ((generic-required . method-required)
                         (generic-keywords . method-keywords)
                         rest)
        (%parse-parameters lambda-list)
      (let ((generic-parameters `(,@generic-required ,@rest ,@generic-keywords))
            (filtered-types (%filter-types filter)))
        (destructuring-bind (op &key extend (methods t)) op
          `(u:eval-always
             ,@(unless extend `((defgeneric ,op ,generic-parameters)))
             ,@(when methods
                 (u:mappend
                  (lambda (type)
                    (let ((specs (%parse-extended-specializers type method-required)))
                      (mapcar
                       (lambda (required)
                         `(defmethod ,op (,@required ,@rest ,@method-keywords)
                            ,@(when doc `(,(%process-documentation doc required)))
                            ,@body))
                       specs)))
                  filtered-types))
             (export ',op)))))))
