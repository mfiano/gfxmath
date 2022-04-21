(asdf:defsystem #:gfxmath
  :description "A graphics math library."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/gfxmath"
  :version "0.1.1"
  :encoding :utf-8
  :depends-on (#:mfiano-utils
               #:str)
  :in-order-to ((asdf:test-op (asdf:test-op #:gfxmath.test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "macros")
   (:file "types")
   (:file "common")
   (:file "shared-ops")
   (:file "vector")
   (:file "matrix")
   (:file "quaternion")))
