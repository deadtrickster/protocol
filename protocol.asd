(asdf:defsystem #:protocol
  :description "Continuations for IOLIB"
  :license "MIT"
  :depends-on (#:alexandria
               #:babel-streams
               #:cl-cont
               #:iolib
               #:pack)
  :serial t
  :components
  ((:file "packages")
   (:module :src
            :components
            ((:file "connection" :depends-on ("recv-buf"))
             (:file "recv-buf")))))
