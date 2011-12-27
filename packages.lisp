(in-package #:common-lisp-user)

(defpackage #:protocol
  (:use #:alexandria
        #:cl-cont
        #:common-lisp
        #:pack)
  (:export #:*event-base*
           #:connection
           #:connection-buffer
           #:connection-socket
           #:recv
           #:recv-buf
           #:recv-delimited
           #:recv-fixed
           #:recv-stream
           #:recv-variable
           #:send
           #:send-bytes
           #:stream-to-delimited
           #:stream-to-expected
           #:with-recv))
