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
           #:eat
           #:eat-stream
           #:eat-to-delimiter
           #:eat-to-expected
           #:recv
           #:recv-buf
           #:recv-delimited
           #:recv-fixed
           #:recv-recipe
           #:recv-stream
           #:recv-unpack
           #:recv-variable
           #:send
           #:send-bytes
           #:stream-to-delimiter
           #:stream-to-expected
           #:with-recv))
