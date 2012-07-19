(in-package #:common-lisp-user)

(defpackage #:protocol
  (:use #:alexandria
        #:cl-cont
        #:common-lisp
        #:yapack)
  (:export #:*event-base*
           #:connection
           #:connection-buffer
           #:connection-socket
           #:eat
           #:eat-stream
           #:eat-to-expected
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
