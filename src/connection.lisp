(in-package #:protocol)

(defvar *event-base* (make-instance 'iomux:event-base))

(defclass connection ()
  ((socket
    :reader connection-socket
    :initarg :socket)
   (buffer
    :reader connection-buffer
    :initform (make-instance 'recv-buf))))

(defun make-connection (&rest args)
  (make-instance 'connection :socket (apply #'sockets:make-socket args)))

;; sockets:receive-from can throw end-of-file
(defun recv-some (cnn &optional (num-bytes +page-size+))
  (declare (optimize (speed 3)))
  (with-slots (buffer read write)
      (ensure-space (connection-buffer cnn) num-bytes)
    (declare (fixnum write))
    (multiple-value-bind (_ nbytes)
        (sockets:receive-from (connection-socket cnn) :buffer buffer :start write :dont-wait t)
      (declare (ignore _) (fixnum nbytes))
      (incf write nbytes))
    (connection-buffer cnn)))

;; returns recv-buf
(defun/cc recv-delimited (cnn delimiter)
  (declare (octets delimiter))
  (let/cc cont
    (labels ((self (fd event exception)
               (declare (ignore event exception))
               (handler-case
                   (recv-some cnn)
                 (end-of-file ()
                   (iomux:remove-fd-handlers *event-base* fd :read t)
                   (funcall cont (connection-buffer cnn))))
               (when (position-of-delimiter (connection-buffer cnn) delimiter)
                 (iomux:remove-fd-handlers *event-base* fd :read t)
                 (funcall cont (connection-buffer cnn)))))
      (if (position-of-delimiter (connection-buffer cnn) delimiter)
          (funcall cont (connection-buffer cnn))
          (iomux:set-io-handler *event-base* (sockets:socket-os-fd (connection-socket cnn)) :read #'self)))))

;; returns recv-buf
(defun/cc recv-fixed (cnn length)
  (let/cc cont
    (with-slots (expected)
        (connection-buffer cnn)
      (incf expected length)
      (labels ((self (fd event exception)
                 (declare (ignore event exception))
                 (recv-some cnn (- expected (recv-available (connection-buffer cnn))))
                 (when (>= (recv-available (connection-buffer cnn)) expected)
                   (iomux:remove-fd-handlers *event-base* fd :read t)
                   (funcall cont (connection-buffer cnn)))))
        (if (>= (recv-available (connection-buffer cnn)) expected)
            (funcall cont (connection-buffer cnn))
            (iomux:set-io-handler *event-base* (sockets:socket-os-fd (connection-socket cnn)) :read #'self))))))

;; returns recv-buf
(defun/cc recv-variable (cnn type &optional offset)
  (let/cc cont
    (let ((size (size-of type))
          (recv-buf (connection-buffer cnn)))
      (unless offset
        (setf offset (- size)))
      (let* ((start
              (if (minusp offset)
                  (- (recv-expected recv-buf) offset)
                  offset))
             (end
              (+ start size))
             (stream
              (flex:make-in-memory-input-stream (recv-buffer recv-buf)
                                                :start (+ (recv-read recv-buf) start)
                                                :end (+ (recv-read recv-buf) end)))
             (length
              (unpack stream type)))
        (recv-fixed cont cnn length)))))

(defun plan (recipe)
  (let ((size 0)
        (plan nil))
    (dolist (step recipe)
      (cond
        ((keywordp step)
         (incf size (size-of step)))
        ((listp step)
         (incf size (size-of (second step)))
         (push (list 'read-fixed size) plan)
         (push (list 'read-variable (first step)) plan)
         (setf size 0))))
    (unless (zerop size)
      (push (list 'read-fixed size) plan))
    (nreverse plan)))

(defun recipe (vars)
  (mapcar (lambda (var)
            (cond
              ((and (listp var)
                    (keywordp (first var)))
               var)
              ((listp var)
               (second var))
              (t
               var)))
          vars))

;; returns stream
(defun/cc recv-plan (cnn plan)
  (let/cc cont
    (labels ((self (recv-buf)
               (if (null plan)
                   (funcall cont (stream-to-expected recv-buf))
                   (let ((step (pop plan)))
                     (apply (first step) #'self cnn (rest step))))))
      (self (connection-buffer cnn)))))

;; returns stream
(defun/cc recv-recipe (cnn recipe)
  (recv-plan cnn (plan recipe)))

(defun/cc send-bytes (cnn bytes)
  (let/cc cont
    (let ((sent 0))
      (labels ((self (fd event exception)
                 (declare (ignore fd event exception))
                 (handler-case
                     (incf sent (sockets:send-to (connection-socket cnn) bytes :start sent :dont-wait t))
                   ;; will generally be a hangup, but whatever...
                   (condition (c)
                     (done c)))
                 (when (>= sent (length bytes))
                   (done)))
               (done (&optional condition)
                 (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd (connection-socket cnn)) :write t)
                 (if condition
                     (funcall cont condition)
                     (funcall cont))))
        (iomux:set-io-handler *event-base* (sockets:socket-os-fd (connection-socket cnn)) :write #'self)))))

(defun/cc send (cnn &rest args)
  (send-bytes cnn (apply #'pack-bytes args)))

;; returns stream
(defun/cc recv (cnn &rest args)
  (recv-recipe cnn (connection-buffer cnn) args))

(defmacro with-recv ((cnn) (&rest vars) &body body)
  `(with-unpack ((recv ,cnn ,(recipe vars)))
       (,@vars)
     ,@body))