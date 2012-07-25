(in-package #:protocol)

;; arbitrary, really, but traditional
(defconstant +page-size+ 4096)

(defclass recv-buf ()
  ((buffer
    :reader recv-buffer
    :type (simple-array 'octet))
   (read
    :accessor recv-read
    :initform 0
    :type fixnum)
   (write
    :accessor recv-write
    :initform 0
    :type fixnum)
   (expected
    :accessor recv-expected
    :initform 0
    :type fixnum)))

(defmethod initialize-instance :after ((recv-buf recv-buf) &key (size (* 2 +page-size+)) &allow-other-keys)
  (setf (slot-value recv-buf 'buffer) (make-array size :element-type 'octet :adjustable t)))

(defun recv-length (recv-buf)
  (the fixnum (length (recv-buffer recv-buf))))

(defun recv-available (recv-buf)
  (the fixnum (- (recv-write recv-buf) (recv-read recv-buf))))

(defun eat (recv-buf &optional length sharedp)
  (with-slots (buffer read write)
      recv-buf
    (let* ((length (or length (- write read)))
           (result (if sharedp
                       (make-array length :element-type 'octet :displaced-to buffer :displaced-index-offset read)
                       (replace (make-array length :element-type 'octet) buffer :start2 read))))
      (incf read length)
      result)))

(defun eat-stream (recv-buf &optional length sharedp)
  (babel-streams:make-in-memory-input-stream (eat recv-buf length sharedp)))

(defun eat-to-expected (recv-buf &optional sharedp)
  (prog1
      (eat recv-buf (recv-expected recv-buf) sharedp)
    (setf (recv-expected recv-buf) 0)))

(defun stream-to-expected (recv-buf &optional sharedp)
  (babel-streams:make-in-memory-input-stream (eat-to-expected recv-buf sharedp)))

(defun eat-to-delimiter (recv-buf delimiter)
  (declare (octets delimiter))
  (when-let ((pos (position-of-delimiter recv-buf delimiter)))
    (eat recv-buf (+ pos (length delimiter)))))

(defun stream-to-delimiter (recv-buf delimiter)
  (babel-streams:make-in-memory-input-stream (eat-to-delimiter recv-buf delimiter)))

(defun position-of-delimiter (recv-buf delimiter)
  (declare (octets delimiter))
  (with-slots (buffer read write)
      recv-buf
    (when (>= (- write read) (length delimiter))
      (loop
         with end = (1+ (- write (length delimiter)))
         for start = read then (1+ pos)
         for pos = (position (aref delimiter 0) buffer :start start :end end)
         while pos
         thereis (and (loop
                         for d across delimiter
                         for p from pos
                         always (= d (aref buffer p)))
                      (- pos read))))))

(defun ensure-space (recv-buf &optional (num-bytes +page-size+))
  (with-slots (buffer read write)
      recv-buf
    ;; shift down
    (when (and (plusp read)
               (< (- (length buffer) write) num-bytes))
      (loop
         for from from read below write
         for to from 0
         do (setf (aref buffer to) (aref buffer from)))
      (psetf read 0
             write (- write read)))
    ;; grow
    (when (< (- (length buffer) write) num-bytes)
      (adjust-array buffer (+ (length buffer) num-bytes)))
    recv-buf))
