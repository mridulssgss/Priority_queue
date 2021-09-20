;;;to implement priority queue

(defpackage :priority-queue
  (:use
   :cl
   :cl-user)
  (:export
   :make-priority-queue
   :insert
   :remove-top
   :update
   :top))

(in-package :priority-queue)


(defclass priority-queue ()
  ((values
    :initform nil
    :initarg :values) ;contain object and its position in heap array
   (heap
    :initarg :heap)
   (heap-size
    :initarg :heap-size)
   (key
    :initarg :key)))


(defgeneric heapify (heap hash p &key n key)
  (:documentation "to heapify vector"))

(defmethod heapify ((heap vector) hash p &key (n (length heap)) (key (lambda (x) x)))
  "to heapify vector based on key"
  (labels ((min-index (a b)
             (if (< (funcall key (aref heap a)) (funcall key (aref heap b))) a b)))

    (let* ((lchild (if (< (+ (* 2 p) 1) n) (+ (* 2 p) 1) p))
           (rchild (if (< (+ (* 2 p) 2) n) (+ (* 2 p) 2) p))
           (mchild (min-index lchild (min-index rchild p))))

      (cond ((not (= mchild p))
             (rotatef (aref heap p) (aref heap mchild))
             (rotatef (gethash (aref heap p) hash) (gethash (aref heap mchild) hash))
             (heapify heap hash mchild :n n :key key)))
      heap)))

(defgeneric build-heap (heap hash &key n key)
  (:documentation "to build heap"))


(defmethod build-heap ((heap vector) hash &key (n (length heap)) (key (lambda (x) x)))
  (loop for i from (1- (ceiling (/ n 2))) downto 0 do
        (heapify heap hash i :n n :key key))
  heap)


(defun make-priority-queue (element &key (key (lambda (x) x)) (n (length element)))
  (let ((hash (make-hash-table :test 'equal))
        (heap (make-array n :adjustable t :initial-contents element :fill-pointer n))
        (fun (lambda (el) (funcall key el))))
    (loop for i from 0 below n do
          (setf (gethash (aref element i) hash) i))
    (build-heap heap hash :n n :key fun)  ;build a heap with value
    (make-instance 'priority-queue :values hash :heap heap :key key :heap-size n)))

(defgeneric top (queue)
  (:documentation "returns the top element of the queue"))

(defmethod top ((queue priority-queue))
  "returns the top element of the queue"
  (with-slots (values heap heap-size) queue
    (aref heap 0)))

(defgeneric remove-top (queue)
  (:documentation "removes the top element and then takes it"))


(defmethod remove-top ((queue priority-queue))
  "removes the top element of the queue"
  (with-slots (values heap heap-size key) queue
    (remhash (aref heap 0) values)
    (setf (aref heap 0) (aref heap (1- heap-size)))
    (decf (fill-pointer heap))
    (decf heap-size)
    (heapify heap values 0 :key key :n heap-size)))


(defgeneric empty? (queue)
  (:documentation "checks whether the queue is empty"))


(defmethod empty? ((queue priority-queue))
  (= (slot-value queue 'heap-size) 0))


(defgeneric update (queue element)
  (:documentation "update the element value in the queue with new-value and fixes the rep invariant"))


(defmethod update (queue element)
  "updates an puts the index i in the right position"
  (with-slots (values heap heap-size key) queue
    (let ((i (gethash element values nil)))
      (when i
        (let ((parent (1- (ceiling (/ i 2)))))
          (cond ((and (>= parent 0) (> (funcall key (aref heap parent))
                                       (funcall key (aref heap i))))
                 (rotatef (aref heap i) (aref heap parent))
                 (rotatef (gethash (aref heap i) values) (gethash (aref heap parent) values))
                 (update queue (aref heap i)))
                (t
                 (heapify heap values i :key key :n heap-size))))))))

(defgeneric insert (queue element)
  (:documentation "sets the value and update the node"))

(defmethod insert ((queue priority-queue) element)
  "sets the value of the priority queue and then fixes the heap"
  (with-slots (values heap heap-size key) queue
    (cond ((null (gethash element values))
           (setf (gethash element values) heap-size)
           (incf heap-size)
           (vector-push-extend element heap)
           (update queue element))
          (t
           (error "element already exist")))))



