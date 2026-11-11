(in-package #:mcclim-browser)

;; Standalone input streams have queues but no window-system port. Upstream's
;; single-threaded queue methods unconditionally poll that port, including NIL.
;; Concurrent queues keep their upstream synchronization. Browser queues also
;; finish already-dispatched events before polling the next native event: focus
;; notifications must run before a following key is routed to its input sheet.
(defun portless-queue-p (queue)
  (and (null (climi::queue-port queue))
       (let ((concurrent (find-class 'climi::concurrent-queue nil)))
         (not (and concurrent (typep queue concurrent))))))

(defun browser-queue-ready-p (queue)
  (let ((browser (find-class 'browser-port nil))
        (concurrent (find-class 'climi::concurrent-queue nil)))
    (when (and browser (typep (climi::queue-port queue) browser)
               (not (and concurrent (typep queue concurrent)))
               (climi::queue-head queue))
      (climi::do-port-force-output queue)
      (climi::check-schedule queue)
      t)))

(defun update-portless-queue (queue)
  (loop for alarm = (climi::event-schedule-time queue)
        while (and alarm (<= alarm (climi::now)))
        do (climi::check-schedule queue)))

(defun wait-for-portless-queue (queue timeout wait-function)
  (let ((deadline (when timeout (+ (climi::now) timeout))))
    (loop
      (update-portless-queue queue)
      (when (and wait-function (funcall wait-function))
        (return (values nil :wait-function)))
      (when (climi::queue-head queue) (return t))
      (when (and deadline (>= (climi::now) deadline))
        (return (values nil :timeout)))
      (sleep (min .01 (or (climi::compute-decay
                          deadline (climi::event-schedule-time queue)) .01))))))

(defmethod climi::queue-listen-or-wait :around ((queue climi::simple-queue) timeout wait-function)
  (cond ((portless-queue-p queue)
         (wait-for-portless-queue queue timeout wait-function))
        ((browser-queue-ready-p queue)
         (if (and wait-function (funcall wait-function))
             (values nil :wait-function) t))
        (t (call-next-method))))

(defmethod climi::queue-read-or-wait :around ((queue climi::simple-queue) timeout wait-function)
  (cond ((portless-queue-p queue)
         (multiple-value-bind (available reason)
             (wait-for-portless-queue queue timeout wait-function)
           (if available (climi::%queue-read queue) (values nil reason))))
        ((browser-queue-ready-p queue)
         (if (and wait-function (funcall wait-function))
             (values nil :wait-function) (climi::%queue-read queue)))
        (t (call-next-method))))

(defmethod climi::queue-read :around ((queue climi::simple-queue))
  (cond ((portless-queue-p queue) (climi::queue-read-or-wait queue nil nil))
        ((browser-queue-ready-p queue) (climi::%queue-read queue))
        (t (call-next-method))))

(defmethod climi::queue-read-no-hang :around ((queue climi::simple-queue))
  (cond ((portless-queue-p queue)
         (update-portless-queue queue) (climi::%queue-read queue))
        ((browser-queue-ready-p queue) (climi::%queue-read queue))
        (t (call-next-method))))

(defmethod climi::queue-peek :around ((queue climi::simple-queue))
  (cond ((portless-queue-p queue)
         (update-portless-queue queue) (first (climi::queue-head queue)))
        ((browser-queue-ready-p queue) (first (climi::queue-head queue)))
        (t (call-next-method))))

(defmethod climi::queue-peek-if :around (predicate (queue climi::simple-queue))
  (cond ((portless-queue-p queue)
         (update-portless-queue queue) (find-if predicate (climi::queue-head queue)))
        ((browser-queue-ready-p queue) (find-if predicate (climi::queue-head queue)))
        (t (call-next-method))))

(defmethod climi::queue-listen :around ((queue climi::simple-queue))
  (cond ((portless-queue-p queue)
         (update-portless-queue queue) (not (null (climi::queue-head queue))))
        ((browser-queue-ready-p queue) t)
        (t (call-next-method))))
