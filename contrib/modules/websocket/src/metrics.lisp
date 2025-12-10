;;;; WebSocket Metrics Collection
;;;;
;;;; This module provides metrics collection and monitoring for WebSocket connections.

(defpackage epsilon.websocket.metrics
  (:use
   cl
   epsilon.syntax)
  (:local-nicknames
   (lock epsilon.sys.lock)
   (map epsilon.map))
  (:export
   ;; Metrics collector
   metrics-collector
   make-metrics-collector
   metrics-collector-total-connections
   metrics-collector-active-connections
   metrics-collector-messages-sent
   metrics-collector-messages-received
   metrics-collector-bytes-sent
   metrics-collector-bytes-received
   metrics-collector-frames-sent
   metrics-collector-frames-received
   metrics-collector-error-count
   metrics-collector-errors-by-type
   
   ;; Recording functions
   record-connection-opened
   record-connection-closed
   record-message-sent
   record-message-received
   record-error
   record-frame-sent
   record-frame-received
   
   ;; Metrics operations
   reset-metrics
   get-metrics-snapshot
   format-metrics))

(in-package epsilon.websocket.metrics)

;;; Metrics collector structure

(defstruct metrics-collector
  "WebSocket metrics collector"
  (total-connections 0 :type integer)
  (active-connections 0 :type integer)
  (messages-sent 0 :type integer)
  (messages-received 0 :type integer)
  (bytes-sent 0 :type integer)
  (bytes-received 0 :type integer)
  (frames-sent 0 :type integer)
  (frames-received 0 :type integer)
  (error-count 0 :type integer)
  (errors-by-type (map:make-map) :type map:hamt)
  (start-time (get-universal-time) :type integer)
  (lock (lock:make-lock) :type lock:lock))

;;; Recording functions

(defun record-connection-opened (collector)
  "Record a new connection being opened"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-total-connections collector))
    (incf (metrics-collector-active-connections collector))))

(defun record-connection-closed (collector)
  "Record a connection being closed"
  (lock:with-lock ((metrics-collector-lock collector))
    (decf (metrics-collector-active-connections collector))))

(defun record-message-sent (collector type size)
  "Record a message being sent"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-messages-sent collector))
    (incf (metrics-collector-bytes-sent collector) size)))

(defun record-message-received (collector type size)
  "Record a message being received"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-messages-received collector))
    (incf (metrics-collector-bytes-received collector) size)))

(defun record-frame-sent (collector)
  "Record a frame being sent"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-frames-sent collector))))

(defun record-frame-received (collector)
  "Record a frame being received"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-frames-received collector))))

(defun record-error (collector error-type)
  "Record an error occurrence"
  (lock:with-lock ((metrics-collector-lock collector))
    (incf (metrics-collector-error-count collector))
    (let ((current-count (or (map:get (metrics-collector-errors-by-type collector)
                                      error-type)
                             0)))
      (setf (metrics-collector-errors-by-type collector)
            (map:assoc (metrics-collector-errors-by-type collector)
                      error-type
                      (1+ current-count))))))

;;; Metrics operations

(defun reset-metrics (collector)
  "Reset all metrics to zero"
  (lock:with-lock ((metrics-collector-lock collector))
    (setf (metrics-collector-total-connections collector) 0
          (metrics-collector-active-connections collector) 0
          (metrics-collector-messages-sent collector) 0
          (metrics-collector-messages-received collector) 0
          (metrics-collector-bytes-sent collector) 0
          (metrics-collector-bytes-received collector) 0
          (metrics-collector-frames-sent collector) 0
          (metrics-collector-frames-received collector) 0
          (metrics-collector-error-count collector) 0
          (metrics-collector-errors-by-type collector) (map:make-map)
          (metrics-collector-start-time collector) (get-universal-time))))

(defun get-metrics-snapshot (collector)
  "Get a snapshot of current metrics"
  (lock:with-lock ((metrics-collector-lock collector))
    (let ((uptime (- (get-universal-time) (metrics-collector-start-time collector))))
      (map:from-pairs
       `(("total_connections" . ,(metrics-collector-total-connections collector))
         ("active_connections" . ,(metrics-collector-active-connections collector))
         ("messages_sent" . ,(metrics-collector-messages-sent collector))
         ("messages_received" . ,(metrics-collector-messages-received collector))
         ("bytes_sent" . ,(metrics-collector-bytes-sent collector))
         ("bytes_received" . ,(metrics-collector-bytes-received collector))
         ("frames_sent" . ,(metrics-collector-frames-sent collector))
         ("frames_received" . ,(metrics-collector-frames-received collector))
         ("error_count" . ,(metrics-collector-error-count collector))
         ("errors_by_type" . ,(metrics-collector-errors-by-type collector))
         ("uptime_seconds" . ,uptime)
         ("messages_per_second" . ,(if (zerop uptime)
                                       0
                                       (/ (+ (metrics-collector-messages-sent collector)
                                            (metrics-collector-messages-received collector))
                                         uptime)))
         ("bytes_per_second" . ,(if (zerop uptime)
                                    0
                                    (/ (+ (metrics-collector-bytes-sent collector)
                                         (metrics-collector-bytes-received collector))
                                      uptime))))))))

(defun format-metrics (collector &optional (stream t))
  "Format metrics for display"
  (let ((snapshot (get-metrics-snapshot collector)))
    (format stream "WebSocket Metrics:~%")
    (format stream "==================~%")
    (format stream "Connections:~%")
    (format stream "  Total: ~D~%" (map:get snapshot "total_connections"))
    (format stream "  Active: ~D~%" (map:get snapshot "active_connections"))
    (format stream "~%Messages:~%")
    (format stream "  Sent: ~D~%" (map:get snapshot "messages_sent"))
    (format stream "  Received: ~D~%" (map:get snapshot "messages_received"))
    (format stream "  Rate: ~,2F/sec~%" (map:get snapshot "messages_per_second"))
    (format stream "~%Data Transfer:~%")
    (format stream "  Sent: ~D bytes~%" (map:get snapshot "bytes_sent"))
    (format stream "  Received: ~D bytes~%" (map:get snapshot "bytes_received"))
    (format stream "  Rate: ~,2F bytes/sec~%" (map:get snapshot "bytes_per_second"))
    (format stream "~%Frames:~%")
    (format stream "  Sent: ~D~%" (map:get snapshot "frames_sent"))
    (format stream "  Received: ~D~%" (map:get snapshot "frames_received"))
    (format stream "~%Errors:~%")
    (format stream "  Total: ~D~%" (map:get snapshot "error_count"))
    (when (> (map:get snapshot "error_count") 0)
      (format stream "  By Type:~%")
      (map:each (lambda (type count)
                  (format stream "    ~A: ~D~%" type count))
                (map:get snapshot "errors_by_type")))
    (format stream "~%Uptime: ~D seconds~%" (map:get snapshot "uptime_seconds"))))