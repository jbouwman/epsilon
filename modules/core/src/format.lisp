;;;; Human-readable formatting utilities
;;;;
;;;; This module provides functions for formatting numbers, times, and sizes
;;;; in human-readable forms with appropriate units and precision.

(defpackage :epsilon.format
  (:use :cl)
  (:export
   ;; Duration formatting
   #:format-duration
   #:format-time-duration
   
   ;; Size formatting
   #:format-bytes
   #:format-memory-size
   
   ;; Number formatting
   #:format-count
   #:format-throughput
   #:format-rate
   
   ;; Precision control
   #:with-precision))

(in-package :epsilon.format)

;;; Duration formatting

(defun format-duration (seconds &key (precision 2))
  "Format a duration in seconds to a human-readable string with appropriate units.
   PRECISION controls the number of decimal places (default 2)."
  (cond
    ;; Picoseconds
    ((< seconds 1e-9) 
     (format nil "~,vF ps" precision (* seconds 1e12)))
    ;; Nanoseconds
    ((< seconds 1e-6) 
     (format nil "~,vF ns" precision (* seconds 1e9)))
    ;; Microseconds
    ((< seconds 1e-3) 
     (format nil "~,vF µs" precision (* seconds 1e6)))
    ;; Milliseconds
    ((< seconds 1.0) 
     (format nil "~,vF ms" precision (* seconds 1e3)))
    ;; Seconds
    ((< seconds 60) 
     (format nil "~,vF s" precision seconds))
    ;; Minutes
    ((< seconds 3600)
     (multiple-value-bind (minutes remaining-seconds)
         (floor seconds 60)
       (if (zerop remaining-seconds)
           (format nil "~D min" minutes)
           (format nil "~D min ~,vF s" minutes precision remaining-seconds))))
    ;; Hours
    ((< seconds 86400)
     (multiple-value-bind (hours remaining-seconds)
         (floor seconds 3600)
       (let ((minutes (floor remaining-seconds 60)))
         (if (zerop minutes)
             (format nil "~D h" hours)
             (format nil "~D h ~D min" hours minutes)))))
    ;; Days
    (t
     (multiple-value-bind (days remaining-seconds)
         (floor seconds 86400)
       (let ((hours (floor remaining-seconds 3600)))
         (if (zerop hours)
             (format nil "~D d" days)
             (format nil "~D d ~D h" days hours)))))))

(defun format-time-duration (seconds)
  "Alias for format-duration with default precision."
  (format-duration seconds))

;;; Size formatting

(defun format-bytes (bytes &key (precision 1) (binary t))
  "Format byte count to human-readable string.
   If BINARY is true (default), use binary units (KiB, MiB, GiB).
   Otherwise use decimal units (KB, MB, GB)."
  (let ((divisor (if binary 1024.0 1000.0))
        (units (if binary
                   #("B" "KiB" "MiB" "GiB" "TiB" "PiB")
                   #("B" "KB" "MB" "GB" "TB" "PB"))))
    (loop for unit across units
          for size = (float bytes) then (/ size divisor)
          for i from 0
          when (or (< size divisor) (= i (1- (length units))))
            return (if (zerop i)
                      (format nil "~D ~A" (round size) unit)
                      (format nil "~,vF ~A" precision size unit)))))

(defun format-memory-size (bytes)
  "Alias for format-bytes with binary units."
  (format-bytes bytes :binary t))

;;; Number formatting

(defun format-count (number &key (precision 2))
  "Format large numbers with appropriate suffixes (K, M, B, T)."
  (cond
    ((< number 1000)
     (format nil "~D" (round number)))
    ((< number 1e6)
     (format nil "~,vFK" precision (/ number 1e3)))
    ((< number 1e9)
     (format nil "~,vFM" precision (/ number 1e6)))
    ((< number 1e12)
     (format nil "~,vFB" precision (/ number 1e9)))
    (t
     (format nil "~,vFT" precision (/ number 1e12)))))

(defun format-throughput (operations-per-second &key (precision 2))
  "Format throughput/rate with appropriate units."
  (cond
    ((< operations-per-second 1)
     (format nil "~,vF ops/sec" (+ precision 2) operations-per-second))
    ((< operations-per-second 1e3)
     (format nil "~,vF ops/sec" precision operations-per-second))
    ((< operations-per-second 1e6)
     (format nil "~,vF K ops/sec" precision (/ operations-per-second 1e3)))
    ((< operations-per-second 1e9)
     (format nil "~,vF M ops/sec" precision (/ operations-per-second 1e6)))
    ((< operations-per-second 1e12)
     (format nil "~,vF G ops/sec" precision (/ operations-per-second 1e9)))
    (t
     (format nil "~,vF T ops/sec" precision (/ operations-per-second 1e12)))))

(defun format-rate (value unit &key (precision 2))
  "Format a rate with appropriate scaling.
   E.g., (format-rate 5000 \"req/s\") => \"5.00 K req/s\""
  (let ((formatted-value (format-count value :precision precision)))
    (if (search " " formatted-value)
        formatted-value
        (format nil "~A ~A" formatted-value unit))))

;;; Precision control

(defmacro with-precision ((precision) &body body)
  "Execute body with specified default precision for formatting functions."
  `(let ((*print-precision* ,precision))
     ,@body))

;;; Helper for dynamic precision
(defvar *print-precision* 2
  "Default precision for formatting functions.")