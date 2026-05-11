;;;; epsilon.time tests
;;;;
;;;; Tests for timestamp and duration types: creation, normalization,
;;;; comparison, arithmetic, and format/parse round-trips.

(defpackage epsilon.time-tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.time time)))

;;; Timestamp creation and normalization

(deftest make-timestamp-basic
  "Basic timestamp creation"
  (let ((ts (time:make-timestamp 100 500)))
    (assert-= 100 (time:timestamp-seconds ts))
    (assert-= 500 (time:timestamp-nanoseconds ts))))

(deftest make-timestamp-normalizes-nanoseconds
  "Nanoseconds overflow normalizes into seconds"
  (let ((ts (time:make-timestamp 0 1500000000)))
    (assert-= 1 (time:timestamp-seconds ts))
    (assert-= 500000000 (time:timestamp-nanoseconds ts))))

(deftest make-timestamp-normalizes-large-nanoseconds
  "Large nanosecond values normalize correctly"
  (let ((ts (time:make-timestamp 0 3000000000)))
    (assert-= 3 (time:timestamp-seconds ts))
    (assert-= 0 (time:timestamp-nanoseconds ts))))

(deftest make-timestamp-zero
  "Zero timestamp"
  (let ((ts (time:make-timestamp 0 0)))
    (assert-= 0 (time:timestamp-seconds ts))
    (assert-= 0 (time:timestamp-nanoseconds ts))))

;;; Duration creation and normalization

(deftest make-duration-basic
  "Basic duration creation"
  (let ((d (time:make-duration 5 100)))
    (assert-= 5 (time:duration-seconds d))
    (assert-= 100 (time:duration-nanoseconds d))))

(deftest make-duration-normalizes
  "Duration nanoseconds overflow normalizes"
  (let ((d (time:make-duration 0 2500000000)))
    (assert-= 2 (time:duration-seconds d))
    (assert-= 500000000 (time:duration-nanoseconds d))))

;;; Duration constants

(deftest duration-constants
  "Duration unit constants have correct values"
  (assert-= 1 time:+nanosecond+)
  (assert-= 1000 time:+microsecond+)
  (assert-= 1000000 time:+millisecond+)
  (assert-= 1000000000 time:+second+)
  (assert-= (* 60 1000000000) time:+minute+)
  (assert-= (* 3600 1000000000) time:+hour+)
  (assert-= (* 86400 1000000000) time:+day+))

;;; Timestamp comparison

(deftest timestamp-equal
  "timestamp= with equal and unequal timestamps"
  (let ((ts1 (time:make-timestamp 100 500))
        (ts2 (time:make-timestamp 100 500))
        (ts3 (time:make-timestamp 100 501)))
    (assert-true (time:timestamp= ts1 ts2))
    (assert-not (time:timestamp= ts1 ts3))))

(deftest timestamp-less-than
  "timestamp< compares correctly"
  (let ((early (time:make-timestamp 100 0))
        (late (time:make-timestamp 101 0))
        (late-ns (time:make-timestamp 100 1)))
    (assert-true (time:timestamp< early late))
    (assert-true (time:timestamp< early late-ns))
    (assert-not (time:timestamp< late early))
    (assert-not (time:timestamp< early early))))

(deftest timestamp-greater-than
  "timestamp> compares correctly"
  (let ((early (time:make-timestamp 100 0))
        (late (time:make-timestamp 101 0)))
    (assert-true (time:timestamp> late early))
    (assert-not (time:timestamp> early late))))

;;; Duration comparison

(deftest duration-equal
  "duration= with equal and unequal durations"
  (let ((d1 (time:make-duration 5 100))
        (d2 (time:make-duration 5 100))
        (d3 (time:make-duration 5 200)))
    (assert-true (time:duration= d1 d2))
    (assert-not (time:duration= d1 d3))))

(deftest duration-less-than
  "duration< compares correctly"
  (let ((short (time:make-duration 1 0))
        (long (time:make-duration 2 0)))
    (assert-true (time:duration< short long))
    (assert-not (time:duration< long short))))

(deftest duration-greater-than
  "duration> compares correctly"
  (let ((short (time:make-duration 1 0))
        (long (time:make-duration 2 0)))
    (assert-true (time:duration> long short))
    (assert-not (time:duration> short long))))

;;; Duration arithmetic

(deftest duration-addition
  "duration+ adds two durations"
  (let* ((d1 (time:make-duration 1 500000000))
         (d2 (time:make-duration 2 700000000))
         (sum (time:duration+ d1 d2)))
    (assert-= 4 (time:duration-seconds sum))
    (assert-= 200000000 (time:duration-nanoseconds sum))))

(deftest duration-subtraction
  "duration- subtracts with borrow"
  (let* ((d1 (time:make-duration 3 100000000))
         (d2 (time:make-duration 1 500000000))
         (diff (time:duration- d1 d2)))
    (assert-= 1 (time:duration-seconds diff))
    (assert-= 600000000 (time:duration-nanoseconds diff))))

(deftest duration-subtraction-no-borrow
  "duration- without nanosecond borrow"
  (let* ((d1 (time:make-duration 5 800000000))
         (d2 (time:make-duration 2 300000000))
         (diff (time:duration- d1 d2)))
    (assert-= 3 (time:duration-seconds diff))
    (assert-= 500000000 (time:duration-nanoseconds diff))))

;;; Timestamp + Duration arithmetic

(deftest add-duration-to-timestamp
  "add-duration adds duration to timestamp"
  (let* ((ts (time:make-timestamp 100 500000000))
         (d (time:make-duration 1 600000000))
         (result (time:add-duration ts d)))
    (assert-= 102 (time:timestamp-seconds result))
    (assert-= 100000000 (time:timestamp-nanoseconds result))))

(deftest subtract-duration-from-timestamp
  "subtract-duration with nanosecond borrow"
  (let* ((ts (time:make-timestamp 100 200000000))
         (d (time:make-duration 1 500000000))
         (result (time:subtract-duration ts d)))
    (assert-= 98 (time:timestamp-seconds result))
    (assert-= 700000000 (time:timestamp-nanoseconds result))))

;;; Format and parse

(deftest format-timestamp-unix
  "format-timestamp with :unix format"
  (let* ((ts (time:make-timestamp 1234567890 123456789))
         (s (time:format-timestamp ts :unix)))
    (assert-equal "1234567890.123456789" s)))

(deftest parse-timestamp-unix
  "parse-timestamp with :unix format"
  (let ((ts (time:parse-timestamp "1234567890.123456789" :unix)))
    (assert-= 1234567890 (time:timestamp-seconds ts))
    (assert-= 123456789 (time:timestamp-nanoseconds ts))))

(deftest format-parse-roundtrip-unix
  "Format then parse with :unix preserves timestamp"
  (let* ((original (time:make-timestamp 1700000000 999999999))
         (formatted (time:format-timestamp original :unix))
         (parsed (time:parse-timestamp formatted :unix)))
    (assert-true (time:timestamp= original parsed))))

;;; MessagePack compatibility

(deftest encode-unix-timestamp
  "encode-unix-timestamp produces expected list"
  (let* ((ts (time:make-timestamp 1000 500))
         (encoded (time:encode-unix-timestamp ts t)))
    (assert-eq :timestamp (first encoded))
    (assert-= 1000 (second encoded))
    (assert-= 500 (third encoded))))

(deftest decode-unix-timestamp
  "decode-unix-timestamp reconstructs timestamp"
  (let ((ts (time:decode-unix-timestamp '(:timestamp 1000 500))))
    (assert-= 1000 (time:timestamp-seconds ts))
    (assert-= 500 (time:timestamp-nanoseconds ts))))

(deftest encode-decode-unix-timestamp-roundtrip
  "encode then decode unix timestamp preserves values"
  (let* ((original (time:make-timestamp 99999 123456789))
         (encoded (time:encode-unix-timestamp original t))
         (decoded (time:decode-unix-timestamp encoded)))
    (assert-true (time:timestamp= original decoded))))

;;; now/since/until monotonicity

(deftest now-returns-timestamp
  "now returns a timestamp struct"
  (let ((ts (time:now)))
    (assert-true (typep ts 'time:timestamp))
    (assert-true (> (time:timestamp-seconds ts) 0))))
