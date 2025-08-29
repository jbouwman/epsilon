(defpackage #:epsilon.time
  (:use
   #:cl)
  (:shadow
   #:second)
  (:export
   ;; Time representation
   #:timestamp
   #:duration
   #:make-timestamp
   #:timestamp-seconds
   #:timestamp-nanoseconds
   #:timestamp=
   #:timestamp<
   #:timestamp>
   
   ;; Durations
   #:+nanosecond+
   #:+microsecond+
   #:+millisecond+
   #:+second+
   #:+minute+
   #:+hour+
   #:+day+
   #:make-duration
   #:duration-seconds
   #:duration-nanoseconds
   #:duration+
   #:duration-
   #:duration<
   #:duration>
   #:duration=
   
   ;; Time functions
   #:now
   #:since
   #:until
   #:add-duration
   #:subtract-duration
   
   ;; Formatting
   #:format-timestamp
   #:parse-timestamp
   
   ;; MessagePack compatibility
   #:encode-unix-timestamp
   #:decode-unix-timestamp

   #:format-rfc1123-timestring
   #:format-rfc3339-timestring
   #:universal-to-timestamp
   ))

(in-package #:epsilon.time)

;;; Constants for durations

(defconstant +nanosecond+ 1)
(defconstant +microsecond+ (* 1000 +nanosecond+))
(defconstant +millisecond+ (* 1000 +microsecond+))
(defconstant +second+ (* 1000 +millisecond+))
(defconstant +minute+ (* 60 +second+))
(defconstant +hour+ (* 60 +minute+))
(defconstant +day+ (* 24 +hour+))

;;; Time data structures

(defstruct (timestamp (:constructor %make-timestamp))
  "A timestamp representing a point in time with nanosecond precision."
  (seconds 0 :type integer)
  (nanoseconds 0 :type (integer 0 999999999)))

(defstruct (duration (:constructor %make-duration))
  "A duration representing a span of time with nanosecond precision."
  (seconds 0 :type integer)
  (nanoseconds 0 :type (integer 0 999999999)))

(defun make-timestamp (seconds &optional (nanoseconds 0))
  "Create a timestamp with the given seconds and nanoseconds.
   If nanoseconds >= 1s, it will be normalized."
  (multiple-value-bind (extra-seconds normalized-nanos)
      (floor nanoseconds 1000000000)
    (%make-timestamp :seconds (+ seconds extra-seconds)
                    :nanoseconds normalized-nanos)))

(defun make-duration (seconds &optional (nanoseconds 0))
  "Create a duration with the given seconds and nanoseconds.
   If nanoseconds >= 1s, it will be normalized."
  (multiple-value-bind (extra-seconds normalized-nanos)
      (floor nanoseconds 1000000000)
    (%make-duration :seconds (+ seconds extra-seconds)
                   :nanoseconds normalized-nanos)))

;;; Comparison functions

(defun timestamp= (ts1 ts2)
  "Return T if timestamps are equal."
  (and (= (timestamp-seconds ts1) (timestamp-seconds ts2))
       (= (timestamp-nanoseconds ts1) (timestamp-nanoseconds ts2))))

(defun timestamp< (ts1 ts2)
  "Return T if ts1 is earlier than ts2."
  (or (< (timestamp-seconds ts1) (timestamp-seconds ts2))
      (and (= (timestamp-seconds ts1) (timestamp-seconds ts2))
           (< (timestamp-nanoseconds ts1) (timestamp-nanoseconds ts2)))))

(defun timestamp> (ts1 ts2)
  "Return T if ts1 is later than ts2."
  (or (> (timestamp-seconds ts1) (timestamp-seconds ts2))
      (and (= (timestamp-seconds ts1) (timestamp-seconds ts2))
           (> (timestamp-nanoseconds ts1) (timestamp-nanoseconds ts2)))))

(defun duration= (d1 d2)
  "Return T if durations are equal."
  (and (= (duration-seconds d1) (duration-seconds d2))
       (= (duration-nanoseconds d1) (duration-nanoseconds d2))))

(defun duration< (d1 d2)
  "Return T if d1 is shorter than d2."
  (or (< (duration-seconds d1) (duration-seconds d2))
      (and (= (duration-seconds d1) (duration-seconds d2))
           (< (duration-nanoseconds d1) (duration-nanoseconds d2)))))

(defun duration> (d1 d2)
  "Return T if d1 is longer than d2."
  (or (> (duration-seconds d1) (duration-seconds d2))
      (and (= (duration-seconds d1) (duration-seconds d2))
           (> (duration-nanoseconds d1) (duration-nanoseconds d2)))))

;;; Duration arithmetic

(defun duration+ (d1 d2)
  "Add two durations together."
  (let ((seconds (+ (duration-seconds d1) (duration-seconds d2)))
        (nanoseconds (+ (duration-nanoseconds d1) (duration-nanoseconds d2))))
    (make-duration seconds nanoseconds)))

(defun duration- (d1 d2)
  "Subtract d2 from d1."
  (let ((seconds (- (duration-seconds d1) (duration-seconds d2)))
        (nanoseconds (- (duration-nanoseconds d1) (duration-nanoseconds d2))))
    (if (< nanoseconds 0)
        (make-duration (1- seconds) (+ 1000000000 nanoseconds))
        (make-duration seconds nanoseconds))))

;;; Time operations

(defun now ()
  "Return the current time as a timestamp."
  (multiple-value-bind (seconds nanoseconds)
      (sb-ext:get-time-of-day)
    (make-timestamp seconds (* nanoseconds 1000))))

(defun since (timestamp)
  "Return the duration elapsed since the given timestamp."
  (let ((now (now)))
    (make-duration
     (- (timestamp-seconds now) (timestamp-seconds timestamp))
     (- (timestamp-nanoseconds now) (timestamp-nanoseconds timestamp)))))

(defun until (timestamp)
  "Return the duration until the given timestamp."
  (let ((now (now)))
    (make-duration
     (- (timestamp-seconds timestamp) (timestamp-seconds now))
     (- (timestamp-nanoseconds timestamp) (timestamp-nanoseconds now)))))

(defun add-duration (timestamp duration)
  "Add a duration to a timestamp, returning a new timestamp."
  (make-timestamp
   (+ (timestamp-seconds timestamp) (duration-seconds duration))
   (+ (timestamp-nanoseconds timestamp) (duration-nanoseconds duration))))

(defun subtract-duration (timestamp duration)
  "Subtract a duration from a timestamp, returning a new timestamp."
  (let ((seconds (- (timestamp-seconds timestamp) (duration-seconds duration)))
        (nanoseconds (- (timestamp-nanoseconds timestamp) (duration-nanoseconds duration))))
    (if (< nanoseconds 0)
        (make-timestamp (1- seconds) (+ 1000000000 nanoseconds))
        (make-timestamp seconds nanoseconds))))

;;; Format/parse functions

(defun format-timestamp (timestamp &optional (format :rfc3339))
  "Format a timestamp according to the given format.
   Supported formats: :rfc3339, :unix, :iso8601"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time 
       (+ 2208988800 ; Unix epoch offset (1970-01-01T00:00:00Z) from universal time epoch (1900-01-01T00:00:00Z)
          (timestamp-seconds timestamp))
       0) ; UTC timezone
    (case format
      (:rfc3339
       (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~9,'0DZ"
               year month day hour min sec (timestamp-nanoseconds timestamp)))
      (:unix
       (format nil "~D.~9,'0D" 
               (timestamp-seconds timestamp) (timestamp-nanoseconds timestamp)))
      (:iso8601
       (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
               year month day hour min sec))
      (otherwise
       (error "Unsupported format: ~A" format)))))

(defun parse-timestamp (string &optional (format :rfc3339))
  "Parse a timestamp from a string according to the given format.
   Supported formats: :rfc3339, :unix, :iso8601"
  (case format
    (:unix
     (let* ((dot-pos (position #\. string))
            (seconds (parse-integer string :end (or dot-pos (length string))))
            (nanoseconds (if dot-pos
                            (parse-integer string :start (1+ dot-pos))
                            0)))
       (make-timestamp seconds nanoseconds)))
    (:rfc3339
     ;; Simple RFC3339 parser (not handling all variants)
     (let* ((year (parse-integer string :start 0 :end 4))
            (month (parse-integer string :start 5 :end 7))
            (day (parse-integer string :start 8 :end 10))
            (hour (parse-integer string :start 11 :end 13))
            (minute (parse-integer string :start 14 :end 16))
            (second (parse-integer string :start 17 :end 19))
            (nanoseconds (if (char= (char string 19) #\.)
                             (parse-integer string :start 20 :junk-allowed t)
                             0))
            (universal-time (encode-universal-time 
                             second minute hour day month year 0)))
       (make-timestamp (- universal-time 2208988800) nanoseconds)))
    (:iso8601
     ;; Simple ISO8601 parser
     (let* ((year (parse-integer string :start 0 :end 4))
            (month (parse-integer string :start 5 :end 7))
            (day (parse-integer string :start 8 :end 10))
            (hour (parse-integer string :start 11 :end 13))
            (minute (parse-integer string :start 14 :end 16))
            (second (parse-integer string :start 17 :end 19))
            (universal-time (encode-universal-time 
                            second minute hour day month year 0)))
       (make-timestamp (- universal-time 2208988800) 0)))
    (otherwise
     (error "Unsupported format: ~A" format))))

;;; MessagePack compatibility

(defun encode-unix-timestamp (timestamp &optional include-nanoseconds)
  "Convert a timestamp to a list representation for MessagePack encoding.
   Returns (:timestamp seconds nanoseconds) format."
  (list :timestamp 
        (timestamp-seconds timestamp)
        (if include-nanoseconds (timestamp-nanoseconds timestamp) 0)))

(defun decode-unix-timestamp (timestamp-list)
  "Convert a MessagePack encoded timestamp (as a list) to a timestamp object."
  (assert (eq (first timestamp-list) :timestamp))
  (make-timestamp (cl:second timestamp-list) 
                 (if (>= (length timestamp-list) 3)
                     (third timestamp-list)
                     0)))
