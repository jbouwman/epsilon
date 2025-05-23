(defpackage #:epsilon.net.dns
  (:use #:cl)
  (:export
   ;; Structures
   #:dns-response #:dns-record #:dns-question #:dns-header
   
   ;; Accessors
   #:response-header #:response-questions #:response-answers 
   #:response-authority #:response-additional
   #:header-id #:header-flags #:header-qd-count #:header-an-count
   #:header-ns-count #:header-ar-count
   #:question-name #:question-type #:question-class
   #:record-name #:record-type #:record-class #:record-ttl #:record-data
   
   ;; Record types
   #:a-record #:aaaa-record #:cname-record #:mx-record #:ns-record
   #:ptr-record #:txt-record #:soa-record #:srv-record
   
   ;; Constructors
   #:make-dns-response #:make-dns-record #:make-dns-question #:make-dns-header
   
   ;; Query flags
   #:qr-query #:qr-response #:aa-flag #:tc-flag #:rd-flag #:ra-flag
   
   ;; Response codes
   #:rcode-no-error #:rcode-format-error #:rcode-server-failure
   #:rcode-name-error #:rcode-not-implemented #:rcode-refused))

(in-package :epsilon.net.dns)

;;; DNS Record Types

(defconstant +a-record+ 1)
(defconstant +ns-record+ 2)
(defconstant +cname-record+ 5)
(defconstant +soa-record+ 6)
(defconstant +ptr-record+ 12)
(defconstant +mx-record+ 15)
(defconstant +txt-record+ 16)
(defconstant +aaaa-record+ 28)
(defconstant +srv-record+ 33)

;;; DNS Classes

(defconstant +in-class+ 1)

;;; Query Flags

(defconstant +qr-query+ 0)
(defconstant +qr-response+ 1)
(defconstant +aa-flag+ #x0400)
(defconstant +tc-flag+ #x0200)
(defconstant +rd-flag+ #x0100)
(defconstant +ra-flag+ #x0080)

;;; Response Codes

(defconstant +rcode-no-error+ 0)
(defconstant +rcode-format-error+ 1)
(defconstant +rcode-server-failure+ 2)
(defconstant +rcode-name-error+ 3)
(defconstant +rcode-not-implemented+ 4)
(defconstant +rcode-refused+ 5)

;;; Exported constants

(defconstant a-record +a-record+)
(defconstant aaaa-record +aaaa-record+)
(defconstant cname-record +cname-record+)
(defconstant mx-record +mx-record+)
(defconstant ns-record +ns-record+)
(defconstant ptr-record +ptr-record+)
(defconstant txt-record +txt-record+)
(defconstant soa-record +soa-record+)
(defconstant srv-record +srv-record+)

(defconstant qr-query +qr-query+)
(defconstant qr-response +qr-response+)
(defconstant aa-flag +aa-flag+)
(defconstant tc-flag +tc-flag+)
(defconstant rd-flag +rd-flag+)
(defconstant ra-flag +ra-flag+)

(defconstant rcode-no-error +rcode-no-error+)
(defconstant rcode-format-error +rcode-format-error+)
(defconstant rcode-server-failure +rcode-server-failure+)
(defconstant rcode-name-error +rcode-name-error+)
(defconstant rcode-not-implemented +rcode-not-implemented+)
(defconstant rcode-refused +rcode-refused+)

;;; DNS Header Structure

(defstruct dns-header
  (id 0 :type (unsigned-byte 16))
  (flags 0 :type (unsigned-byte 16))
  (qd-count 0 :type (unsigned-byte 16))
  (an-count 0 :type (unsigned-byte 16))
  (ns-count 0 :type (unsigned-byte 16))
  (ar-count 0 :type (unsigned-byte 16)))

;;; DNS Question Structure

(defstruct dns-question
  (name "" :type string)
  (type +a-record+ :type (unsigned-byte 16))
  (class +in-class+ :type (unsigned-byte 16)))

;;; DNS Record Structure

(defstruct dns-record
  (name "" :type string)
  (type +a-record+ :type (unsigned-byte 16))
  (class +in-class+ :type (unsigned-byte 16))
  (ttl 0 :type (unsigned-byte 32))
  (data nil))

;;; DNS Response Structure

(defstruct dns-response
  (header (make-dns-header) :type dns-header)
  (questions nil :type list)
  (answers nil :type list)
  (authority nil :type list)
  (additional nil :type list))

;;; Header Flag Utilities

(defun response-p (header)
  "Check if this is a response (QR bit set)."
  (logbitp 15 (dns-header-flags header)))

(defun authoritative-p (header)
  "Check if this is an authoritative answer (AA bit set)."
  (logbitp 10 (dns-header-flags header)))

(defun truncated-p (header)
  "Check if the message was truncated (TC bit set)."
  (logbitp 9 (dns-header-flags header)))

(defun recursion-desired-p (header)
  "Check if recursion is desired (RD bit set)."
  (logbitp 8 (dns-header-flags header)))

(defun recursion-available-p (header)
  "Check if recursion is available (RA bit set)."
  (logbitp 7 (dns-header-flags header)))

(defun response-code (header)
  "Extract the response code from the header flags."
  (logand (dns-header-flags header) #x000F))

;;; Record Type Predicates

(defun a-record-p (record)
  "Check if this is an A record."
  (= (dns-record-type record) +a-record+))

(defun aaaa-record-p (record)
  "Check if this is an AAAA record."
  (= (dns-record-type record) +aaaa-record+))

(defun cname-record-p (record)
  "Check if this is a CNAME record."
  (= (dns-record-type record) +cname-record+))

(defun mx-record-p (record)
  "Check if this is an MX record."
  (= (dns-record-type record) +mx-record+))

(defun ns-record-p (record)
  "Check if this is an NS record."
  (= (dns-record-type record) +ns-record+))

(defun ptr-record-p (record)
  "Check if this is a PTR record."
  (= (dns-record-type record) +ptr-record+))

(defun txt-record-p (record)
  "Check if this is a TXT record."
  (= (dns-record-type record) +txt-record+))

(defun soa-record-p (record)
  "Check if this is an SOA record."
  (= (dns-record-type record) +soa-record+))

(defun srv-record-p (record)
  "Check if this is an SRV record."
  (= (dns-record-type record) +srv-record+))

;;; Response Analysis

(defun successful-response-p (response)
  "Check if the DNS response indicates success."
  (= (response-code (dns-response-header response)) +rcode-no-error+))

(defun find-records-by-type (response record-type)
  "Find all records of a specific type in the response."
  (remove-if-not (lambda (record)
                   (= (dns-record-type record) record-type))
                 (dns-response-answers response)))

(defun find-a-records (response)
  "Find all A records in the response."
  (find-records-by-type response +a-record+))

(defun find-aaaa-records (response)
  "Find all AAAA records in the response."
  (find-records-by-type response +aaaa-record+))

(defun find-cname-records (response)
  "Find all CNAME records in the response."
  (find-records-by-type response +cname-record+))

(defun find-mx-records (response)
  "Find all MX records in the response."
  (find-records-by-type response +mx-record+))