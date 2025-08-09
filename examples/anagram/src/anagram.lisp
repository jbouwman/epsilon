(defpackage anagram
  (:use
   cl)
  (:local-nicknames
   (web epsilon.web)
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (json epsilon.json)
   (request epsilon.http.request)
   (response epsilon.http.response)
   (server epsilon.http.server)
   (argparse epsilon.argparse))
  (:export
   main
   compute-anagram
   shuffle-string))

(in-package anagram)

;;; Simple anagram logic using character shuffling

(defun shuffle-string (string)
  "Randomly shuffle characters in a string"
  (let ((chars (coerce string 'list)))
    (coerce (loop for i from (length chars) downto 2
                  do (rotatef (nth (random i) chars)
                              (nth (1- i) chars))
                  finally (return chars))
            'string)))

(defun compute-anagram (text)
  "Generate an anagram by shuffling words individually"
  (str:join #\Space 
    (seq:map #'shuffle-string 
             (str:split #\Space text))))

;;; Web handlers using epsilon.web

(web:defhandler home-handler (req)
  (web:html 
   "<html>
<head>
  <title>Anagram Service</title>
  <style>
    body { font-family: Arial, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; }
    .form-group { margin: 20px 0; }
    input[type=text] { width: 300px; padding: 10px; }
    button { padding: 10px 20px; background: #007cba; color: white; border: none; cursor: pointer; }
    #result { margin-top: 20px; padding: 15px; background: #f0f0f0; border-radius: 5px; }
  </style>
</head>
<body>
  <h1>Anagram Service</h1>
  <p>Enter some text to generate an anagram:</p>
  
  <div class='form-group'>
    <input type='text' id='text-input' placeholder='Enter text here...'>
    <button onclick='generateAnagram()'>Generate Anagram</button>
  </div>
  
  <div id='result'></div>
  
  <script>
    function generateAnagram() {
      const text = document.getElementById('text-input').value;
      if (!text.trim()) {
        document.getElementById('result').innerHTML = '<p style=\"color: red;\">Please enter some text.</p>';
        return;
      }
      
      fetch('/api/anagram', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ text: text })
      })
      .then(response => response.json())
      .then(data => {
        if (data.error) {
          document.getElementById('result').innerHTML = '<p style=\"color: red;\">Error: ' + data.error + '</p>';
        } else {
          document.getElementById('result').innerHTML = 
            '<h3>Result:</h3>' +
            '<p><strong>Original:</strong> ' + data.original + '</p>' +
            '<p><strong>Anagram:</strong> ' + data.anagram + '</p>';
        }
      })
      .catch(error => {
        document.getElementById('result').innerHTML = '<p style=\"color: red;\">Error: ' + error + '</p>';
      });
    }
    
    // Allow Enter key to submit
    document.getElementById('text-input').addEventListener('keypress', function(e) {
      if (e.key === 'Enter') {
        generateAnagram();
      }
    });
  </script>
</body>
</html>"))

(web:defhandler anagram-handler (req)
  (web:with-json-body (data req)
    (let ((text (map:get data "text")))
      (if (and text (> (length (string-trim " " text)) 0))
          (let ((anagram (compute-anagram text)))
            (web:json (map:make-map "original" text
                                    "anagram" anagram)))
          (web:bad-request "No text provided")))))

;;; Route definition

(web:defroutes *routes*
  (:get "/" #'home-handler)
  (:post "/api/anagram" #'anagram-handler))

;;; Signal handling for graceful shutdown

(defvar *server* nil
  "Current running server instance")

(defvar *shutdown-requested* nil
  "Flag to indicate shutdown was requested")

(defun setup-signal-handlers ()
  "Set up signal handlers for graceful shutdown"
  ;; Handle SIGINT (Ctrl+C)
  (sb-sys:enable-interrupt sb-posix:sigint 
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (format t "~%Received SIGINT, initiating graceful shutdown...~%")
                             (setf *shutdown-requested* t)
                             (when *server*
                               (ignore-errors (server:stop-server *server*)))))
    
  ;; Handle SIGTERM (typical Docker/systemd shutdown)
  (sb-sys:enable-interrupt sb-posix:sigterm
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (format t "~%Received SIGTERM, initiating graceful shutdown...~%")
                             (setf *shutdown-requested* t)
                             (when *server*
                               (ignore-errors (server:stop-server *server*))))))

(defun create-argument-parser ()
  "Create command line argument parser for anagram service"
  (let ((parser (argparse:make-parser 
                 :command "anagram"
                 :description "A web service that generates anagrams by shuffling word characters")))
    
    ;; Port option
    (argparse:add-argument parser
                           "--port"
                           :type 'integer
                           :default 8080
                           :help "Port to listen on (default: 8080)")
    
    ;; Host/address option  
    (argparse:add-argument parser
                           "--host"
                           :type 'string
                           :default "0.0.0.0"
                           :help "Host address to bind to (default: 0.0.0.0)")
    
    ;; Help option
    (argparse:add-argument parser
                           "--help"
                           :action 'store-true
                           :help "Show this help message and exit")
    
    parser))

;;; Main entry point

(defun main (&rest args)
  "Main entry point for the anagram service"
  (let ((parser (create-argument-parser)))
    
    ;; Parse command line arguments
    (handler-case
        (let ((parsed (argparse:parse-args parser args)))
          
          ;; Handle help
          (when (map:get (argparse:parsed-options parsed) "help")
            (argparse:print-help parser)
            (sb-ext:exit :code 0))
          
          ;; Extract options
          (let ((port (map:get (argparse:parsed-options parsed) "port" 8080))
                (host (map:get (argparse:parsed-options parsed) "host" "0.0.0.0")))
            
            ;; Validate port range
            (unless (and (integerp port) (<= 1 port 65535))
              (format t "Error: Port must be between 1 and 65535~%")
              (sb-ext:exit :code 1))
            
            (format t "~&Starting Anagram Service v1.0.0~%")
            (format t "~&Listening on ~A:~A~%" host port)
            
            ;; Set up signal handlers for graceful shutdown
            (setup-signal-handlers)
            
            ;; Create application with middleware
            (let ((app (web:wrap-middleware 
                        (web:handle-routes *routes*)
                        web:logging-middleware
                        web:json-errors-middleware)))
              
              ;; Start the server
              (handler-case
                  (progn
                    (setf *server* (server:start-server app :port port :address host))
                    (format t "~&Server started successfully. Press Ctrl+C to stop.~%")
                    
                    ;; Keep main thread alive until shutdown requested
                    (loop until *shutdown-requested*
                          do (sleep 1))
                    
                    (format t "~&Shutdown complete.~%")
                    (sb-ext:exit :code 0))
                
                (error (e)
                  (format t "~&Error starting server: ~A~%" e)
                  (sb-ext:exit :code 1))))))
      
      ;; Handle argument parsing errors gracefully
      (argparse:argument-error (e)
        (format t "~&Error: ~A~%" (argparse::error-message e))
        (terpri)
        (argparse:print-usage parser)
        (sb-ext:exit :code 1))
      
      (error (e)
        (format t "~&Unexpected error: ~A~%" e)
        (sb-ext:exit :code 1)))))
