;Dis one EZ
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
;OR
(define zero-crossings
  (stream-map sign-change-detector (stream-cdr sense-data) sense-data))