#lang racket

;; Adds a result to the history, assigning it an ID.
(define (add-to-history result history)
  (cons result history))

;; Formats the output with the history ID and converts the number to a string.
(define (format-output result id)
  (let ([float-result (real->double-flonum result)])
    (string-append (number->string id) ": " (number->string float-result))))

;; Prints the result with the proper format using `display`.
(define (print-result result id)
  (display (format-output result id))
  (newline))

;; Interactive mode: evaluates expressions and prints the result with history ID.
(define (interactive-mode)
  (displayln "Prefix Calculator (Interactive Mode)")
  (let loop ([history '()] [id 1]) ; Start with empty history and ID 1
    (display "> ")
    (let ([input (string-trim (read-line))])
      (if (string=? input "quit")
          (begin
            (displayln "Exiting... Goodbye!")
            (exit))
          (unless (eof-object? input)
            (with-handlers ([exn:fail? (lambda (e) 
                                          (displayln (format "Error: ~a" (exn-message e))))])
              (let ([result (evaluate-expression (read (open-input-string input)) history)])
                (print-result result id)
                (loop (add-to-history result history) (add1 id)))))))))

;; Batch mode: processes expressions and prints the results with history IDs.
(define (batch-mode)
  (let loop ([history '()] [id 1])
    (with-handlers ([exn:fail? 
                     (lambda (e)
                       (displayln (format "Error: ~a" (exn-message e))))]) ; Print error message and continue
      (for ([input (in-lines)])
        (let ([trimmed-input (string-trim input)])
          (unless (string=? trimmed-input "")
            (with-handlers ([exn:fail? 
                             (lambda (e)
                               (displayln (format "Error: ~a" (exn-message e))))]) ; Print error message
              (let ([result 
                     (with-handlers ([exn:fail? (lambda (e) 
                                                  'error)]) ; Return 'error' on failure
                       (evaluate-expression 
                        (read (open-input-string trimmed-input)) history))])
                (if (not (equal? result 'error))
                    (begin
                      (print-result result id)
                      (set! history (add-to-history result history))
                      (set! id (add1 id)))
                    (begin
                      ;; Explicit "else" branch for the `if` statement
                      (displayln "Skipping invalid result due to error")))))))))
    (displayln "Finished Processing All Lines")))

;; Evaluate expression (requires history as a parameter now).
(define (evaluate-expression expr history)
  (process (if (list? expr)
               (map (lambda (x) (replace-history-ref x history)) expr)
               (replace-history-ref expr history))))

;; Replace history references (e.g., $n).
(define (replace-history-ref arg history)
  (cond
    [(symbol? arg)
     (let ([str (symbol->string arg)])
       (if (string-prefix? str "$")
           (let ([id (string->number (substring str 1))])
             (if (and (number? id) (exact-nonnegative-integer? id) (<= id (length history)))
                 (list-ref (reverse history) (sub1 id))
                 (error (format "Invalid history reference $~a: Out of range" id))))
           arg))]
    [else arg]))

;; Process the expression (basic operators only, no rounding/preprocessing).
(define (process expr)
  (cond
    [(number? expr)
     (real->double-flonum expr)] ; Convert all numbers to float
    [(list? expr)
     (let* ([op (car expr)]
            [args (map process (cdr expr))]) ; Process all arguments recursively
       (if (andmap number? args)
           (case op
             [(+) (real->double-flonum (apply + args))]
             [(*) (real->double-flonum (apply * args))]
             [(/) (if (or (null? args) (zero? (second args)))
                     (error "Error: Division by zero is undefined")
                     (real->double-flonum (apply / args)))]
             [(-) (if (>= (length args) 1)
                      (real->double-flonum (apply - args))
                      (error "Invalid Expression"))]
             [else (error (format "Error: Unknown operator ~a" op))])
           (error (format "Error: Arguments ~a are invalid for operation ~a" args op))))]    
    [else
     (error "Invalid Expression")]))

;; Main entry point.
(define prompt?
   (let ((args (current-command-line-arguments)))
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

(define (main)
  (if prompt?
      (interactive-mode)
      (batch-mode)))

(main)