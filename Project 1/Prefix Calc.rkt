#lang racket

(define prompt?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

(define history '())

(define (add-to-history result)
  (set! history (cons result history)))

(define (get-from-history id)
  (let ([index (- (length history) id)])
    (if (or (< index 0) (>= index (length history)))
        (error (format "Invalid history reference $~a: Out of range" id))
        (list-ref history index))))

(define (round-number num)
  (if (real? num)
      (let ([rounded (/ (floor (+ (* num 100) 0.5)) 100.0)])
        (if (= rounded (floor rounded))
            (exact->inexact (floor rounded))
            rounded))
      num))

(define (format-output result)
  (cond
    [(and (real? result) (= result (floor result)))
     (substring (number->string result) 0 (- (string-length (number->string result)) 2))]
    [(real? result) (number->string result)]
    [else (format "~a" result)]))

(define (replace-history-ref arg)
  (cond
    [(symbol? arg)
     (let ([str (symbol->string arg)])
       (if (string-prefix? str "$")
           (let ([id (string->number (substring str 1))])
             (if (and (number? id) (exact-nonnegative-integer? id))
                 (get-from-history id)
                 (error "Invalid history reference format")))
           arg))]
    [else arg]))

(define (process expr)
  (cond
    [(number? expr) (round-number expr)]
    [(list? expr)
     (let ([op (car expr)]
           [args (map process (cdr expr))])
       (round-number
        (case op
          [(+) (apply + args)]
          [(*) (apply * args)]
          [(/) (if (or (null? (cdr args)) 
                       (and (number? (car (cdr args))) (zero? (car (cdr args)))))
                  (error "Error: Division by zero is undefined")
                  (apply / args))]
          [(-) (if (= (length args) 1) (- (car args)) (apply - args))])))]
    [(symbol? expr) (replace-history-ref expr)]
    [else (error "Invalid expression")]))

(define (evaluate-expression expr)
  (process (if (list? expr)
               (map replace-history-ref expr)
               (replace-history-ref expr))))

(define (interactive-mode)
  (displayln "Prefix Calculator (Interactive Mode)")
  (let loop ()
    (display "> ")
    (let ([input (string-trim (read-line))])
      (if (string=? input "quit")
          (begin
            (displayln "Exiting... Goodbye!")
            (exit))
          (unless (eof-object? input)
            (with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
              (let ([result (evaluate-expression (read (open-input-string input)))])
                (add-to-history result)
                (display (format-output result))
                (newline)))
            (loop))))))

(define (batch-mode)
  (let loop ()
    (let ([input (read-line)])
      (cond
        [(or (eof-object? input) (string=? (string-trim input) "quit"))
         ;; Exit when EOF or "quit" is encountered
         (displayln "Exiting batch mode... Goodbye!")
         (exit)]
        [else
         (let ([trimmed-input (string-trim input)]) ; Trim whitespace
           (unless (string=? trimmed-input "") ; Skip empty lines
             (with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
               (let ([result (evaluate-expression (read (open-input-string trimmed-input)))])
                 (add-to-history result)
                 (display (format-output result))
                 (newline)))))
         (loop)])))) ; Recursively process the next line

(define (main)
  (if prompt?
      (interactive-mode)
      (batch-mode)))

(main)