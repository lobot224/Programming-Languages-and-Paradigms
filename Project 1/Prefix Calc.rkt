#lang racket

;; Determine interactive mode based on command-line arguments.
(define prompt?
  (let* ([args (current-command-line-arguments)]
         [flag (if (> (vector-length args) 0) (vector-ref args 0) "")])
    (not (or (string=? flag "-b") (string=? flag "--batch")))))

;; History storage.
(define history '())

(define (add-to-history result)
  (set! history (cons result history)))

(define (get-from-history id)
  (let ([index (- (length history) id)])
    (if (or (< index 0) (>= index (length history)))
        (error (format "Invalid history reference $~a: Out of range" id))
        (list-ref history index))))

;; Rounds a number to two decimal places.
(define (round-number num)
  (if (real? num)
      (let ([rounded (/ (round (* num 100)) 100.0)])
        (if (= rounded (floor rounded))
            (exact->inexact (floor rounded))
            rounded))
      num))

;; Formats the output:
;; For whole numbers, strips the trailing ".0" using a cached string conversion.
(define (format-output result)
  (cond
    [(and (real? result) (= result (floor result)))
     (let ([s (number->string result)])
       (substring s 0 (- (string-length s) 2)))]
    [(real? result) (number->string result)]
    [else (format "~a" result)]))

;; Replaces history references (e.g. $3) with their corresponding value.
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

;; Processes an expression recursively.
(define (process expr)
  (cond
    [(number? expr) (round-number expr)]
    [(list? expr)
     (let* ([op (car expr)]
            [args (map process (cdr expr))])
       (round-number
        (case op
          [(+) (apply + args)]
          [(*) (apply * args)]
          [(/) (if (or (null? (cdr args))
                       (and (number? (car (cdr args))) (zero? (car (cdr args)))))
                  (error "Error: Division by zero is undefined")
                  (apply / args))]
          [(-) (if (= (length args) 1)
                   (- (car args))
                   (apply - args))])))]
    [(symbol? expr) (replace-history-ref expr)]
    [else (error "Invalid Expression")])) 

;; Evaluates an expression after replacing history references.
(define (evaluate-expression expr)
  (process (if (list? expr)
               (map replace-history-ref expr)
               (replace-history-ref expr))))

;; Custom error handler that prints specific messages.
(define (handle-error e)
  (let ([msg (exn-message e)])
    (cond
      [(or (string-prefix? msg "Error: Division by zero is undefined")
           (string-prefix? msg "Invalid history reference")
           (string=? msg "Invalid history reference format")
           (string=? msg "Invalid Expression"))
       (displayln msg)]
      [else (displayln "Invalid Expression")])))

;; Interactive mode: prompts for and evaluates expressions until "quit" is entered.
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
            (with-handlers ([exn:fail? handle-error])
              (let ([result (evaluate-expression (read (open-input-string input)))])
                (add-to-history result)
                (display (format-output result))
                (newline)))
            (loop))))))

;; Batch mode: processes expressions from standard input until EOF or "quit" is seen.
(define (batch-mode)
  (let loop ()
    (let ([input (read-line)])
      (cond
        [(or (eof-object? input)
             (string=? (string-trim input) "quit"))
         (displayln "Exiting batch mode... Goodbye!")
         (exit)]
        [else
         (let ([trimmed-input (string-trim input)])
           (unless (string=? trimmed-input "")
             (with-handlers ([exn:fail? handle-error])
               (let ([result (evaluate-expression (read (open-input-string trimmed-input)))])
                 (add-to-history result)
                 (display (format-output result))
                 (newline)))))
         (loop)]))))

;; Main entry point.
(define (main)
  (if prompt?
      (interactive-mode)
      (batch-mode)))

(main)