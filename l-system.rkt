;;;LSystems

;;;inference rule - to be more functional, there will be no l-rules defined before ... the end.
;;So, the inference rule will be passed as an argument through the used functions.
;inference rule : (String) -> List(String)


;fct qui prend une liste de string et qui va a chacun lui appliquer la règle de dérivation
(define (generation-acc l l-rules acc)
  (if (pair? l)
    (generation-acc (cdr l) l-rules (cons (l-rules (car l)) acc))
    (flat (reverse acc))
    )
  )
(define (generation l l-rules)
  (generation-acc l l-rules '())
)

;;;iterations
(define (iterations lst l-rules n)
  (if (= n 0)
      lst
      (iterations (generation lst l-rules) l-rules (- n 1))
    )
)

;;helper fct
;fct qui va flatter à un niveau une liste de liste donnée en entrée.
(define (flat-acc llst acc)
  (if (null? llst)
    acc
    (flat-acc (cdr llst) (append acc (car llst))))
   )
  
(define (flat llst)
  (flat-acc llst '())
)


;;; main

;fct qui a un string associe la règle de dérivation et retourne une list de string
(define (levy-curve-rules x)
  (cond
    ((eq? x "F") (list "-" "F" "+" "+" "F" "-"))
    (else (list x))
    )
  )

(define (plant-rules x)
  (cond
    ((eq? x "F") (list "F" "F" "+" "[" "+" "F" "-" "F" "-" "F" "]" "-" "[" "-" "F" "+" "F" "+" "F" "]"))
    (else (list x))
    )
  )

(define (pentaplex-rules x)
  (cond
    ((eq? x "F") (list "F" "+" "+" "F" "+" "+" "F" "|" "F" "-" "F" "+" "+" "F"))
    (else (list x))
    )
  )

; test d'execution
(define my-l-rules levy-curve-rules) ; as a shortcut
(iterations (list "F") my-l-rules 2)