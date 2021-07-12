#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 null))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  ;fac o noua copie pe care o salvez in acelasi counter si astfel fac actualizarea
  (struct-copy counter C [index (counter-index C)] [tt (+ (counter-tt C) minutes)])) ;actualizez cu counter-tt + minutes


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt counters)
  (cond ((null? counters) null)
        ((= (length counters) 1) (cons (match (car counters) [(counter index _ _) index]) (match (car counters) [(counter _ tt _) tt])))
        ((<= (match (car counters) [(counter _ tt _) tt]) (match (cadr counters) [(counter _ tt _) tt])) (min-tt (cons (car counters) (cddr counters))))
        ((> (match (car counters) [(counter _ tt _) tt]) (match (cadr counters) [(counter _ tt _) tt])) (min-tt (cdr counters)))
        ))
; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (define C1 (make-counter (counter-index C) (counter-tt C) (reverse (cons (cons name n-items) (reverse (counter-queue C))))))
  (struct-copy counter C1 [index (counter-index C1)] [tt (+ (counter-tt C1) n-items)]))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (get-counter-of-index index C1 C2 C3 C4)
  (cond ((= 1 index) C1)
        ((= 2 index) C2)
        ((= 3 index) C3)
        ((= 4 index) C4)))

(define (add-to-specified-counter C1 C2 C3 C4 requests C)
  (cond  ((= 1 (counter-index C)) (serve (cdr requests) C C2 C3 C4))
         ((= 2 (counter-index C)) (serve (cdr requests) C1 C C3 C4))
         ((= 3 (counter-index C)) (serve (cdr requests) C1 C2 C C4))
         ((= 4 (counter-index C)) (serve (cdr requests) C1 C2 C3 C))))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (add-to-specified-counter C1 C2 C3 C4 requests (tt+ (get-counter-of-index index C1 C2 C3 C4) minutes))]
                                    
        [(list name n-items)         (cond ((<= n-items ITEMS) (add-to-specified-counter C1 C2 C3 C4 requests (add-to-counter (get-counter-of-index (car (min-tt (list C1 C2 C3 C4))) C1 C2 C3 C4) name n-items)))
                                           
                                           ((> n-items ITEMS) (add-to-specified-counter C1 C2 C3 C4 requests(add-to-counter (get-counter-of-index (car (min-tt (list C2 C3 C4))) C1 C2 C3 C4) name n-items))))])))