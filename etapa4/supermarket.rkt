#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index state tt et queue) #:transparent)

; state este 1 atunci cand casa e deschisa, iar 0 altfel
(define (empty-counter index)
  (make-counter index 1 0 0 empty-queue))


;aceasta functie imi intoarce counter-ul cu indexul primit ca argument
;sau null daca nu exista
(define (get-counter-of-index index counters)
  (if (null? (filter (lambda (C) (if (= index (counter-index C)) #t #f)) counters))
      null
      (first (filter (lambda (C) (if (= index (counter-index C)) #t #f)) counters))))


; functia de update care aplica o functie unara asupra unui counter
(define (update f counters index)
  (define (update-tail f counters index acc)
    (cond
      ((null? counters) (flatten acc))
      ((= index (counter-index (car counters))) (update-tail f (cdr counters) index (cons acc (apply f (get-counter-of-index index counters) null))))
      (else (update-tail f (cdr counters) index (cons acc (car counters))))
      ))

  (if (null? (get-counter-of-index index counters))
      counters
      (update-tail f counters index null)))


;functiile care incrementeaza tt si et 
(define (tt+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [state (counter-state C)] [tt (+ (counter-tt C) minutes)] [et (+ (counter-et C) minutes)])))

(define (et+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [state (counter-state C)] [tt (counter-tt C)] [et (+ (counter-et C)  minutes)])))


(define (add-to-counter name items)     ; testată de checker 
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (cond ((and (queue-empty? (counter-queue C)) (> (counter-tt C) 0)) (make-counter (counter-index C) (counter-state C) (+ items (counter-tt C)) (+ items (counter-et C)) (enqueue (cons name items) (counter-queue C))))
          ((queue-empty? (counter-queue C)) (make-counter (counter-index C) (counter-state C) items items (enqueue (cons name items) (counter-queue C))))
          (else (make-counter (counter-index C) (counter-state C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C)))))))


(define pair-of-min
  (lambda (counters)
    (lambda (x)
      (cond ((null? counters) null)
            ((= (length counters) 1) (cons (counter-index (car counters)) (x (car counters))))
            ((<= (x (car counters)) (x (cadr counters))) ((pair-of-min (cons (car counters) (cddr counters))) x))
            ((> (x (car counters)) (x (cadr counters))) ((pair-of-min (cdr counters)) x)))
      )))

(define (min-tt counters)
  ((pair-of-min counters) counter-tt)); 

(define (min-et counters)
  ((pair-of-min counters) counter-et))


(define (remove-first-from-counter C)   ; testată de checker
  (let ((L (dequeue (counter-queue C))))
    (if (queue-empty? L)
        (make-counter (counter-index C) (counter-state C) 0 0 L)
        (make-counter (counter-index C) (counter-state C) (- (counter-tt C) (counter-et C)) (cdr (top L)) L))))


; in cazul in care counter-ul primit ca argument are coada goala, se actualizeaza tt si et
; astfel incat acestea sa nu fie pe '-'
; altfel, se scad minutele primite ca argument 
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (queue-empty? (counter-queue C))
        (struct-copy counter C [index (counter-index C)] [state (counter-state C)] [tt (max 0 (- (counter-tt C) minutes))] [et (max 0 (- (counter-et C) minutes))])
        (struct-copy counter C [index (counter-index C)] [state (counter-state C)] [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)]))))


;functia calculeaza ttmed folosind foldl pentru a calcula suma tt-urilor tuturor caselor
(define (compute-ttmed counters)
  (if (null? counters)
      0
      (/ (foldl (lambda(C acc) (+ acc (counter-tt C))) 0 (filter (lambda(C)
                                                                   (if (= 0 (counter-state C)) #t #f)) counters)) (length counters))))

;functia modifica slow-counters pana cand ttmed devine <= cu average primit ca parametru
;functia intoarce lista slow-counters actualizata
(define (ensure-ttmed average fast-counters slow-counters)
  (if (>= average (compute-ttmed (append fast-counters slow-counters)))
      slow-counters
      (ensure-ttmed average fast-counters (append slow-counters (list(empty-counter (+ 1 (length (append fast-counters slow-counters)))))))))


; aceasta functie are ca scop actualizarea listei cu persoanele care au iesit de la case
; daca timpul primit ca argument este mai mare decat et-ul counter-ului inseamna ca
; va fi iesi cel putin o persoana din coada acestuia, deci se actualizeaza lista cu persoane
; care parasesc counters
(define (update-exit-list counters exit-list minutes)
  (if (or (null? (min-et counters)) (= 0 minutes))
      exit-list
      (foldl (lambda (C acc)
               (if (or (> (counter-et C) minutes) (queue-empty? (counter-queue C)))
                   acc
                   (append acc (list (cons (counter-index C) (car (top (counter-queue C)))))))) exit-list counters)))


; aceasta functie are ca scop actualizarea listelor de counters
; daca timpul primit ca parametru este mai mare decat et-ul counter-ului, inseamna
; ca trebuie sa eliminam prima persoana din coada si sa actualizam timpii counter-ului
(define (update-list counters minutes)
  (if (null? counters)
      null
      (foldl (lambda (C acc)
               (if (or (queue-empty? (counter-queue C)) (> (counter-et C) minutes))
                   (append acc (list ((pass-time-through-counter minutes) C)))
                   (if (>= (- minutes (counter-et C)) (counter-et (remove-first-from-counter C)))
                       (if (queue-empty? (counter-queue (remove-first-from-counter C)))
                           (append acc (list ((tt+ (- (counter-et C) minutes)) (remove-first-from-counter C))))  
                           (append acc (list ((tt+ (+ (counter-et (remove-first-from-counter C)) (counter-et (remove-first-from-counter (remove-first-from-counter C))))) (remove-first-from-counter (remove-first-from-counter C))))))
                       (append acc (list ((tt+ (- (counter-et C) minutes)) (remove-first-from-counter C))))))) null counters)))
                       


(define (close-counter)
  (lambda (C)
    (make-counter (counter-index C) 0 (counter-tt C) (counter-et C) (counter-queue C))))

;functie primeste o lista de counters si returneaza o lista cu cozile care nu sunt goale
(define (make-queues-list counters)
  (define (make-queues-list-tail counters acc)
    (if (null? counters)
        acc
        (if (queue-empty? (counter-queue (car counters)))
            (make-queues-list-tail (cdr counters) acc)
            (make-queues-list-tail (cdr counters) (append acc (list (cons (counter-index (car counters)) (counter-queue (car counters)))))))))

  (if (null? counters)
      counters
      (make-queues-list-tail counters null)))


;aceasta functie intoarce un counter cu tt minim care este deschisa
(define (open-counter-with-min-tt counters)
  (foldl (lambda (C acc)
            (if (and (< (counter-tt C) (counter-tt acc)) (= (counter-state C) 1)) C acc)) (make-counter 9999 0 9999 9999 empty-queue) counters))
;(foldl (lambda(C acc)
;         (if (queue-empty? (counter-queue C))
;              acc
;             (cons acc (cons (counter-index C) (counter-queue C))))) null counters))
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (serve requests fast-counters slow-counters)
  (define (serve-tail requests fast-counters slow-counters exit-list)
    (if (null? requests)
        (cons exit-list (make-queues-list (append fast-counters slow-counters)))
        (match (car requests)                            

          [(list 'ensure average)      (serve-tail (cdr requests) fast-counters (ensure-ttmed average fast-counters slow-counters) exit-list)]

          [(list 'close index)         (serve-tail (cdr requests) (update (close-counter) fast-counters index) (update (close-counter) slow-counters index) exit-list)]

          [(list name n-items)         (cond
                                         ((<= n-items ITEMS)
                                          (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                                              (serve-tail (cdr requests) (update (add-to-counter name n-items) fast-counters (counter-index (open-counter-with-min-tt fast-counters))) slow-counters exit-list)
                                              (serve-tail (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (counter-index (open-counter-with-min-tt slow-counters))) exit-list)))
                                         ((> n-items ITEMS) (serve-tail (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (counter-index (open-counter-with-min-tt slow-counters))) exit-list)))]

          [(list 'delay index minutes) (serve-tail (cdr requests) (update (tt+ minutes) fast-counters index) (update (tt+ minutes) slow-counters index) exit-list)]

          [x                           (serve-tail (cdr requests) (update-list fast-counters x) (update-list slow-counters x) (update-exit-list (append fast-counters slow-counters) exit-list x))])))

  
  (serve-tail requests fast-counters slow-counters null)
  )