#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


;aceasta functie imi intoarce counter-ul cu indexul primit ca argument
;sau null daca nu exista
(define (get-counter-of-index index counters)
  (if (null? (filter (lambda (C) (if(= index (counter-index C)) #t #f)) counters))
      null
      (first (filter (lambda (C) (if(= index (counter-index C)) #t #f)) counters))))

; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))


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


(define (tt+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [tt (+ (counter-tt C) minutes)] [et (+ (counter-et C) minutes)])))

(define (et+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [tt (counter-tt C)] [et (+ (counter-et C)  minutes)])))


(define (add-to-counter name items)     ; testată de checker 
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (cond ((equal? #t (and (queue-empty? (counter-queue C)) (> (counter-tt C) 0))) (make-counter (counter-index C) (+ items (counter-tt C)) (+ items (counter-et C)) (enqueue (cons name items) (counter-queue C))))
          ((equal? #t (queue-empty? (counter-queue C))) (make-counter (counter-index C) items items (enqueue (cons name items) (counter-queue C))))
          (else (make-counter (counter-index C)  (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C)))))))


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
    (if (equal? #t (queue-empty? L))
        (make-counter (counter-index C) 0 0 L)
        (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr(top L)) L))))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!

; in cazul in care counter-ul primit ca argument are coada goala, se actualizeaza tt si et
; astfel incat acestea sa nu fie pe '-'
; altfel, se scad minutele primite ca argument 
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (equal? #t (queue-empty? (counter-queue C)))
        (struct-copy counter C [index (counter-index C)] [tt (max 0 (- (counter-tt C) minutes))] [et (max 0 (- (counter-et C) minutes))])
        (struct-copy counter C [index (counter-index C)] [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)]))))


;functia calculeaza ttmed folosind foldl pentru a calcula suma tt-urilor tuturor caselor
(define (compute-ttmed counters)
  (if (null? counters)
      0
      (/ (foldl (lambda(C acc) (+ acc (counter-tt C))) 0 counters) (length counters))))


;functia modifica slow-counters pana cand ttmed devine <= cu average primit ca parametru
;functia intoarce lista slow-counters actualizata
(define (ensure-ttmed average fast-counters slow-counters)
  (if (>= average (compute-ttmed (append fast-counters slow-counters)))
      slow-counters
      (ensure-ttmed average fast-counters (append slow-counters (list(empty-counter (+ 1 (length (append fast-counters slow-counters)))))))))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

; aceasta functie are ca scop actualizarea listei cu persoanele care au iesit de la case
; daca timpul primit ca argument este mai mare decat et-ul counter-ului inseamna ca
; va fi iesi cel putin o persoana din coada acestuia, deci se actualizeaza lista cu persoane
; care parasesc counters
(define (update-exit-list counters exit-list minutes)
  (if (equal? #t (or (null? (min-et counters)) (= 0 minutes) (and (< (cdr (min-et counters)) minutes) (< 0 (cdr (min-et counters))))))
      exit-list
      (foldl (lambda (C acc)
               (if (> (counter-et C) minutes)
                   acc
                   (if (equal? #t  (queue-empty? (counter-queue C)))
                       acc
                       (append acc (list (cons (counter-index C) (car (top (counter-queue C))))))))) exit-list counters)))


; aceasta functie are ca scop actualizarea listelor de counters
; daca timpul primit ca parametru este mai mare decat et-ul counter-ului, inseamna
; ca trebuie sa eliminam prima persoana din coada si sa actualizam timpii counter-ului
(define (update-list counters minutes)
  (if (null? counters)
      null
      (foldl (lambda (C acc)
               (if (> (counter-et C) minutes)
                   (append acc (list((pass-time-through-counter minutes) C)))
                   (if (equal? #t (queue-empty? (counter-queue C)))
                       (append acc (list ((pass-time-through-counter minutes) C)))
                       (if (> (- minutes (counter-et C)) (counter-et (remove-first-from-counter C)))
                           (append acc (list ((tt+ (counter-et (remove-first-from-counter C))) (remove-first-from-counter C))))
                           (append acc (list ((tt+ (- (counter-et C) minutes)) (remove-first-from-counter C)))))))) null counters)))


(define (serve requests fast-counters slow-counters)
  (define (serve-tail requests fast-counters slow-counters exit-list)
    (if (null? requests)
        (cons exit-list (append fast-counters slow-counters))
        (match (car requests)                            

          [(list 'ensure average)      (serve-tail (cdr requests) fast-counters (ensure-ttmed average fast-counters slow-counters) exit-list)]

          [(list name n-items)         (cond
                                         ((<= n-items ITEMS)
                                          (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                                              (serve-tail (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters exit-list)
                                              (serve-tail (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) exit-list)))
                                         ((> n-items ITEMS) (serve-tail (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) exit-list)))]

          [(list 'delay index minutes) (serve-tail (cdr requests) (update (tt+ minutes) fast-counters index) (update (tt+ minutes) slow-counters index) exit-list)]
          [x                           (serve-tail (cdr requests) (update-list fast-counters x) (update-list slow-counters x) (update-exit-list (append fast-counters slow-counters) exit-list x))])))

  (serve-tail requests fast-counters slow-counters null)
  )
        
        
