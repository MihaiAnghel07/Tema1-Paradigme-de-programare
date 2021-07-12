#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 null))


;aceasta functie imi intoarce counter-ul cu indexul primit ca argument
;sau null daca nu exista
(define (get-counter-of-index index counters)
  (cond
    ((null? counters) null)
    ((= index (counter-index (car counters))) (car counters))
    (else (get-counter-of-index index (cdr counters)))))

; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.


;folosesc o functie cu recursivitate pe coada pentru a construi rezultatul astfel:
;construiesc o noua lista cu counters primiti ca argument, iar cand il intalnesc pe cel
;cu indexul cautat, aplic functia asupra lui si il adaug in lista
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


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (tt+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [tt (+ (counter-tt C) minutes)] [et (+ (counter-et C) minutes)])))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ minutes)
  (lambda (C)
    (struct-copy counter C [index (counter-index C)] [tt (counter-tt C)] [et (+ (counter-et C)  minutes)])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.

;in functie de caracteristica counter-ului, adaug o perosana in coada, actualizand coada counter-ului,
;tt si et
(define (add-to-counter name n-items)
  (lambda (C)
    (cond ((equal? #t (and (= 0 (length (counter-queue C))) (> (counter-tt C) 0))) (make-counter (counter-index C) (+ n-items (counter-tt C)) (+ n-items (counter-et C)) (list(cons name n-items))))
          ((= 0 (length (counter-queue C))) (make-counter (counter-index C) n-items n-items (list(cons name n-items))))
          (else (make-counter (counter-index C)  (+ (counter-tt C) n-items) (counter-et C) (reverse (cons (cons name n-items) (reverse (counter-queue C)))))))
    ))
        


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

;spre deosebire de etapa trecuta, abstractizez functie astfel incat , in functie de x,
;se intorca perechea minima (index.tt) sau (index.et)
(define pair-of-min
  (lambda (counters)
    (lambda (x)
      (cond ((null? counters) null)
            ((= (length counters) 1) (cons (counter-index (car counters)) (x (car counters))))
            ((<= (x (car counters)) (x (cadr counters))) ((pair-of-min (cons (car counters) (cddr counters))) x))
            ((> (x (car counters)) (x (cadr counters))) ((pair-of-min (cdr counters)) x)))
      )))

(define (min-tt counters)
  ((pair-of-min counters) counter-tt)); folosind funcția de mai sus

(define (min-et counters)
  ((pair-of-min counters) counter-et)) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
;salvez queue initiala intr-o lista
;iar apoi construiesc un nou counter conform regulilor din cerinta

;in functie de starea cozii casei, se face remove astfel:
; - daca in coada nu e nicun client, se intoarce casa cu coada goala
; - daca in coada este un singur client, tt si et devin 0
; - daca sunt mai multi clienti, se modifica tt, et si coada
(define (remove-first-from-counter C) 
  (cond
    ((= 1 (length (counter-queue C))) (make-counter (counter-index C) 0 0 (cdr (counter-queue C))))
    ((< 1 (length (counter-queue C))) (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr(cadr (counter-queue C))) (cdr (counter-queue C))))
    (else (make-counter (counter-index C) (counter-tt C) (counter-et C) null))))


;aceasta functie primeste un counter(din lista, dar modificat) si o lista de counters si actualizeaza lista
;cu counter-ul C modificat si primit ca argument
(define (my-update C counters)

  (define (my-update-tail C counters acc)
    (cond
      ((null? counters) (flatten acc))
      ((= (counter-index C) (counter-index (car counters))) (my-update-tail C (cdr counters) (cons acc C)))
      (else (my-update-tail C (cdr counters) (cons acc (car counters))))
      ))

  (if (null? (get-counter-of-index (counter-index C) counters))
      counters
      (my-update-tail C counters null)))


;pentru lizibilitate, am creat urmatoarele doua functii care vor ajuta la
;filtrarea elementelor listei
(define (positive-et? C)
  (if (> (counter-et C) 0)
      #t
      #f))


(define (positive-et&queue? C)
  (if (> (counter-et C) 0)
      (if (> (length (counter-queue C)) 0)
          #t
          #f)
      #f))


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
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)


;Explicatie:
;rezultatul final se construieste apeland functia "serve" recursiv, argumentele ei fiind actualizate
;la fiecare apel ajutorul altor functii

;pentru adaugarea unei persoane:
;   - daca numarul de produse ale persoanei este <= cu ITEMS verific unde se gaseste
;     counter-ul cu tt minim si adaug persoana in coada lui
;   - altfel, se adauga direct in slow-counters

;pentru delay:
;   - se actualizeaza tt-ul si et-ul counter-ului cu indexul dat folosind functia update

;pentru remove-first:
;   - daca nicio casa nu are et-ul pozitiv, atunci se ignora comanda
;   - altfel se verifica daca sunt case cu persoane in coada si se elimina persoana cu et minim
;   - daca nu sunt case cu persoane in coada, atunci se modifica cea cu delay, et devine 0

;pentru ensure:
;   - se modifica slow-counters cu ajutorul functiei ensure-ttmed
(define (serve requests fast-counters slow-counters)

  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)                            

        [(list 'ensure average)    (serve (cdr requests) fast-counters (ensure-ttmed average fast-counters slow-counters))]

        [(list name n-items)         (cond
                                       ((<= n-items ITEMS)
                                        (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                                            (serve (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                                            (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))))
                                       ((> n-items ITEMS) (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))))]

        [(list 'delay index minutes) (serve (cdr requests) (update (tt+ minutes) fast-counters index) (update (tt+ minutes) slow-counters index))]
        
        [(list 'remove-first)  (if (null? (filter positive-et? (append fast-counters slow-counters)))                                   
                                   (serve (cdr requests) fast-counters slow-counters)
                                   (if (null? (filter positive-et&queue? (append fast-counters slow-counters)))
                                       (if (null? (get-counter-of-index (car (min-et (filter positive-et? (append fast-counters slow-counters)))) fast-counters))
                                           (serve (cdr requests) fast-counters (my-update (remove-first-from-counter (get-counter-of-index (car (min-et (filter positive-et? slow-counters))) slow-counters)) slow-counters))
                                           (serve (cdr requests) (my-update (remove-first-from-counter (get-counter-of-index (car (min-et (filter positive-et? fast-counters))) fast-counters)) fast-counters) slow-counters))

                                       (if (null? (get-counter-of-index (car (min-et (filter positive-et&queue? (append fast-counters slow-counters)))) fast-counters))
                                           (serve (cdr requests) fast-counters (my-update (remove-first-from-counter (get-counter-of-index (car (min-et (filter positive-et&queue? slow-counters))) slow-counters)) slow-counters))
                                           (serve (cdr requests) (my-update (remove-first-from-counter (get-counter-of-index (car (min-et (filter positive-et&queue? fast-counters))) fast-counters)) fast-counters) slow-counters))))])))

          