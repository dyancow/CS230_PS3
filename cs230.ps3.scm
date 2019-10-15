;;;
;;;cs230.ps3.scm
;;;
;;; For this problem set, when defining functions do not use types in 
;;; the lambda expressions. Instead, you should add a comment as to what 
;;; the type should be.

;;; Do not use (require racket/base) for this problem set.
;; ----- Useful functions -----

;; Define a predicate member? that returns #t if obj is a member of
;; lst and #f otherwise.

;; Contrast with the builtin member function, which returns the
;; sublist of lst starting with obj if obj is in the list.
(require racket/class)

(define member?
  (lambda (obj lst)
    (not (not (member obj lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;foldr and foldl are provided in scheme                                                                                                                                           
;;                                                                                                                                                                                 
;;(define accumulate                                                                                                                                                               
;; (lambda (initial op l)                                                                                                                                                          
;;    (cond ((null? l) initial)                                                                                                                                                    
;;      (else                                                                                                                                                                      
;;        (op (car l) (accumulate initial op (cdr l)))))))                                                                                                                         
;;                                                                                                                                                                                 
;;Note: (accumulate  '() cons '(1 2 3 4)) => '(1 2 3 4)
;;(define foldr (lambda (op init lst) (accumulate init op lst)))                                         
;;Note: (foldr cons '() '(1 2 3 4)) => '(1 2 3 4)                                                                              
;;Whereas: (foldl cons '() '(1 2 3 4) => '(4 3 2 1)  
;; ------ Data type definitions -----

;; Directed graph class definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(vertices <graph>) => list of vertices
;;(edges <graph>) => list of edges 
;;
(defclass <graph> ()
  (vertices :initarg :vertices :accessor vertices) 
  (edges :initarg :edges :accessor edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(name <vertex>) => the name of the vertex
;;ex: (name (make-vertex 'a)) => a
;;
(defclass <vertex> ()
  (name :initarg :name :accessor name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start <directed-edge>) => first vertex of directed-edge
;;ex: (start (v1 v2)) => v1
;;(finish <directed-edge>) => second vertex of directed-edge
;;ex: (finish (v1 v2)) => v2
;;
(defclass <directed-edge> ()
  (start :initarg :start :accessor start)
  (finish :initarg :finish :accessor finish))

(define make-vertex
  (lambda (name)
    (make <vertex> :name name)))

(define make-edge
  (lambda (a b) ;a <vertex> b <vertex>
    (make <directed-edge> :start a :finish b)))

;; Two vertices are considered equal if their names are equal

(define equal-vertex?
  (lambda (v1 v2)
    (eq? (name v1) (name v2))))

;; lookup-vertex takes a name and a list of vertices, and finds a vertex
;; with that name.  Useful when you have the name of a vertex and need
;; the vertex itself.

(define lookup-vertex
  (lambda (vname vlist)
    (cond ((null? vlist) #f)
          ((equal? vname (name (first vlist))) (first vlist)) ;replaced car with first
          (else (lookup-vertex vname (rest vlist)))))) ;replaced cdr with rest

;; make-graph takes two lists whose atoms are symbols, one of the form
;;   (v1 v2 v3 ...) 
;; which becomes the list of vertices and the other of the form
;;   ((u1 u2) (u3 u4) ...) 
;; which becomes the list of edges.

(define make-graph
  (lambda (v-names e-list)
    (let* ((v (map make-vertex v-names))
           (create-edge 
              (lambda (name1 name2)
                (make-edge (lookup-vertex name1 v)
                           (lookup-vertex name2 v)))))
        (make <graph>
              :vertices v        
              :edges (map create-edge
                          (map first e-list)
                          (map second e-list))))))

;; Convert a list of vertices to a list of names of vertices

(define name-vertices
  (lambda (vlist)
    (map name vlist)))

;;;Same as standard member function but works with vertices
(define member-vertices 
  (lambda (a lat) 
    (cond ((null? lat) #f) 
          ((equal-vertex? a (car lat)) lat) 
          (else (member-vertices a (cdr lat))))))

(define set-diff-vertices
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member-vertices (car list1) list2) (set-diff-vertices (cdr list1) list2))
          (#t (cons (car list1) (set-diff-vertices (cdr list1) list2))))))

;; Take the union of two sets represented as lists -- no duplicates

(define union
  (lambda (list1 list2)
    (cond ((null? list1) list2) 
          ((member (car list1) list2) (union (cdr list1) list2))
          (else (cons (car list1) (union (cdr list1) list2))))))
          

;; Take the intersection of two sets represented as lists 

(define intersection
  (lambda (list1 list2) 
    (cond ((null? list1) '()) 
          ((member (car list1) list2) 
             (cons (car list1) (intersection (cdr list1) list2)))
          (else (intersection (cdr list1) list2)))))

;;; ----- TESTING EXAMPLES -----

;; ----- Problem 1 -----

;;ANSWER

;(map (lambda (x) (verify-path g (list x (cdr lst)))) (exits (car lst) g))))))

(define exits ; takes vertex (v) and graph (g) and returns a list of a vertices 1 step from v
  (lambda (v g)
    (map (lambda (x) (finish x))
         (filter (lambda (x) (equal-vertex? v (start x))) (edges g)))))

(define verify-path 
  (lambda (g lst)
    (cond ((equals? (cdr lst) '()) #t)
          ((member-vertices (cadr lst) (exits (car lst) g))
           (verify-path g (cdr lst)))
          (else #f))))

;; RUNTIME PROOF!!!!!!!!!!!!!!!!!!!!!!!!!
;; Finally, give a convincing logical argument that your function has a running time which is polynomial in the number of vertices in the graph. Your argument may be informal, in that it does not need to involve mathematical notation, nor does it need to be an outright proof; however, it should be convincing in that someone, such as your graders, should be able to understand why it is true!

(define g1 (make-graph '(a b c d e) 
		       '((a b) (a c) (b c) (b e) (c d) (d b))))

(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))

(display "Problem 1 Testing") (newline)

(verify-path g1
             (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d b e)))
; ==> #t
  
(verify-path g1
             (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d e)))
; ==> #f

;;INFORMAL PROOF EXTERNAL

;; ----- Problem 2 -----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(label <labeled-edge>) => label of labeled-edge
;;ex: (label (make-labeled-edge a b l)) => l
;;
(defclass <labeled-edge> (<directed-edge>)
  (label :initarg :label :accessor label))

(define make-labeled-edge
  (lambda (a b l) ;a <vertex> b <vertex> l <obj>
    (make <labeled-edge> :start a :finish b :label l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start-state <automaton>) => start-state of automaton
;;(final-states <automaton>) => list of final-states of automaton
;;    
(defclass <automaton> (<graph>)
  (start-state :initarg :start-state :accessor start-state)
  (final-states :initarg :final-states :accessor final-states)) ;start-state <symbol> final-states <list>
;;note: all accessors that apply to graph apply to automaton
  
;; make-automaton takes four parameters.  
;; The first is a list of symbols of the form (v1 v2 v3 ...) which 
;;   becomes the list of vertices.
;; The second is a list of triples of the form 
;;   ((u1 u2 l1) (u3 u4 l2) ...) which becomes the list of labeled
;;   edges (with the u's symbols which represent vertices and the l's 
;;   objects which become the labels).
;; The third is a single symbol for the start state.
;; The fourth is a list of symbols that represent final states.

(define make-automaton
  (lambda (v-names e-list s-state f-states) ;v-names <list>, e-list <list>, s-state <symbol>, f-states <list>
    (let* ((v (map make-vertex v-names))
           (create-labeled-edge 
              (lambda (name1 name2 label)
                (make-labeled-edge (lookup-vertex name1 v) 
                                   (lookup-vertex name2 v) 
                                   label))))
      (make <automaton>
            :vertices v
            :edges (map create-labeled-edge
                        (map first e-list)
                        (map second e-list)
                        (map third e-list))
            :start-state s-state
            :final-states f-states))))

(define dfa1
  (make-automaton '(a b c) 
                  '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
                  'a
                  '(a)))

;;; ----- Problem 3 -----

; ANSWER

(define xor ; helper - exclusive or function
  (lambda (a b)
    (cond ((and a b) #f)
          (else
           (or a b)))))

(define check-edge ; helper - checks if start (s) and symbol (sym) are equivalent to edge (e)
  (lambda (s sym e) 
    (if (and (equal? (name (start e)) s)
             (equal? (label e) sym))
        (name (finish e))
        #f)))
   
(define step-dfa
  (lambda (dfa s sym)
    (foldl xor #f
           (map (lambda (x) (check-edge s sym x)) (edges dfa)))))

#|
(define step-dfaTEMP
  (lambda (dfa s sym)
    (letrec ((helper
              (lambda (edgelst)
                (cond ((null? edgelst) #f)
                      ((and (equal? (name (start (car edgelst))) s)
                            (equal? (label (car edgelst)) sym))
                       (name (finish (car edgelst))))
                      (else
                       (helper (cdr edgelst)))))))
      (helper (edges dfa)))))
|#

(display "Problem 3 Testing") (newline)
(step-dfa dfa1 'c 1) ;==> c
(step-dfa dfa1 'd 0) ;==> #f
(step-dfa dfa1 'a 0) ;==> a
(step-dfa dfa1 'a 1) ;==> b
(step-dfa dfa1 'a 2) ;==> #f

(define bad-dfa
  (make-automaton '(a b c)
                  '((a a 0) (a b 0) (b a 1) (b c 0) (c b 0) (c c 1))
                  'a '(a)))

(step-dfa bad-dfa 'a 0) ;==> #f

;; ----- Problem 4 -----

(define integer->binary
  (lambda (n)
    (cond ((eq? n 0) '())
	  (else (append (integer->binary (quotient n 2)) 
                        (list (if (even? n) 0 1)))))))

; ANSWER

(define simulate-dfa
  (lambda (dfa seq)
    (letrec ((helper
              (lambda (s sequen)
                (cond ((null? sequen)
                       (member? s (final-states dfa)))
                      ((step-dfa dfa s (car sequen))
                       (helper (step-dfa dfa s (car sequen)) (cdr sequen)))
                      (else #f)))))
      (helper (start-state dfa) seq))))

#|
(define simulate-dfaTEMP
  (lambda (dfa sequence)
    (letrec ((helper
              (lambda (s seq)
                (cond ((equals? (cdr seq) '()) #t)
                      (else
                       (foldl (lambda (a b) (or a b)) #f 
                              (map (lambda (x) (helper x (cdr seq)))
                                   (filter (lambda (x) (step-dfa dfa x (car seq)))
                                           (name-vertices (exits (lookup-vertex s (vertices dfa)) dfa))))))))))
      (helper (start-state dfa) sequence))))
|#

(display "Problem 4 Testing") (newline)
(simulate-dfa dfa1 '(1 0 0 1)) ;==> #t
(simulate-dfa dfa1 '(1 0 1 1)) ;==> #f
(simulate-dfa dfa1 (integer->binary 12))
(simulate-dfa dfa1 (integer->binary 10))

;; ----- Problem 5 -----


; ANSWER

(define step-nfa
  (lambda (nfa slst sym)
    (cond ((null? slst) '() )
          (else
           (append (filter identity
                   (map (lambda (x) (check-edge (car slst) sym x)) (edges nfa)))
                 (step-nfa nfa (cdr slst) sym))))))

(define simulate-nfa
  (lambda (nfa seq)
    (letrec ((helper
              (lambda (slst sequen)
                (cond ((null? sequen)
                       (member? #t
                                (map (lambda (x) (member? x (final-states nfa))) slst)))
                      ((step-nfa nfa slst (car sequen))
                       (helper (step-nfa nfa slst (car sequen)) (cdr sequen)))                    
                      (else #f)))))
      (helper (list (start-state nfa)) seq))))

(define nfa1
  (make-automaton '(a b c d e)
                  '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
                            (d d 0) (d d 1) (e e 0) (e e 1))
                  'a
                  '(d e)))

(display "Problem 4 Testing") (newline)
(step-nfa nfa1 '(a b c d e) 2) ;==> ()
(step-nfa nfa1 '(a) 1) ;==> (a b)
(step-nfa nfa1 '(c e) 1) ;==> (e)
(step-nfa nfa1 '(b d) 1) ;==> (d) DUPLICATES?

(simulate-nfa nfa1 '(1 0 0 1)) ;==> #t
(simulate-nfa nfa1 '(1 0)) ;==> #f
(simulate-nfa nfa1 '(1 0 0)) ;==> #t
(simulate-nfa nfa1 '(0 1)) ;==> #f

;; ----- Problem 6 -----

(define path?
  (lambda (v1 v2 g)
    (member? (list )

             

(define g2 (make-graph '(a b c) '((a b) (b a) (a c) (c a) (b c))))
(define g3 (make-graph '(a b c d) '((a b) (b c) (a c) (c b) (d b))))
(define g4 (make-graph '(a b c d) '((a b) (a c) (b a) (c a) (a d) (b c) (c b))))

; (path? 'a 'e g1) ==> #t
; (path? 'd 'a g1) ==> #f
; (path? 'a 'c g2) ==> #t
; (path? 'c 'b g2) ==> #t
; (path? 'd 'd g3) ==> #t
; (path? 'a 'd g3) ==> #f
; (path? 'b 'd g4) ==> #t

;; ----- Problem 7 -----
(defclass <vertex+parent> (<vertex>)
  (parent :initarg :parent :accessor parent))

(define make-vertex+parent
  (lambda (v p) ;v <vertex>, p <obj>                                                                                                                                               
    (make <vertex+parent> :name (name v) :parent p)))

; (name-vertices (find-path 'a 'e g1)) ==> (a b e)
; (find-path 'd 'a  g1)                   ==> #f
; (name-vertices (find-path 'a 'c g2)) ==> (a c) or (a b c)
; (name-vertices (find-path 'c 'b g2)) ==> (c a b)
; (name-vertices (find-path 'd 'd g3)) ==> (d)
; (find-path 'a 'd g3)                    ==> #f
; (name-vertices (find-path 'b 'd g4)) ==> (b a d)

;;TODO 6 and 7 and "proof of runtime" for 1