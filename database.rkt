#| Assignment 1 - Racket Query Language  (due Oct 4, 11:50pm, on Markus)

***Write the names, CDF accounts and student IDs for each of your group members below.***
***Max group size is two students.***
Mikhail Ameerally, ameeral2, 1002312124
Sean Coutinho, coutin38, 1002481888
|#
#lang racket
(define table1 '( ("Name" "Age" "Major") ("Sean" 20 "Shitology") ("Mikhail" 20 "Awesomeology") ("Isaac" 25 "Management")))
(define table2 '( ("Name" "StuNum" "UtorID") ("Sean" 666 "DuckSauce") ("Mikhail" 555 "AwesomeGuy")))
(define table3 '( ("Name" "Nationality" "Background") ("Mikhail" "USA" "Guyana") ("Isaac" "USA" "Guyana")))
(define tables (list table1 table2 table3))
(define prefixes '( "P" "T" "S"))

(define ns (make-base-namespace))
; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes tuples size index_of get_attribute table_filter replace-attr get_column fake_select cartesian-product develop-table-helper develop-table
         attributes_from_many make_pairs get_all_pairs isDup new_attributes multi_select_table replace help_replace to_curry where_help order If And Or SELECT)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (if (equal? null table)
      '()
      (first table)
      )
  )


#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (if (equal? table null)
      '()
      (rest table)
      )
  )

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table))
  )


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (index_of attributes atr)
  (if (equal? (member atr attributes) #f)
      '()
      (- (length attributes) (length (member atr attributes)))
  
  ))


(define (get_attribute attributes atr tup)
  (list-ref tup (index_of attributes atr)))
    

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

(define (table_filter table f)
  (cons (first table) (filter f (rest table)))
  )

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#

(define (replace-attr x attributes)
  (lambda (tup)
    (if (equal? (member x attributes) #f)
        x
        (get_attribute attributes x tup)
        )
    )
 )


(define (get_column ind table)
  (map (lambda (x)
         (list (list-ref x ind)))
       table)
  )



  
(define  (fake_select attributes table)
  (if (or (null? table) (null? (rest table)))
      '()
      (if (null? attributes)
          (map (λ (x)
                 '()) (first table))
          (if (equal? * attributes)
              table
              (cons attributes (apply map (append (list append)
                                                  (map (lambda (a)
                                                         (get_column (index_of (first table) a) (rest table))
                                                         )attributes)
                                                  )))

          )

          )))


(define (cartesian-product table1 table2)
  (apply append (map (lambda (x)
         (map (lambda (y)
                (append x y)) table2)
         ) table1))
  )

(define (develop-table-helper tables)
  (map (lambda (x)
         (rest x)
         ) tables))

(define (develop-table tables) ; make sure attributes not included. Need rest of every item. develop new list.
  (let ([x (develop-table-helper tables)])
    (if (not (null? x))
        (foldr cartesian-product (first (reverse x)) (reverse (rest (reverse x))))
        (foldr cartesian-product x x)
        )
  )
  )




(define (attributes_from_many tables)
  (map (λ (x)
         (first x))tables))
       

(define (make_pairs attr_lst pre)
  (map (lambda (x)
         (cons x (list pre)))
       attr_lst))

(define (get_all_pairs table_attrs prefixes)
  (apply append (map (lambda (x y)
         (make_pairs x y)) table_attrs prefixes)))

(define (isDup element lst)
  (if (null? lst)
      #f
      (if (and (not (equal? (member element (cdr lst)) #f)) (equal? (car lst) element))
          #t
          (isDup element (cdr lst))
          )
  ))

(define (new_attributes lst_of_pairs)
  (let ([x (map car lst_of_pairs)])
    (map (lambda (y)
           (if (isDup (car y) x)
               (string-append (string-append (first (cdr y)) ".") (car y))
               (car y)
               )) lst_of_pairs)
    )
  )

(define (multi_select_table new_attributes_list new_table_developed)
  (cons new_attributes_list new_table_developed))





; Starter for Part 3; feel free to ignore!

; What should this macro do?
; Give back an argument that filter can use???

(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     (list (replace expr table) ...)
       ]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     (replace-attr atom table)
     ]))
(define (help_replace list_of_lambdas tuple)
  (if (null? list_of_lambdas)
          '()
          
              (if (list? (first list_of_lambdas))
                  (cons (help_replace (first list_of_lambdas) tuple) (help_replace (rest list_of_lambdas) tuple))
                  (cons ((first list_of_lambdas) tuple) (help_replace (rest list_of_lambdas) tuple))
                  )
              ))


(define (to_curry list_of_lambdas tuple)
  (if (list? list_of_lambdas)
      (if (and (equal? (length list_of_lambdas) 1 ) (not (procedure? (first (help_replace list_of_lambdas tuple)))))
          (first (help_replace list_of_lambdas tuple))
          (eval (help_replace list_of_lambdas tuple) ns))
      (list_of_lambdas tuple)))




(define-syntax where_help
  (syntax-rules()
    [(where_help <cond> <table>)
     (let (
        [curry2 (curry to_curry (replace <cond> (first <table>)))]
        )
    (cons (first <table>) (filter curry2 (rest <table>)))
       )
     ]
    )
  )


(define-syntax order
  (syntax-rules()
    [(order <ord> <table>)
     (let (
        [curry2 (curry to_curry (replace <ord> (first <table>)))]
        )
    (cons (first <table>) (sort (rest <table>)
          (λ (x y)
            (> x y))
          #:key curry2)
          )
          
                           
       )
     ]))
    





(define (If cond conseq alter) (if cond conseq alter))
(define (And x y) (and x y))
(define (Or x y) (or x y))



(define-syntax SELECT
  (syntax-rules ()
    [(SELECT <attrs> FROM table)
    (fake_select <attrs> table)]
    [(SELECT <attrs> FROM [table pre] ...)
     (let ([tables (list table ...)]
           [prefixes (list pre ...)]
           #|[new_table_developed (develop-table tables)]
           [table_attrs (attributes_from_many tables)]
           [lst_of_pairs (get_all_pairs (attributes_from_many tables) prefixes)]
           [new_attributes_list (new_attributes (get_all_pairs (attributes_from_many tables) prefixes))]|#
           [new_table (multi_select_table
                       (new_attributes (get_all_pairs (attributes_from_many (list table ...)) (list pre ...)))
                       (develop-table (list table ...)))]
           
           ) ; defns
       (fake_select <attrs> new_table)
       ); let bracket

      ]; case 2 bracket

    [(SELECT <attrs> FROM <table> WHERE arg)
     
     (fake_select <attrs> (where_help arg <table>))
     ]
    
   [(SELECT <attrs> FROM [table pre] ... WHERE arg)
    (let (
           [new_table (multi_select_table
                       (new_attributes (get_all_pairs (attributes_from_many (list table ...)) (list pre ...)))
                       (develop-table (list table ...)))])
      
      (fake_select <attrs> (where_help arg new_table)))
           
    ] ; case 3 bracket
     ; case 4 bracket

    [(SELECT <attrs> FROM table ORDER BY <ord>)
     ;(displayln "not there yet")
     (fake_select <attrs> (order <ord> table))
     ]; case 7
    
    [(SELECT <attrs> FROM [table pre] ... ORDER BY <ord>)
     (let (
           [new_table (multi_select_table
                       (new_attributes (get_all_pairs (attributes_from_many (list table ...)) (list pre ...)))
                       (develop-table (list table ...)))])
      ;(displayln "Hello world")
      (fake_select <attrs> (order <ord> new_table))
       )
     ]
    
    [(SELECT <attrs> FROM [table pre] ... WHERE arg ORDER BY <ord>)
     ;(displayln "(SELECT <attrs> FROM [table pre] ... WHERE arg ORDER BY <ord>)")
     (let (
           [new_table (multi_select_table
                       (new_attributes (get_all_pairs (attributes_from_many (list table ...)) (list pre ...)))
                       (develop-table (list table ...)))])
      
      (fake_select <attrs> (order <ord> (where_help arg new_table)))
       )
     ] ; case 5

    [(SELECT <attrs> FROM table WHERE arg ORDER BY <ord>)
     ;(displayln "(SELECT <attrs> FROM table WHERE arg ORDER BY <ord>)")]
     (fake_select <attrs> (order <ord> (where_help arg table)))]
      ; case 6

     ; case 8

    

    
    ) ; syntax rules
  )


(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))


