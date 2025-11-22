#lang racket/base
(require racket/list)
;;Your full name: Sindia Aourar
;;Student ID: 001354264-8
;;Date of birth (day/month/year): 15/11/2005

;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.

;;Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
  ((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
  ((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
  ((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
  ((Mary Jones) (() ()) ((12 5 1967) (19 5 2024)))
  ((Tom Blake) (() ()) ((17 1 1964) ()))
  ((Ada West) (() ()) ((22 8 1973) ()))
  ((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
  ((Ned Bloom) (() ()) ((23 4 2001) ())) 
  ((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))
;,Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021))) 
((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
((John Doe) (() ()) ((18 2 1899)(7 7 1970)))
((Lisa Brown) (() ())((30 6 1904) (6 3 1980)))
((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1
(define (lst-mb mb)
  mb)
 
;; C2 
(define (lst-pb pb)
  pb)
  
;; C3
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))
			
(define (lst-all mb pb)
  (append-lst mb pb))
(lst-all Mb Pb)

;; A1
(define (parents mb)
  (remove-duplicates ;; remove duplicate
   (apply append  ;; combine it into one list
          (map (lambda (person) ;; applies a function to each person
                 (if (>= (length person) 2)  ;; Ensure there is a parent list
                     (cadr person)  ;; Extract parent
                     '(() ())))  ;; if parent is missing return empty
               mb))))

;; A2
(define (living-members mb)
  (filter (lambda (member) ;; keep only the living members
            (let ((death-info (if (>= (length member) 3) (caddr member) '())))  ;; get the death date if available
              (or (null? death-info) (null? (cadr death-info)))))  ;; ensures if death date empty
          mb))

;; A3 (I have used AI to get some help in this part)
(define (current-age mb)
  (map (lambda (person) ;; apply a function to each mb person
         (let* ((birth-info (if (and (>= (length person) 3) (pair? (caddr person)))
                                (caddr person)
                                '(() ())))  ;; get the birth info if available else empty
                (birth-date (if (pair? birth-info) (car birth-info) '())) ;; get birth date
                (birth-year (if (and (pair? birth-date) (>= (length birth-date) 3))
                                (list-ref birth-date 2) ;; get the birth year
                                'Unknown))  ;; no valid birth year, return 'Unknown'
                (current-year 2025))  ;; current year
           (if (number? birth-year)
               (list (car person) (- current-year birth-year)) 
               (list (car person) 'Unknown))))  ;; Return 'Unknown' if birth year missing
       mb))

;; A4
(define (birthday-month-same mb)
  (filter (lambda (person) ;; filter list to get the people born in same month (June)
            (let* ((dates (if (and (>= (length person) 3) (pair? (caddr person))) 
                              (caddr person) 
                              '(() ())))  ;; get the birth date
                   (birthdate (if (pair? dates) (car dates) '())) ;; get actual birthday
                   (birth-month (if (and (pair? birthdate) (>= (length birthdate) 2))  
                                    (cadr birthdate)  ;; Extract birth month
                                    #f)))  ;; If invalid, return #f
              (equal? birth-month 6)))  ;; check if birth month is June (6)
          mb))

;; A5
(define (get-last-name person) ;; function to extract last name
  (let ((name (car person)))  ;; get name list
    (if (and (list? name) (>= (length name) 2))  ;; Ensure it's a valid name list
        (symbol->string (cadr name))  ;; get last name safely
        "")))  ;; if name invalid return empty

(define (sort-by-last mb)
  (define (compare-last-names person1 person2) ;; comparison function
    (string<? (get-last-name person1) (get-last-name person2)))  ;; Compare last names
  (sort mb compare-last-names)) ;; sort list using the comparison function

;; A6
(define (change-name mb)
  (map (lambda (person) ;; apply function to each person on the list
         (let* ((full-name (car person))  ;; get full name list
                (first-name (car full-name))  ;; get first name
                (last-name (cadr full-name)))  ;; get last name
           (if (equal? first-name 'John)
               (list (list 'Juan last-name) (cadr person) (caddr person))  ;; Change "John" to "Juan"
               person)))  ;; else, keep person unchanged
       mb))


;; B1 - Function to return all children in the paternal branch
(define (children lst)
  ;; Extracts the names (first element) of each entry in the list
  (map (lambda (x) (car x)) lst))


;; B2 - Function to get the oldest living member of the paternal branch
(define (oldest-living-member pd)
  (define current-year 2025) ;; Manually setting the current year

  ;; Extract the birth year safely
  (define (birth-year dob)
    (if (and (list? dob) (= (length dob) 3))
        (caddr dob)  ;; Get the last element (year)
        0))  ;; Return 0 if the date format is incorrect

  ;; Check if a person is alive (death date is empty)
  (define (living? death-date)
    (null? death-date))  

  ;; Recursive function to find the oldest living person
  (define (find-oldest members oldest-so-far)
    (if (null? members)
        oldest-so-far  ;; Return the oldest found so far
        (let* ((current-member (car members)) ;; Take first person
               (dob (caddr current-member))  ;; Extract birth details
               (death-date (cadr dob))  ;; Get death date (if exists)
               (current-age (- current-year (birth-year dob))))  ;; Calculate age

          ;; Compare ages and update oldest living person
          (if (and (living? death-date)
                   (> current-age (- current-year (birth-year (caddr oldest-so-far)))))
              (find-oldest (cdr members) current-member)  ;; Update oldest
              (find-oldest (cdr members) oldest-so-far)))))  ;; Continue checking others

  ;; Get list of living members
  (let ((living-members (filter (lambda (person) (living? (cadr (caddr person)))) pd)))
    (if (null? living-members)
        'No-living-members  ;; If no one is alive
        (find-oldest living-members (car living-members)))))  ;; Find the oldest among the living


;; B3 - Function to calculate the average age at death
(define (average-age-on-death pb)
  ;; Extract birth year from (day month year)
  (define (birth-year dob)
    (if (and (list? dob) (= (length dob) 3))
        (caddr dob)  ;; Get year part
        #f))  ;; Return #f if format is wrong

  ;; Extract death year from (day month year)
  (define (death-year dod)
    (if (and (list? dod) (= (length dod) 3))
        (caddr dod)  ;; Get year part
        #f))  ;; Return #f if format is wrong

  ;; calculate age at death for a person
  (define (age-at-death person)
    (let* ((dates (caddr person))  ;; Extract (birthdate, deathdate)
           (dob (car dates))  ;; Get birthdate
           (dod (cadr dates)))  ;; Get deathdate
      (if (and dob dod (birth-year dob) (death-year dod))  ;; Ensure valid dates
          (- (death-year dod) (birth-year dob))  ;; calculate age at death
          #f)))  ;; Return #f if dates are missing

  ;; Get a list of valid ages at death
  (define valid-ages (filter number? (map age-at-death pb)))

  ;; calculate average if there are valid ages
  (if (null? valid-ages)
      0  ;; If no valid data, return 0
      (/ (exact->inexact (apply + valid-ages)) (exact->inexact (length valid-ages)))))  ;; calculate and convert to decimal (I used AI to know how to exactly convert to decimal)


;; B4 - Function to find members born in November (month 11)
(define (birthday-month pb)
  (filter (lambda (person)
            (let* ((dates (caddr person))  ;; Extract (birthdate, deathdate) list
                   (birthdate (car dates))  ;; Get birthdate
                   (birth-month (if (and (list? birthdate) (>= (length birthdate) 2))  
                                    (cadr birthdate)  ;; Extract month safely
                                    #f)))  ;; Return #f if invalid
              (equal? birth-month 11)))  ;; Check if birth month is November
          pb))


;; B5 - Function to sort members by first name
(define (sort-by-first pb)
  ;; Extract the first name as a string
  (define (get-first-name person)
    (symbol->string (car (car person))))  

  ;; Compare first names for sorting
  (define (compare-first-names person1 person2)
    (string<? (get-first-name person1) (get-first-name person2)))  

  ;; Sort members and extract names
  (map car (sort pb compare-first-names)))  


;; B6 - Function to change "Mary" to "Maria" in the family tree
(define (change-name-mary pb)
  (map (lambda (person)
         (let* ((full-name (car person))  ;; Extract full name (list)
                (first-name (car full-name))  ;; Extract first name
                (last-name (cadr full-name)))  ;; Extract last name
           (if (equal? first-name 'Mary)
               (list (list 'Maria last-name) (cadr person) (caddr person))  ;; Rename "Mary" to "Maria"
               person)))  ;; Otherwise, keep person unchanged
       pb))

;; Below is the test code to execute each function

(newline)
(display "Parents:")
(parents Mb)

(newline)
(display "living members:")
(living-members Mb)

(newline)
(display "Current age:")
(current-age Mb)

(newline)
(display "same birthday:")
(birthday-month-same Mb)

(newline)
(display "sort by last name:")
(sort-by-last Mb)

(newline)
(display "Change name to:")
(change-name Mb)

(newline)
(display "Children: ")
(display (children Pb))


(newline)
(display "Oldest living member: ")
(display (oldest-living-member Pb))


(newline)
(display "Average age on death: ")
(display (average-age-on-death Pb))

(newline)
(display "Born in November: ")
(display (birthday-month Pb))

(newline)
(display "Change Name 'Mary' to 'Maria': ")
(display (change-name-mary Pb))

(newline)
(display "Sorted by first name: ")
(for-each (lambda (name) (displayln name)) (sort-by-first Pb)) ;; Loop through the sorted list and print each name on a new line

