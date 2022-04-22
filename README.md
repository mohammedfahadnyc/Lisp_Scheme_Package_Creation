# Lisp_Scheme_Package_Creation
List in Scheme Represented as prime factorization and represented as number. Complete package for list manipulation.


;; Can we use numbers (and nothing but numbers) to represent lists and sets of positive integers?

;;;;;;;;;;;;;;;;;;;;;;;

;; First: 
;; indexing of prime numbers

;; We are all familiar with the prime numbers.  Let's index them starting at 0: thus 2 is the 0th prime, 3 is the first prime,
;; and so on.

;;     prime     index
;;     -----     -----
;;       2         0
;;       3         1
;;       5         2
;;       7         3

;;      ...       ...


;;;;;;;;;;;;;;;;;;;;;;;

;; Second:
;; indexing of lists

;; We follow Scheme and use 0-based indexing for lists.  Thus, in the list (1 2 3), the 0th element is 1, the 1st element is 2,
;; and the 2nd element is 3.


;;;;;;;;;;;;;;;;;;;;;;;

;; Third
;; lists represented as numbers


;; Here is one idea:

;;; The empty list is represented by 1

;;; If j is the kth element of a nonempty list s, and if p is the kth prime number,
;;; then (1) p^j = (expt p j) is a factor of (num s), and (2) no higher power of p is a factor of (num s).

;; Let's have a few examples

;;; if s is the list (5), (num s) would be the number 2^5 = 32
;;; if s is (5 2), (num s) would be (2^5) * (3^2) = 32 * 9 = 288
;;; if s is (5 2 8), (num s) = 288 * (5^8) = 112500000
;;; if s is (5 2 8 2), (num s) = 112500000 * (7^2) = 5512500000

;;;; As the last example makes clear, lists are not sets, even if one ignores the ordering of a list's elements:
;;;; lists can have multiple occurrences of the same element.

;; One can decode (num s) to answer questions about s.  For example, if s is (5 2 8 2), we can use (num s) to see that
;; the index 2 element of s is 8 by computing that the highest power of the 2nd prime (namely, 5) which divides (num s)
;; is 8.  

;; This package uses this idea and includes the following functions for lisp-R5RS scheme
 for list manipulation.
;;; -- a function myequal? which inputs numbers n representing a list s and m representing a list t, and which checks whether s and
;;;    t are the same list (and returns #t  only if s and t are the same lists)
;
;;; -- a function head which inputs a number n which represents a list s and which returns the number in the
;;;    first position of s, that is, the head of s
;
;;; -- more generally, a function ref which inputs a number n representing a list s and which returns the number
;;;    in the kth position of s
;
;;; -- a function tail which inputs a number n which represents a list s and which returns the number representing the tail
;;;    of s, that is, the list obtained from s by removing its first element
;
;;; -- a function insert-at-head which inputs a number n representing a list s and a second number p, and which returns the number
;;;    representing the list obtained by inserting p at the head of the list s 
;
;;; -- a function len which inputs a number n which represents a list s and which returns the number of elements of s
;
;;; -- a function snoc which inputs a number n which represents a list s and a second number q, and which returns the number
;;;    representing the list obtained by inserting q at the end of the list s
;
;;; -- a function last which inputs a number n which represents a non-empty list s and which returns the rightmost element of s
;
;;; -- a function insert-at which inputs a number n representing a list s, a second number x, and a third number y and which returns
;;;    the number representing the list obtained by inserting x in the yth position of s.  You will need preconditions
;;;    to ensure that the number y makes sense as a position in s
;
;;; -- a function myappend which inputs numbers m and n representing lists s and t, respectively, and which returns the number
;;;    representing the list formed by appending s and t
;
;;; -- a function myreverse which inputs a number representing a list s and which outputs the number representing the reverse
;;;    of s
;
;;; -- a function palin? which inputs a number representing a list s and which determines whether s is a palindrome
;
;;; -- a function sort which inputs a number representing a list s and which outputs the number representing the list
;;;    formed by sorting (smallest to largest) the elements of s
