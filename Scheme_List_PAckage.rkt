
; Project 1 - Author: Mohammed Fahad

;___________________________________________________________________________________________________________________________________________

;;; Note : As of now, our list is not 0 based, rather 1 index based, means we start at index 1. So list starts at index1,
;; and the end of list = length of the list. Function 1-8 is fully functional.
;; After function 13,from line 438, additional helper functions added to generate prime numbers, nthPrime is the main function which
;; generates nth Prime and 3 other functions work as its helper.






;___________________________________________________________________________________________________________________________________________

;;; Function 1 :
;;; -- a function myequal? which inputs numbers n representing a list s and m representing a list t, and which checks whether s and
;;;    t are the same list (and returns #t  only if s and t are the same lists)
;; Author - Mohammed Fahad

(define ( myequal? m n )
    (cond
      (( = m n ) #t)
       (( not (= m n) ) #f)))


;; Proof : We are representing our lists as prime factorisation. It's a fundamental theoreum of Artihmetic , which states that
;;Every integer greater than 1 can be expressed as a product of primes. and this product is unique up to reordering the factors.
;; Here's a brief proof : We claim that The product is unique up to reordering the factors. Suppose that some number n has two prime factorisations.
;;Say n=p1...pk=q1...ql, where p1, ..., pk, q1, ..., ql are primes, not necessarily distinct.Then p1 divides q1...ql and p1 is prime,
;;so p1 divides q1 or p1 divides q2 or ... or p1 divides ql.Since p1, q1, ..., ql are prime,
;;we must have p1=qr for some r.Then we can cancel these primes from the products.w e can continue in this way.
;;So p1, p2, ..., pk are in fact q1, q2, ..., ql in some order.So each number representation of our list is unique upto reordering the factors.
;; Hence so  each number is unique and hence can only be obtained by a single unique list. In other words, no two  different lists will be able
;;to produce the same number representation of list using prime factorization,
;;hence it's  sufficient to only check if two numbers m and n matches or not, if they match, only then the lists s and t would be equal, otherwise they
;; wont be equal.






;___________________________________________________________________________________________________________________________________________


;;; Function 2 :
;;; -- a function head which inputs a number n which represents a list s and which returns the number in the
;;;   first position of s, that is, the head of s

;; Author - Mohammed Fahad

 (define (head  n) ( head-helper n 0))
 (define ( head-helper n count )
    (cond
      (( > ( remainder n 2) 0 ) count)
     ( else (head-helper ( quotient n 2 ) ( + 1 count) ))))

;; testrun output : lets say list is list ( 12 9 25 ), (num s) = 2700, so input (head 2700) = 12,
;; list = list (11 3 2), (num s) = 1382400.(head 1382400) = 11. So it's able to return any head number of a list.

;; Proof :
;; takes input n representing list s and returns the head of the list s. We are representing our list with
;; prime numbers , and the first prime number is 2. So we will recursively divide our number n and find the
;; number of times we are able to divide n without a remainder, and keep track of a variable count and for each
;; recrsive call count increases. We will work primarily with head-helper function.
;;Design Idea: Call the head-helper and divide n with 2 for each recursive call, and input the new number
;; that we get from dividing n with 2 as the new n and divide it with 2, count increases by 1
;;each time the procedure is called.

;; proof of head-helper : 
;;Precondition: Input n is an integer representing  list s 
;;Postcondition: Outputs returns an integer count which is the power such as 2 ^ power = head of list
;;Basis Step: The basis step would be for an input n where remainder of n divided by 2 is greater than 0,
; and count returns
;;Induction Hypothesis: Given that the precondition holds, 
;;assume that the recursive call holds
;;Induction Step: Each call of function will increment the count and return count
;;Termination Argument: The program terminates upon the function call when remainder of n and 2 > 0

;; proof of head :
;; precondition : Inputs a number n representing a list
;; postcondition : returns the head of the list.
;; assuming head-helper works, it returns count,means it reutrns the head value




;___________________________________________________________________________________________________________________________________________



;;; Function 3 :
;;; -- more generally, a function ref which inputs a number n representing a list s and which returns the number
;;;    in the kth position of s
;; Author - Mohammed Fahad

 (define (ref  n k ) ( ref-helper n 0 (nthPrime k)))
  (define ( ref-helper n count prime )
     (cond
     (( > ( remainder n prime) 0 ) count)
      ( else (ref-helper ( quotient n prime ) ( + 1 count)  prime))))
;; testrun output : lets say list is list ( 32 9 25 ), (num s) = 7200, so input (ref 7200 1) = 32,
;; (ref 7200 2) = 9, (ref 7200 3) = 25. So it is able to return any nth position number of a list.


;;Proof :
;; takes input n representing a list s and k , and returns the kth position of the list s.
;;Design Idea: We will use a helper function ref-helper. ref-helper takes input n and also two input count
;; and prime. n is the same n represnting list s , count's design role is same as previous head function,
;; and prime is the kth prime number generated by the previously discussed function nthPrime, which generates
;; nth Prime number. Using all these arguments, we divide try to find how many times we can divide n with kth
;; prime without a remainder. And we count this inside the count variable and return count. Then our ref
;; funtion uses this count and reutrns expt (nthPrime k)  count that is (kthPrime^count) which is our
;; kth number of the list.

;;proof of ref-helper :
;;Precondition: Input n is an integer representing  list s , and k is the kth index, which is a non zero positive integer.
;;Postcondition: Outputs returns an integer count such as kthPrime ^ count = kth number of list
;;Basis Step: The basis step would be for an input n where remainder of n divided by prime is greater than 0,
; and count returns
;;Induction Hypothesis: Given that the precondition holds, 
;;assume that the recursive call holds
;;Induction Step: Each call of function will increment the count and return count
;;Termination Argument: The program terminates upon the function call when remainder of n and prime > 0

;; proof of ref : precondition : Inputs a number n representing a list
;; postcondition : returns the kth number  of the list.
;; assuming ref-helper works, it returns reutrns the kth value
;; Note that if input k is such an index that's out of range means it's greater than the legth of our list, the function will simply return 0
;; ( as the initialized count is 0 and k>index will trigger the base step and return count immediately which is 0). We are not restricting the
;; k with pre condition such that k should be less than length of list, we are not imposing this preconditon restriction as later we can use this property
;; to determine the length of our list such as : we will try to return kth value beginning at the first value, and we will increment k each time,
;; whenever this function returns 0 we will know that our list is out of index and we could simply return k-1 which would be our length of list.






;___________________________________________________________________________________________________________________________________________


;;; Function 4:
;;; -- a function tail which inputs a number n which represents a list s and which returns the number representing the tail
;;;    of s, that is, the list obtained from s by removing its first element
;; Author - Mohammed Fahad

 (define (tail n) ( / n ( expt 2 (head n))))

;; testrun output : lets say list is list ( 11 3 2 ), (num s) =1382400, so input (tail 1382400) = 675,
;;so tis function returs number representaion of list obtained by removing its first element (11 in this case)

;;Proof
;; takes input n representing a list s return number represntation of list obtained by removing its head
;;Design Idea: We can use the head function here , head function return the head of the list, and since
;; our representation is obtained by multiplying each nth prime to nth numbers, if we multiply
;; number n with 2^head (or ( / n ( expt 2 (head n)))), the postcondition will be satisfied
;;Precondition: Input n is an integer representing  list s 
;;Postcondition: Outputs returns the number obtained by removing head from the list represntaion of n
;; We haved proved the function head, which returns the head of a list where the input is n , the number
;; representaion of the list n. So by calling head n , we are obtaining the head value, and if we
;; divide n by 2^ head, we are getting our number representaion of list s by removing it's head.




;___________________________________________________________________________________________________________________________________________

;;; Function 5 :
;;; -- a function insert-at-head which inputs a number n representing a list s and a second number p, and which returns the number
;;;    representing the list obtained by inserting p at the head of the list s
;; Author - Mohammed Fahad

(define ( insert-at-head-helper n new-n count length p )
    (cond
      (( = count ( + 1 length) ) ( * new-n ( expt 2 p)))
      
      ( else ( insert-at-head-helper n ( * new-n ( expt ( nthPrime ( + 1 count)) ( ref n count))) ( + 1 count) length p))))

(define (insert-at-head n p) ( insert-at-head-helper n 1 1 ( len n) p))

;;test run output example : lets say our list is list (2 3 4), so (num s) is 67500. We want to insert 5 at the head. (insert-at-head)
;; returns 86436000, which is obtained by 2^5 * 3^2 * 5^3 * 7^4, means 5 has been added to the head and each other number has shifted by 1.

;; Proof:
;;Design Idea:We will be using insert-at-head helper function and describe each of it's variables roles now. To add at head, we not only need to add the number at the beginning but also shift each numbers 1 index to the right. So we can
;; use our len function to find the length of the list, and keep a variable count which starts from 1 and go upto length. For each iteration, we
;; can find the count th item in the list, starting from the first item using the ref function, and using the nthPrime function, we can get hold of the
;; count+1 th prime number, and calculate (count+1 th prime)^(count th number). This will shift the current number at count index 1 to the right, and
;; we can keep track of n using another variable called new n, where each of the new number generated after shifted nth to the right gets multiplied,
;; and when count reached the length, we simply multiply new-n with 2^p, where p is the number to be added to the head.

;;proof of insert-at-head-helper :
;;Design Roles: Input n represents list s, new-n is the representation of list at each function call obtained by shifting numbers
;; 1 index to the right.count is the variable keeping track of each indices moving to the right upto the length of the list
;; length is the length of our list s before adding p to its head obtained by calling len function, q is the number to be added to the head.
;;Function return: Returns integer which is the representaion of new list obtained by adding p to its at
;;Basis Step: Since we are adding p to the head, the length of our list will be incremented by 1, and
;; The basis step would be for an input where count is equal to the length, which means we have shifted all digits 1 index to the right.
;;Induction Hypothesis: Given that the precondition holds, 
;;assume that the recursive call holds
;;Induction Step: Each call of function will shift the numbers of list 1 index to the right
;;Termination Argument: The program terminates when all numbers has been shifted to the right, in that case, after all numbers has been shifted 1
;; index to the right, new-n is containing the new representation of list where each number is  shifted to right and  before adding the p at head.
;; So now, it simply returns the new-n multiplied by 2^p, which would be the new representation of list s after adding p to the head
;;Precondition: Input n represents list s and q is the number to be added to the head.
;;Postcondition: Returns integer which is the representaion of new list obtained by adding q to its at





;___________________________________________________________________________________________________________________________________________

;;;Function 6 :
;;; -- a function len which inputs a number n which represents a list s and which returns the number of elements of s
;; Author - Mohammed Fahad

   ( define (len-helper n k)
     ( cond
     ( ( = (ref n k) 0 ) ( - k 1))
      ( else ( len-helper n ( + k 1)))))
   ( define (len n) (len-helper n 1))

;;test run example : lets say list = list ( 2 1 1 1 2 2) , so (num s) = 780780, (len 780780) = 6. So it returns the length of any list(number representation.)


;; Proof :
;; takes input n representing a list s return the number of elements in the list.
;; Design idea - From the previous discussion of ref function, we know that ref will return 0 whenever we are our of index of the list. So we can start
;; k at 1 and try to return 1st value of the list and we can increase the value of k at each call and try to see if we can return kth value.
;; The moment when ref k return 0, we know that we went out of index for this call, so we can simply  return k's value for previous call
;; means return k-1 and this will be our length of list.

;;proof of len-helper :
;;Precondition: Input n is an integer representing  list s
;;Postcondition: Outputs returns an integer ( k - 1) such that k-1 is the length of the list s
;; Since we have proved that our function ref is valid and can return any kth index value of our list, and when k is greater than the length of list,
;; ref simply return 0. We will use this fact to design our len and len-helper function.
;;Basis Step: The basis step would be for an  n when we try to find kth value from the list using ref and ref returns 0 means index went out of range
; and k for the previous call ( k-1) returns
;;Induction Hypothesis: Given that the precondition holds, 
;;assume that the recursive call holds
;;Induction Step: Each call of function will increment k and return k-1 or the call when k went out of range, that call's previous call's k value
;;Termination Argument: The program terminates upon the function call when ref function return 0 means our index went out of range and k exceeds list length

;; proof of len : precondition : Inputs a number n representing a list
;; postcondition : returns the length of the list
;; assuming len-helper works, it returns reutrns the length of our list.




;___________________________________________________________________________________________________________________________________________

;;; Function 7 :
;;; -- a function snoc which inputs a number n which represents a list s and a second number q, and which returns the number
;;;    representing the list obtained by inserting q at the end of the list s
;; Author - Mohammed Fahad


( define ( snoc n q) ( * n ( expt (nthPrime ( + 1 ( len n))) q)))
;;test run output example : lets say our list is list ( 2 1 1 1 1 2), so num s = 780780. we want to add 3 to the end.
;; so, (snoc 780780 3) returns the value 3835972140, which is 780780*17^3. So it can insert the number at the end of the list.


;; Proof :
;; takes input n representing a list s return the number of elements in the list.
;; Design Idea - We can take the list and find its length using previous len function. Then using the prime generator funtion nthPrime,
;; we can get the next prime number k, calculate k^q and multiply current n with k^q. That would be new number representation.

;;Precondition: Input n is an integer representing  list s, and the number q to be added at the end of the list
;;Postcondition: Outputs returns an integer representing list s obtained by inserting q at the end of the list s.
;; Since we have proved that our function len is valid and can return the length of  given list, and nthPrime function returns nth Prime number.
;; We have used these facts to design our snoc function.
;; This function inputs n and q, and finds the length of the list s, and using the nthPrime function, finds the next prime number and returns n*nthPrime^q,
;; which is our new representation of s obtained by adding s to the end of the list.
;; since this function basically uses nthPrime and len function, and we have proved these functions previously, where nthPrime generates nth prime number.
;; and len function returns the length of a list, so we can say our function snoc works by returning n such that n is n*kthPrime^q, where k is the new
;; length of the list after adding q obtained by using len function and adding 1 with the returned value of len.






;___________________________________________________________________________________________________________________________________________
;;; Function 8 :
;;; -- a function last which inputs a number n which represents a non-empty list s and which returns the rightmost element of s
;; Author - Mohammed Fahad


 ( define (last n) ( ref n (len n)))
;; test run example : Lets say our list is list (2 3 5 4), so (num s) = 810337500. We need to return last element that is 4.
;; (last 810337500) return 4, So this returns the last or rightmost element of s.

;;Proof :
;;Design Idea - We can use len function to find out the length of our list, call it k. And then simply call ref function to return kth element of the
;; the list, which would be the last or right most element of our list. Since our list is 1 based index, the length of the list is the index of last
;; element in our list, hence we just need to reutrn the length of list using len function and then using ref function, we can return the last index's
;; value.
;; Precondition : Input an integer number n which represents a list s
;; Postcondition : returns the rightmost or last element of the list
;; Since we previously proved our ref and len function, where len returns the length of a list and ref returns the value at nth index of a list,
;; we can say that our last function works because in the last function we call the length to find the length of our list and then we call ref to
;; return the value at that index means the value at the last index of our list, which satisfies the post condition.



;___________________________________________________________________________________________________________________________________________



;Function 9 :
;;; -- a function insert-at which inputs a number n representing a list s, a second number x, and a third number y and which returns
;;;    the number representing the list obtained by inserting x in the yth position of s.  You will need preconditions
;;;    to ensure that the number y makes sense as a position in s
;;; Author - Mohammed Fahad


;(define ( insert-at-helper n x y )
;    (cond
;      (( = 1 y ) (insert-at-head n x))
;      (( = ( + 1 (len n)) y ) (* n ( expt (nthPrime ( + 1 ( len n ))) x )))
;      ( else (isPrimeHelper x 2 ) )))

;(define (insert-at n x y) (insert-at-helper n x y))





;___________________________________________________________________________________________________________________________________________

;;;Function 10
;;; -- a function myappend which inputs numbers m and n representing lists s and t, respectively, and which returns the number
;;;    representing the list formed by appending s and t
;

;; Author - Mohammed Fahad
;(myappend m n)
;Precondition: input m and n represents a list and greater than or equal to 1
;Postcondition: Returns the join of the lists that n and m represent
;Design Idea: find the length of m and n, keep m as it is and then shift n by length of m and multiply it with m
;GI:at each step, return m*number where number represents list n obtained by already shifting them to the right
;Strong enough: when n is fully shifted to right, we have m*n where n has been completely shifted to the right by len m, so we have new appened list
;Weak enough :  when n is a single number's list, m*n represents n shifted to the right so we have m*n such as n is shifted to the right
; is GI preserved : on each call, it returns m*number where number is obtained by shifting n's digits to the right, so preserved


;___________________________________________________________________________________________________________________________________________

;;;Function 11
;;; -- a function myreverse which inputs a number representing a list s and which outputs the number representing the reverse
;;;    of s
;;; Author - Mohammed Fahad


;(define ( myreverse-helper n new-n count len new-len  )
;    (cond
;      (( = count  len ) new-n)
;      ( else (myreverse-helper n  ( * new-n ( expt (nthPrime count) ( ref n new-len))) ( + 1 count) len ( - 1 new-len)))))
;(define (myreverse n) ( myreverse-helper n 1 1 (len n) (len n)))
                               
;Precondition: input n is greater than 1 and represents a valid list
;Postcondition: returns and integer that represents the list s with reversed elements
; Design Idea: using a helper function, we keep track of the length of the list and at we start like this : 1st prime ^ last number ,,,
; second prime ^ second last number and we use ref to extract those numbers. At each iteration, new-n is number obtained by reversing
;; digits from last .
;(myreverse-helper n new-n count len new-len)
;Precondition: input n and rsf both represent valid lists, and count is greater than 0 but less than len of n
;Postcondition: returns the reversed list of n upto element count
; Design Roles : n is list representation, new-n is the number obtained by reversing list, count keeps track of how many numbers has
; been reversed, len is the length of n and new-len is the index starting from end of n
; base step : when count is same as length, means we have reversed all numbers, we exit the recursion
; IH : assuming recursion call works
; IS : reverse digits from the end


;___________________________________________________________________________________________________________________________________________

;Function 12
;;; -- a function palin? which inputs a number representing a list s and which determines whether s is a palindrome
;;; Author - Mohammed Fahad

;(define ( palin-helper n len index count result a b )
;   (cond
;      (( = count len ) result)
;      (( not ( = a b) ) #f)
;      ( else (palin-helper n len ( - 1 index) ( + 1 count) #t ( ref n ( + 1 count)) ( ref n ( - 1 index)) ))))

;(define (palin n) (palin-helper n ( len n) 4 1 #f ( ref n 1) ( ref n (len n))));

;(palin? n)
;Precondition: input n is greater that 1 and represents a valid list
;Postcondition: returns #t or #f depending on if the list represented by n is a palindrome or not
;Design Idea: Use a helper function to compare the first index of n
;with the n-count index of n

;(palin-helper n len index count result a b)
;Precondition: count is greater that 0
;Postcondition: #t or #f
;; Design Roles : n is list, len is length of list, index is the index of number being checked from end, count it index
;;ofnumber checking from front,
;; result is true or false, a is number being checked from head, b is number being checked from tail
;; base step : when  count = result means all number has been checked, we return the result either true or false
;; IH : assuming recursive call works, last call check numbers for being equal and returns true or false
;; IS : each call checks for numbers being equal or not 

;___________________________________________________________________________________________________________________________________________

;Function 13
;;; -- a function sort which inputs a number representing a list s and which outputs the number representing the list
;;;    formed by sorting (smallest to largest) the elements of s
;; Author - Mohammed Fahad

;(sort n)
;Precondition: input n is greater that 1 and represents a valid list
;Postcondition: returns the number inorder of sorting the list
;Design Idea: Use a helper function to compare the first index of n
;with the n-count index of n

;(sort-helper n count)
;Precondition: count is greater that 0
;Postcondition: #t or #f
;GI: At index count a pair of elements will be compared, index count and index (len n) - count to read through the front and back elements of n to check for equality.
;Strong Enough: On the first call of help-palin, index 1 will be compared to index (len n)-1 which is the last index of n
;Weak enough: help-palin terminates when count reaches len of n
;Preserable: Upon calling help-palin, count it incremented by 1
;each call so the GI will not be violated as long as the precondition holds


;___________________________________________________________________________________________________________________________________________
; Additional Helper functions to generate nth Prime number. Author - Mohammed Fahad

(define (isPrimeHelper x k)
  (if (= x k) x
  (if (= (remainder x k) 0) -1
      (isPrimeHelper x (+ k 1)))))

;Finds if x is a prime or not, returns x if prime and -1 if not a prime number. precond: x is non zero positive integer
(define ( isPrime x )
    (cond
      (( = x 1 ) -1)
      (( = x 2 ) x)
      ( else (isPrimeHelper x 2 ) )))

;finds the nth prime number. precond: n is non zero positive integer
(define (nthPrimeGen x count n)
  ( cond
     ( ( = count n) ( - x 1))

     ( ( = ( isPrime x)  x) ( nthPrimeGen ( + x 1) ( + count 1) n))

     ( (= ( isPrime x)  -1) ( nthPrimeGen ( + x 1) count n))
     ))

;; precond : n is a non zero positive integer, postcond : returns nth prime number
(define (nthPrime n)
  (nthPrimeGen 1 0 n))



;;

;; we cant use this method to represent list of  list and number because list might contain same numbers and we would have a factorization
;; problem. Same goes for list of lists ( for example list (list (123) list(123)) would cause factorization issues.


