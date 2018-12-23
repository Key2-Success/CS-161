; QUESTION 1
; purpose: this function will return true if the number N is present in the TREE
; purpose: if the number is not present in the tree, then it will return NIL
; solution: recurse through children unil atomic value which is base case
; input: N is a number. TREE is a tree
; output: T or NIL based on if N is in TREE
(defun TREE-CONTAINS (N TREE)
	(cond ((atom TREE) ; base case: if tree is atom/a number
		       (if (= TREE N) T NIL)) ; if the number is the tree, return true, else return NIL

		  ; induction case: recurse through children until atomic value
		  (( = N (second TREE)) T) ; if the root number is N, then return true
	  	  (( < N (second TREE)) (TREE-CONTAINS N (first TREE))) ; if N is less than root, then recurse on left sub-tree
		  (( > N (second TREE)) (TREE-CONTAINS N (third TREE))) ; if N is greater than root, then recurse on right sub-tree
	)
)

; QUESTION 2
; purpose: this function will return the minimum value from the tree
; solution: recurse through tree through left sub-child until number to reach min value
; input: TREE is a tree
; output: minimum value in TREE
(defun TREE-MIN (TREE)
	(cond ((atom TREE) TREE) ; base case: the tree is a number, so return number
		  (T (TREE-MIN (first TREE))) ; induction case: the tree is proper, so recurse on left child
	)
)

; QUESTION 3
; purpose: this function returns the tree in pre-order format
; solution: recurse through root node, left sub-tree, then right sub-tree until number to output in pre-order format
; input: TREE is a tree
; output: pre-ordered list of the numbers from TREE
(defun TREE-ORDER (TREE)
	(cond ((atom TREE) (list TREE)) ; base case: if the tree is a number, return it the number as a list
		  (T (append (TREE-ORDER(second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)))) ; induction case: recurse by going to root node, then left child, then right child
	)
)

; QUESTION 4
; purpose: this function takes a list and returns a sub-list based on starting position and length of sub-list
; solution: depending on START and LEN values, recurse through the the list until L is empty or LEN becomes 0
; input: L is a list. START is the first value of the sublist. LEN is the length of the sublist. both START and LEN are non-negative integers.
; output: a sub-list of L that begins at START and has length LEN.
(defun SUB-LIST(L START LEN)
	(cond ((NULL L) NIL) ; if list is empty, return NIL
		  (( = LEN 0) NIL) ; if length is 0, return NIL
		  (( = START 0) (cons (car L) (SUB-LIST(cdr L) 0 (- LEN 1)))) ; if starting at 0, append until length (length - 1 since starting at position 0)
		  (( > START 0) (SUB-LIST(cdr L) (- START 1) LEN)) ; if not starting at 0, search until start and stop until length
	)
)

; QUESTION 5
; purpose: this function splits a list. if it is an even element list, it splits it into 2 equal groups. 
; purpose: if the list has odd number of elements, the first list has 1 more element. 
; purpose: the output is one list joined by the two split lists.
; solution: find list length, then depending on if list is even or odd in length, append two lists of equal length by using SUB-LIST function
; input: L is a list.
; output: returns a list of two lists L1 and L2 such that L = L1 + L2 once appended. both lists are of equal length, unless L is odd, in which case, L1 is longer by 1.
(defun SPLIT-LIST(L)
	(let* ((size (length L))) ; assign value of list length to size
		(if (evenp size) ; if list is even, return two lists of equal lengths appended to each other
			(list (SUB-LIST L 0 (/ size 2)) (SUB-LIST L (/ size 2) (/ size 2))) ; if list even, sub list of first half and of second half
			(list (SUB-LIST L 0 (/ (- size 1) 2)) (SUB-LIST L (/ (- size 1) 2) (/ (+ size 1) 2))) ; if list odd, first half has 1 more
		)
	)	
)

; QUESTION 6
; purpose:  function finds the height of a binary tree by finding the maximum height between the two sub-trees
; solution: recurse through left and right sub-tree and find maximum height until atom is reached
; input: TREE is a tree
; output: returns the height of TREE as defined as longest path from root node to farthest leaf node
(defun BTREE-HEIGHT(TREE)
	(cond ((atom TREE) 0) ; if tree is just one root, height is 0
		  (T (let* ((left (BTREE-HEIGHT (first TREE))) (right (BTREE-HEIGHT (second TREE)))) ; find heights of left and right sub-trees
				(if (> left right) (+ left 1) (+ right 1))) ; return the height of larger sub-tree
		  )
	)
)	

; QUESTION 7
; purpose: this function takes a list and returns a binary tree
; solution: recurse through elements using SPLIT-LIST function until leaves is an atom, creating a binary tree
; input: LEAVES is a non-empty list of atoms
; output: returns a binary tree such that the tree leaves are the elements of LEAVES and that the left sub-tree has 1 more child if there are odd children.
(defun LIST2BTREE (LEAVES)
  (cond ((atom LEAVES) LEAVES) ; if leaves is an atom, return it
        ((listp LEAVES) ; if leaves is a list
        	(cond ((= (length LEAVES) 1) (car LEAVES)) ; if list is of length 1
                              ((= (length LEAVES) 2) LEAVES) ; if length 2
                              (T (let ((btree (SPLIT-LIST LEAVES))) ; else if leaves has more than 2 elements, assign btree (binary tree)
                                   (list (LIST2BTREE (car btree)) (LIST2BTREE (cadr btree))))) ; return binary tree
            )
       	)
        (T NIL) ; if neither atom nor a list
   )
)

; QUESTION 8
; purpose: this function is the opposite of LIST2BTREE since it returns the list of a binary tree
; solution: recurse through tree until tree is empty or is an atom, then return as list
; input: TREE is a binary tree
; output: a list of atoms from TREE
(defun BTREE2LIST (TREE)
       (cond ((NULL TREE) NIL) ; if tree is empty
             ((atom TREE) (cons TREE NIL)) ; if tree is a number, return atom as list
             (T (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE)))) ; if tree has more than 1 element, append as list
       )
)

; QUESTION 9
; purpose: this function outputs whether two lists are of the same type, given the same atomic values
; solution: recurse through both lists by checking if both are same type. if not, then return NIL.
; input: E1 and E2 are two LISP expressions whose atoms are all numbers
; output: T if both expressions are equal and NIL if they are not the same type
(defun IS-SAME (E1 E2)
  (cond ((and (null E1) (null E2)) T) ; both are empty, so return T
        ((and (numberp E1) (numberp E2)) (= E1 E2)) ; both are numbers, so return T
        ((and (listp E1) (listp E2)) (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))) ; both are lists, so return T
        (T NIL) ; otherwise, they are not the same type, so return NIL
   )
)