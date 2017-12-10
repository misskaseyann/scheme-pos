; Tax value is 6.5%
(define tax 0.065)

; Adds spaces between each item to be displayed.
; Original code by user Jack from stackoverflow.
; Link to original code snippet is here:
; https://stackoverflow.com/questions/26539585/
; how-to-display-multiple-parameters-in-r5rs-scheme
(define (insert-between v xs)
	(cond ((null? xs) xs)
		((null? (cdr xs)) xs)
		(else (cons (car xs)
			(cons v (insert-between v (cdr xs)))))))

; Displays each item in a list given while adding
; a space to invetween each.
; Original code by user Jack from stackoverflow.
; Link to original code snippet is here:
; https://stackoverflow.com/questions/26539585/
; how-to-display-multiple-parameters-in-r5rs-scheme
(define (display-all . vs)
	(for-each display (insert-between " " vs)))

; POS System for calculating multiple transactions.
(define pos (lambda ()
	(letrec
		; Strings displayed to the user.
		((start "\n\nScheme Point-of-Sale\nStart transaction (exit with 0): \n$")
		(next "\nNext transaction: \n$")
		(subtotal "\nYour subtotal is: \n$")
		(taxes "\nTax is: \n$")
		(total "\nTotal is: \n$")
		; Read and display a string.
		(read-in (lambda (strdisp)
			(display strdisp)
			(read)))
		; Takes starting balance, a new transaction, and gives sum.
		; If user types 0, the program will tally and exit.
		(sum (lambda (strt tran)
			(if (= tran 0)
				(display-all subtotal strt taxes (* strt tax) total (+ strt (* strt tax)) "\n\n\n")
				(transaction (+ strt tran)))))
		; Asks for the next transaction and passes the value to sum.
		(transaction (lambda (strt)
			(sum strt (read-in next)))))
		; Starts the pos system.
	(transaction (read-in start)))))