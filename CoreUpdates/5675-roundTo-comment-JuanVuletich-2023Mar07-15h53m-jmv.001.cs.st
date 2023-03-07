'From Cuis 6.0 [latest update: #5674] on 7 March 2023 at 3:56:05 pm'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 3/7/2023 15:55:15'!
roundTo: quantum
	"Answer the nearest number that is a multiple of quantum.

	Note: Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category.

	Note: You may get surprising results if the desired quantum is not representable as a
	Floating Point number. Instead of getting the closest Float to the correct answer, you'll likely get
	an answer that is a few Floats away. 	For instance, compare:
		4.808 roundTo: 1/100
		(4.808 roundTo: 1/100) asFloat
		4.808 roundTo: 0.01
	"

	^ (self / quantum) rounded * quantum! !

