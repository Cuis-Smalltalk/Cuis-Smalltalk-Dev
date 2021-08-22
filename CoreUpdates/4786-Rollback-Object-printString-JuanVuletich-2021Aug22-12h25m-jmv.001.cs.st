'From Cuis 5.0 [latest update: #4785] on 22 August 2021 at 12:26:01 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 1/10/2014 22:53'!
printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 50000! !

