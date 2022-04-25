'From Cuis 6.0 [latest update: #5114] on 25 April 2022 at 11:13:18 am'!

!Object methodsFor: 'printing' stamp: 'jmv 4/25/2022 10:36:41'!
shortPrintString
	"Answer a String whose characters are a description of the receiver.
	This is a short one, good for showing users (for example, in ObjectExplorers).
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 128.! !

