'From Cuis 4.2 of 25 July 2013 [latest update: #1935] on 10 January 2014 at 11:34:03.707431 pm'!

!SmallInteger commentStamp: 'jmv 1/10/2014 22:34' prior: 0!
My instances are 31-bit numbers, stored in twos complement form. The allowable range is approximately +- 1 billion (see SmallInteger minVal, maxVal).

Of the various classes in the Number hierarchy, SmallInteger gives:
- Maximum performance
- Top precision
- Restricted possible values

LargePositive(Negative)Integer and Fraction give increasing generality (more possible values) at the expense of performance.

Float gives more generality at the expense of precision.

Please see the class comments of the other Number classes.!


!Object methodsFor: 'converting' stamp: 'jmv 1/10/2014 22:56'!
asString
	"Answer a string that represents the receiver.
	Don't include extra quotes for Strings.

	This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray,  the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired).

	It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day.

	In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability.

	In any case, in your code, if possible, use a more specific method.

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printString ! !

!Object methodsFor: 'printing' stamp: 'jmv 1/10/2014 22:41'!
displayStringOrText
	"To be used in the UI. Answer might be an instance of Text if appropriate.
	Don't include extra quotes for Strings.
	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^self printString! !

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


!Number methodsFor: 'truncation and round off' stamp: 'jmv 1/10/2014 22:20'!
roundTo: quantum 
	"Answer the nearest number that is a multiple of quantum."
	"Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category."

	^ (self / quantum) rounded * quantum! !

!Number methodsFor: 'truncation and round off' stamp: 'jmv 1/10/2014 22:20'!
truncateTo: aNumber 
	"Answer the next multiple of aNumber toward zero that is nearest the 
	receiver."
	"Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category."
	"Examples:
		3.1479 truncateTo: 0.01 -> 3.14
		3.1479 truncateTo: 0.1 -> 3.1
		1923 truncateTo: 10 -> 1920
		3.1479 truncateTo: 0.005 -> 3.145
		-3.1479 truncateTo: 0.01 -> -3.14"

	^ (self quo: aNumber) * aNumber! !

