'From Cuis7.9 [latest update: #8299] on 28 October 2026 at 6:59:26 pm'!

!Object methodsFor: 'printing' stamp: 'sqr 13/Aug/2016, 4:25:43 pm (UTC)'!
printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit! !

