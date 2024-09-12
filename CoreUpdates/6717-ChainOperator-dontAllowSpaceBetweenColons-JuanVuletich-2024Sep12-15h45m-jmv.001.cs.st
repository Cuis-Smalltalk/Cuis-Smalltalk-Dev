'From Cuis7.1 [latest update: #6716] on 12 September 2024 at 3:45:55 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 9/12/2024 15:44:03'!
chain
	" {: message} => more messages"

	self advance.
	"The following three lines implement double colon chains"
	hereType == #colon ifFalse: [self expected: 'Chain'].
	hereMark-prevMark = 1 ifFalse: [self expected: 'Chain'].
	self advance.

	(self messagePart: 3 repeat: true) ifFalse: [self expected: 'Chain'].
	hereType == #semicolon ifTrue: [self cascade].
	hereType == #colon ifTrue: [self chain]! !

