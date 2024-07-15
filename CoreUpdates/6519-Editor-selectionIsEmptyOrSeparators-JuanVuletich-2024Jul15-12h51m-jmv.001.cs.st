'From Cuis7.1 [latest update: #6518] on 15 July 2024 at 12:52:26 pm'!

!Editor methodsFor: 'accessing-selection' stamp: 'jmv 7/15/2024 12:51:29'!
selection

	self subclassResponsibility ! !


!Editor methodsFor: 'accessing-selection' stamp: 'jmv 7/15/2024 12:51:35'!
selectionIsEmptyOrSeparators
	"Answer true if no selection or selection is just separators (whitespace)"

	self hasSelection ifFalse: [ ^true ].
	^self selection isSeparators.
! !

!methodRemoval: TextEditor #selectionIsEmptyOrSeparators stamp: 'jmv 7/15/2024 12:51:41'!
TextEditor removeSelector: #selectionIsEmptyOrSeparators!
