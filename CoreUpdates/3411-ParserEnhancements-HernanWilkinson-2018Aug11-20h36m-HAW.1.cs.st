'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 8:37:35 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:37:20'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].
	hereType == #word ifTrue: [^self unaryPattern ].
	self transformVerticalBarAndUpArrowIntoABinarySelectorIfNecessary.
	hereType == #binary ifTrue: [^self binaryPattern ].
	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: 'Message pattern'
! !

