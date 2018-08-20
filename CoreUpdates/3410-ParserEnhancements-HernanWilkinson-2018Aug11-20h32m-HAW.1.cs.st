'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 8:36:31 pm'!

!Parser methodsFor: 'scanning' stamp: 'HAW 8/11/2018 20:36:10'!
transformVerticalBarAndUpArrowIntoABinarySelectorIfNecessary
	
	(hereType == #verticalBar or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].! !


!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 20:33:48'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].

	hereType == #word ifTrue: [^self unaryPattern ].

	(hereType == #verticalBar or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [^self binaryPattern ].

	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: 'Message pattern'
! !

