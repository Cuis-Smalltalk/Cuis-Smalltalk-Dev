'From Cuis 6.0 [latest update: #6073] on 17 November 2023 at 10:38:13 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:37:03'!
selectWordOrDelimitedText
	"Select delimited text or word--the result of double-clicking."

	^self
		selectWordLeftDelimiters: '([{<|''"`'
				rightDelimiters: ')]}>|''"`'.! !

!methodRemoval: SmalltalkEditor #wordLeftDelimiters stamp: 'jmv 11/17/2023 10:37:07'!
SmalltalkEditor removeSelector: #wordLeftDelimiters!
!methodRemoval: SmalltalkEditor #wordRightDelimiters stamp: 'jmv 11/17/2023 10:37:09'!
SmalltalkEditor removeSelector: #wordRightDelimiters!
!methodRemoval: Editor #wordLeftDelimiters stamp: 'jmv 11/17/2023 10:37:07'!
Editor removeSelector: #wordLeftDelimiters!
!methodRemoval: Editor #wordRightDelimiters stamp: 'jmv 11/17/2023 10:37:10'!
Editor removeSelector: #wordRightDelimiters!
