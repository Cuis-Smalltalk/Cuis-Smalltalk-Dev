'From Cuis 5.0 [latest update: #4649] on 29 June 2021 at 11:26:31 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 6/29/2021 11:23:28'!
selectWordOrDelimitedText
	"Select delimited text or word--the result of double-clicking."

	^self selectWordLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters ! !


!Editor methodsFor: 'new selection' stamp: 'jmv 6/29/2021 11:21:01'!
selectWord
	"Select exactly one word.
	See also #selectWordOrDelimitedText"

	^self selectWordLeftDelimiters: '' rightDelimiters: ''! !


!TextEditor methodsFor: 'events' stamp: 'jmv 6/29/2021 11:23:47'!
clickAndHalf

	self selectWordOrDelimitedText.

	doWordSelection _ true.
	doParagraphSelection _ false.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !
