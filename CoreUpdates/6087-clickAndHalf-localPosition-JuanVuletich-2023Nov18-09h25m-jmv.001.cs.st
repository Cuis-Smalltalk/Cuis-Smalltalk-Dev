'From Cuis 6.0 [latest update: #6079] on 18 November 2023 at 9:30:04 am'!

!TextEditor methodsFor: 'events' stamp: 'jmv 11/17/2023 17:23:38'!
clickAndHalfAt: localEventPosition

	self selectWordOrDelimitedTextAt: localEventPosition.

	doWordSelection := true.
	doParagraphSelection := false.
	initialSelectionStart := self startBlock.
	initialSelectionStop := self stopBlock! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 11/18/2023 09:27:02'!
selectWordOrDelimitedTextAt: localEventPosition
	"Select delimited text or word--the result of double-clicking."

	| wordRange |
	wordRange := self
		wordRangeLeftDelimiters: '([{<|''"`'
				rightDelimiters: ')]}>|''"`'.
	self selectFrom: wordRange first to: wordRange last.! !


!SmalltalkEditor methodsFor: 'events' stamp: 'jmv 11/17/2023 17:20:17'!
clickAndHalfAt: localEventPosition

	| here |
	here := self pointIndex.
	(here between: 2 and: model textSize)
		ifTrue: [
			super clickAndHalfAt: localEventPosition ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ]! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 11/17/2023 17:20:52'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor clickAndHalfAt: localEventPosition ].
	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ].! !

!methodRemoval: SmalltalkEditor #clickAndHalf stamp: 'jmv 11/18/2023 09:28:13'!
SmalltalkEditor removeSelector: #clickAndHalf!
!methodRemoval: TextEditor #clickAndHalf stamp: 'jmv 11/18/2023 09:28:18'!
TextEditor removeSelector: #clickAndHalf!
!methodRemoval: Editor #selectWordOrDelimitedTextAt: stamp: 'jmv 11/18/2023 09:27:33'!
Editor removeSelector: #selectWordOrDelimitedTextAt:!
!methodRemoval: Editor #selectWordOrDelimitedText stamp: 'jmv 11/18/2023 09:28:25'!
Editor removeSelector: #selectWordOrDelimitedText!
