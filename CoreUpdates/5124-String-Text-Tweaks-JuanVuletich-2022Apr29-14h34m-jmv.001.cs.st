'From Cuis 6.0 [latest update: #5121] on 29 April 2022 at 2:36:20 pm'!

!String methodsFor: 'accessing' stamp: 'jmv 4/26/2022 15:36:45'!
byteSize
	^self size! !

!String methodsFor: 'accessing' stamp: 'jmv 4/28/2022 15:10:55'!
isLastLineSeparator
	^self last isLineSeparator! !

!String methodsFor: 'accessing' stamp: 'jmv 4/28/2022 15:51:10'!
isLastSeparator
	^self last isSeparator! !


!Text methodsFor: 'accessing' stamp: 'jmv 4/28/2022 15:10:09'!
isLastLineSeparator
	^string isLastLineSeparator! !

!Text methodsFor: 'accessing' stamp: 'jmv 4/28/2022 15:51:16'!
isLastSeparator
	^string isLastSeparator! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 4/28/2022 17:14:48'!
composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

	lines _ argLinesCollection.
	theText _ argText.
	extentForComposing _ argExtentForComposing.
	deltaCharIndex _ argDelta.
	currCharIndex _ startCharIndex _ argStart.
	stopCharIndex _ argStop.
	prevLines _ argPriorLines.
	currentY _ argStartY.
	maxRightX _ 0.
	possibleSlide _ stopCharIndex < theText size.
	nowSliding _ false.
	prevIndex _ 1.
	scanner _ CompositionScanner new text: theText.
	scanner defaultFont: self defaultFont.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed _ false.
	self composeAllLines.
	(anythingComposed not or: [ theText isLastLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine: true ]
		ifFalse: [
			scanner lastLineBreakingSpace = theText size ifTrue: [
				self addEmptyTrailingLine: false ]].
	^ {lines asArray. maxRightX}

! !

