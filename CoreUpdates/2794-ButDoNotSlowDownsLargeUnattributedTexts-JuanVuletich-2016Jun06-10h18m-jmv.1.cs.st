'From Cuis 4.2 of 25 July 2013 [latest update: #2793] on 6 June 2016 at 10:30:50.522404 am'!

!RunArray methodsFor: 'testing' stamp: 'jmv 6/6/2016 10:29'!
is: aSymbol
	^aSymbol == #RunArray or: [ super is: aSymbol ]! !


!Text methodsFor: 'converting' stamp: 'jmv 6/6/2016 10:29'!
optimizedForMutationSpeed
	"Do not use RunArray. Optimized for extensive attribute modification."
	
	(runs is: #RunArray) ifTrue: [
		^Text string: string runs: (RunNotArray withAll: runs) ]! !


!Text methodsFor: 'converting' stamp: 'jmv 9/24/2010 08:19'!
replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| newRepRuns |
	string replaceFrom: start to: stop with: replacementString startingAt: repStart.
	newRepRuns _ RunArray new: stop-start+1 withAll: attributesArray.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacementString size - 1! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 6/6/2016 10:18'!
string: aString attributes: atts
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunArray new: aString size withAll: atts)! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/6/2016 10:30'!
formatAndStyle
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText _ textModel actualContents optimizedForMutationSpeed.
	self privateFormatAndConvert.
	textModel basicActualContents: formattedText.
	formattedText size < 4096
		ifTrue: [
			self privateStyle.
			textModel changed: #shoutStyled ]
		ifFalse: [ self styleInBackgroundProcess ].! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/6/2016 10:28'!
formattedAndStyledText
	"Answer a copy of the actualContents in the model that is both formatted and styled"

	formattedText := textModel actualContents optimizedForMutationSpeed.
	self privateFormatAndConvert.
	self privateStyle.
	^formattedText! !


!RunArray reorganize!
('accessing' = at: canJoinMessage: first last runLengthAt: size withStartStopAndValueDo:)
('adding' addFirst: addLast: coalesce rangeOf:startingAt:)
('copying' , copy copyFrom:to: copyReplaceFrom:to:with:)
('printing' printOn: storeOn: writeOn:)
('private' at:setRunOffsetAndValue: basicReplaceAttributesFrom:to:with: canJoin:and: find: mapValues: runs setRuns:setValues: values)
('enumerating' runsAndValuesDo: runsFrom:to:do:)
('converting' reversed)
('testing' is:)
!

