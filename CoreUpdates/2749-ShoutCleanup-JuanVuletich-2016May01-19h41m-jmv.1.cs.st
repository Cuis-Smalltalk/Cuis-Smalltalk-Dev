'From Cuis 4.2 of 25 July 2013 [latest update: #2748] on 1 May 2016 at 7:53:32.297688 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:50'!
rangesSetWorkspace: aBoolean
	"Answer a collection of SHRanges by parsing aText.
	When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace"

	parser ifNil: [parser _ SHParserST80 new].
	^parser 
		rangesIn: formattedText asString 
		classOrMetaClass: classOrMetaClass 
		workspace: (aBoolean ifTrue:[workspace])! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:49'!
replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"
	| toReplace increaseInLength |

	"We don't handle format and conversion for debuggers"
	disableFormatAndConvert ifTrue: [ ^self ].
	
	toReplace := (self rangesSetWorkspace: false) 
		select: [:each | each rangeType = aSymbol].
	toReplace isEmpty ifTrue: [^self].
	increaseInLength := 0.

	(toReplace asArray sort: [:a :b | a start <= b start]) 
		do: [:each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:47'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]]]! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/1/2016 19:49'!
formatAndStyle
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText _ textModel actualContents.
	self privateFormatAndConvert.
	textModel basicActualContents: formattedText.
	formattedText size < 4096
		ifTrue: [
			self privateStyle.
			textModel changed: #shoutStyled ]
		ifFalse: [ self styleInBackgroundProcess ].! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:45'!
convertAssignmentsToAnsi
	"If the Preference is to show ansiAssignments then answer a copy of  <aText> where each  left arrow assignment is replaced with a ':=' ansi assignment. A parser is used so that each left arrow is only replaced if it occurs within an assigment statement"

	self replaceStringForRangesWithType: #assignment with: ':='! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:45'!
convertAssignmentsToLeftArrow
	"If the Preference is to show leftArrowAssignments then answer a copy of  <aText> where each ansi assignment (:=) is replaced with a left arrow. A parser is used so that each ':=' is only replaced if it actually occurs within an assigment statement"

	self replaceStringForRangesWithType: #ansiAssignment with: '_'! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/1/2016 19:47'!
privateStyle
	| ranges |
	ranges _ self rangesSetWorkspace: true.
	ranges ifNotNil: [ self setAttributesFromRanges: ranges ]! !

!methodRemoval: SHTextStylerST80 #privateStyle:!
SHTextStylerST80 removeSelector: #privateStyle:!
!methodRemoval: SHTextStylerST80 #rangesIn:setWorkspace:!
SHTextStylerST80 removeSelector: #rangesIn:setWorkspace:!
!methodRemoval: SHTextStylerST80 #replaceStringForRangesWithType:with:in:!
SHTextStylerST80 removeSelector: #replaceStringForRangesWithType:with:in:!
!methodRemoval: SHTextStylerST80 #setAttributesFromRanges:in:!
SHTextStylerST80 removeSelector: #setAttributesFromRanges:in:!
!methodRemoval: SHTextStylerST80 #setAttributesIn:fromRanges:in:!
SHTextStylerST80 removeSelector: #setAttributesIn:fromRanges:in:!
!methodRemoval: SHTextStylerST80 #setAttributesIn:fromRasnges:in:!
SHTextStylerST80 removeSelector: #setAttributesIn:fromRasnges:in:!
