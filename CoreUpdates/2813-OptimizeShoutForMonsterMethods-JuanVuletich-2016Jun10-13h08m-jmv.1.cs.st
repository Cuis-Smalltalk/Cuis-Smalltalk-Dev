'From Cuis 4.2 of 25 July 2013 [latest update: #2811] on 10 June 2016 at 1:36:47.926161 pm'!

!RunNotArray methodsFor: 'private' stamp: 'jmv 6/10/2016 13:08'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Copied from Array"
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !


!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/10/2016 13:09'!
basicReplaceAttributesFrom: start to: stop with: replacement

	self replaceFrom: start to: stop with: replacement startingAt: 1 ! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/10/2016 13:19'!
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

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 6/10/2016 13:18'!
formattedAndStyledText
	"Answer a copy of the actualContents in the model that is both formatted and styled"

	formattedText := textModel actualContents.
	self privateFormatAndConvert.
	self privateStyle.
	^formattedText! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/10/2016 13:19'!
privateStyle
	| ranges |
	ranges _ self rangesSetWorkspace: true.
	ranges ifNotNil: [ self setAttributesFromRanges: ranges ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/10/2016 13:36'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	(formattedText size > 2000 and: [ ranges size < 50 ]) ifFalse: [
		formattedText _ formattedText optimizedForMutationSpeed.
		textModel basicActualContents: formattedText ].

	ranges do: [ :range |

		"Smalltalk text styling"
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		"Show as subscripts if appropriate."
		(classOrMetaClass isNil or: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript]) ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]! !


!RunNotArray reorganize!
('accessing' at: canJoinMessage: rangeOf:startingAt: runLengthAt:)
('adding' coalesce)
('enumerating' runsAndValuesDo: runsFrom:to:do:)
('Text support' basicReplaceAttributesFrom:to:with: find: mapValues: values)
('private' replaceFrom:to:with:startingAt:)
!

