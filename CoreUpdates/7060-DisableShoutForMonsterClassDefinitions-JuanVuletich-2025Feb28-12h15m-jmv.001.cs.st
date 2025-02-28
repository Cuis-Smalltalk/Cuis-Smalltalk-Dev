'From Cuis7.3 [latest update: #7059] on 28 February 2025 at 12:24:29 pm'!

!PluggableTextModel methodsFor: 'accessing' stamp: 'jmv 2/28/2025 12:14:09'!
basicReplaceFrom: start to: stop with: replacement
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	"If we style text (as usual for Smalltalk code), but text is huge, assume plain String is OK.
	This was needed to accomodate class definition with thousands of instance variables,
	meaning a class definition with a single word-wrapped line of several hundred thousand character.
	It makes no sense to shout that, and plain String is composed much faster."
	(self stylesText and: [actualContents size + replacement size > self textStylerClass maxSizeForStyling ]) ifTrue: [
		actualContents := actualContents asString copyReplaceFrom: start to: stop with: replacement asString.
		actualContents := actualContents asText.
		^self ].

	^super basicReplaceFrom: start to: stop with: replacement! !


!SHTextStyler class methodsFor: 'constants' stamp: 'jmv 2/28/2025 12:23:14'!
maxSizeForStyling
	"Rather arbitrary, but reasonable."
	^20000! !

!SHTextStyler class methodsFor: 'constants' stamp: 'jmv 2/28/2025 12:13:44'!
minSizeForBackgroundStyling
	"Rather arbitrary, but reasonable."
	^4096! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 2/28/2025 12:14:21'!
convertAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self getReady.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	(aBoolean and: [formattedText size >= self class minSizeForBackgroundStyling])
		ifTrue: [
			formattedText size <= self class maxSizeForStyling ifTrue: [
				self convertAndStyleInBackgroundProcess ]]
		ifFalse: [
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			textModel changed: #shoutStyled ]! !

