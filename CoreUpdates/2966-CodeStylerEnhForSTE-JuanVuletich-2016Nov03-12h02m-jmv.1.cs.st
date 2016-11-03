'From Cuis 4.2 of 25 July 2013 [latest update: #2965] on 3 November 2016 at 12:02:37 pm'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 11/3/2016 11:22:59'!
formatAndStyleWith: anSHTextStyler
	anSHTextStyler formatAndStyle: self actualContents allowBackgroundStyleProcess: true.
	self basicActualContents: anSHTextStyler formattedText! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 11/3/2016 11:22:52'!
formatAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [ self styleInBackgroundProcess ]
		ifFalse: [
			self privateStyle.
			textModel changed: #shoutStyled ]! !

!SHTextStyler methodsFor: 'accessing' stamp: 'jmv 11/3/2016 11:21:08'!
formattedText
	^ formattedText! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 11/3/2016 11:15:39'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean.
		owner redrawNeeded].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	"shout:  re-style the text iff aBoolean is true
	Do not apply any formatting (i.e. changes to the characters in the text),
	just styling (i.e. TextAttributes)"
	(aBoolean and: [self okToStyle])
		ifTrue: [ model formatAndStyleWith: styler ]! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 11/3/2016 11:15:47'!
formatAndStyleIfNeeded
	"Apply both formatting (changes to the characters in the text, such as
	preferred assignment operators), and styling (TextAttributes to make
	Smalltalk code easier to understand)"

	self okToStyle ifTrue: [
		 model formatAndStyleWith: styler ]! !

!methodRemoval: SHTextStyler #formatAndStyle!
SHTextStyler removeSelector: #formatAndStyle!
