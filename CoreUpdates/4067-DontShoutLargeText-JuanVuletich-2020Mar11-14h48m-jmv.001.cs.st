'From Cuis 5.0 [latest update: #4064] on 11 March 2020 at 2:59:20 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 3/11/2020 14:57:11'!
formatAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self styleInBackgroundProcess ]]
		ifFalse: [
			self privateStyle.
			textModel changed: #shoutStyled ]! !

