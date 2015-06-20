'From Cuis 4.2 of 25 July 2013 [latest update: #2367] on 19 June 2015 at 8:56:29.299286 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'pb 6/19/2015 20:55'!
formatAndStyle
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText := textModel actualContents.
	self privateFormatAndConvert.
	textModel basicActualContents: formattedText.
	formattedText size < 4096
		ifTrue: [
			formattedText := textModel actualContents copy.
			self privateStyle.
			textModel stylerStyled: formattedText checkForChanges: false ]
		ifFalse: [ self styleInBackgroundProcess ].! !

!SHTextStyler methodsFor: 'styling' stamp: 'pb 6/19/2015 20:54'!
formattedAndStyledText
	"Answer a copy of the actualContents in the model that is both formatted and styled"

	formattedText := textModel actualContents.
	self privateFormatAndConvert.
	self privateStyle.
	^formattedText! !

!SHTextStyler methodsFor: 'styling' stamp: 'pb 6/19/2015 20:55'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	| afterFormatProcess |
	self terminateBackgroundStylingProcess.

	formattedText := textModel actualContents copy.
	self mutex critical: [
		
		"This part, running at current priority, and fired by sem, is for events to be triggered at current priority"
		sem := Semaphore new. 
		afterFormatProcess := [
			sem ifNotNil: [
				sem wait.
				textModel stylerStyled: formattedText checkForChanges: true ]
		] newProcess.
		afterFormatProcess
			priority: Processor activePriority;
			name: 'Shout after style update';
			resume.

		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self privateStyle.
			sem signal] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

!methodRemoval: TextModel #privateStyleWith:!
TextModel removeSelector: #privateStyleWith:!
