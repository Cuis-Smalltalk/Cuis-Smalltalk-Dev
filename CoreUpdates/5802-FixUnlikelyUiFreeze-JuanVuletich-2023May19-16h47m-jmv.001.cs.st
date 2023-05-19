'From Cuis 6.0 [latest update: #5797] on 19 May 2023 at 4:53:45 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/19/2023 16:47:20'!
convertAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self terminateBackgroundStylingProcess.
	formattedText := text.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self styleInBackgroundProcess ]]
		ifFalse: [
			self privateConvert.
			self privateStyle.
			textModel changed: #shoutStyled ]! !

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/19/2023 16:47:28'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self privateConvert.
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

