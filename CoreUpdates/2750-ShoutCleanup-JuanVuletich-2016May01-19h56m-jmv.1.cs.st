'From Cuis 4.2 of 25 July 2013 [latest update: #2748] on 1 May 2016 at 7:57:57.865462 pm'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/1/2016 19:35'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			WorldState addDeferredUIMessage: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

