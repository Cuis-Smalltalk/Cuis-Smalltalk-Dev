'From Cuis 4.2 of 25 July 2013 [latest update: #2596] on 29 November 2015 at 9:32:22.3807 am'!
!classDefinition: #SHTextStyler category: #Shout!
Object subclass: #SHTextStyler
	instanceVariableNames: 'sem backgroundProcess formattedText textModel mutex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!

!SHTextStyler methodsFor: 'styling' stamp: 'jmv 11/29/2015 09:30'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	formattedText _ textModel actualContents copy.
	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			WorldState addDeferredUIMessage: [ textModel stylerStyled: formattedText checkForChanges: true ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !

!SHTextStyler methodsFor: 'private' stamp: 'jmv 11/29/2015 09:31'!
terminateBackgroundStylingProcess

	self mutex critical: [
		backgroundProcess
			ifNotNil: [
				backgroundProcess terminate.
				backgroundProcess _ nil ].
	]! !

!classDefinition: #SHTextStyler category: #Shout!
Object subclass: #SHTextStyler
	instanceVariableNames: 'backgroundProcess formattedText textModel mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!
