'From Cuis 4.0 of 21 April 2012 [latest update: #1275] on 7 May 2012 at 2:00:31 pm'!
!classDefinition: #SHTextStyler category: #'Shout-Styling'!
Object subclass: #SHTextStyler
	instanceVariableNames: 'sem backgroundProcess monitor formattedText textModel mutex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout-Styling'!

!SHTextStyler methodsFor: 'private' stamp: 'jmv 5/7/2012 13:57'!
mutex
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/7/2012 13:57'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	formattedText _ textModel actualContents copy.
	self mutex critical: [
		
		"This part, running at current priority, and fired by sem, is for events to be triggered at current priority"
		sem _ Semaphore new. 
		[
			sem ifNotNil: [
				sem wait.
				textModel stylerStyled: formattedText checkForChanges: true ]
		] forkAt: Processor activePriority.

		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			textModel privateStyleWith: self.
			sem signal] newProcess.
		backgroundProcess priority: Processor userBackgroundPriority.
		backgroundProcess resume
	]! !

!SHTextStyler methodsFor: 'private' stamp: 'jmv 5/7/2012 13:58'!
terminateBackgroundStylingProcess
	self mutex critical: [
		backgroundProcess 
			ifNotNil: [
				backgroundProcess terminate.
				backgroundProcess := nil].
		sem 
			ifNotNil:[
				sem terminateProcess.
				sem := nil].	
	]		! !

!methodRemoval: SHTextStyler #monitor!
SHTextStyler removeSelector: #monitor!
!classDefinition: #SHTextStyler category: #'Shout-Styling'!
Object subclass: #SHTextStyler
	instanceVariableNames: 'sem backgroundProcess formattedText textModel mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout-Styling'!
