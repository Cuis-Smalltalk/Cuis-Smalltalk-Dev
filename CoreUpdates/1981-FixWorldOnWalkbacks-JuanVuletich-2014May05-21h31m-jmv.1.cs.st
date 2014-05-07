'From Cuis 4.2 of 25 July 2013 [latest update: #1980] on 5 May 2014 at 9:33:32.143104 pm'!

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 5/5/2014 09:47'!
clearCanvas
	worldState clearCanvas! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 5/5/2014 09:50'!
spawnNewMorphicProcessFor: aWorld

	UIProcess ifNotNil: [ UIProcess animatedUI: nil ].
	UIProcess _ [
		aWorld clearWaitDelay.
		aWorld clearCanvas.
		[ aWorld doOneCycle. Processor yield. true] whileTrue: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI';
		animatedUI: aWorld;
		resume! !

