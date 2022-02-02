'From Cuis 6.0 [latest update: #5058] on 2 February 2022 at 7:20:43 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 2/2/2022 19:18:58'!
newUIProcessIfNeeded
	"Answer is not nil only if a new process was created."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isTerminated ifTrue: [
			oldUIProcess 	animatedUI ifNotNil: [ :guiRootObject |
				self spawnNewMorphicProcessFor: guiRootObject.
				^UIProcess ]]].
	^nil! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 2/2/2022 17:15:10'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ UISupervisor newUIProcessIfNeeded.
	p ifNil: [
		p _ Sensor shiftPressed | Preferences cmdDotInterruptTakesStatistics
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	Preferences cmdDotEnabled ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		p == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(p name beginsWith: '[system]') ifTrue: [
			 ('Process {', p printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			p isTerminated
				ifTrue: [ ('Process {', p printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [ UISupervisor userInterrupt: p ]] fork
		]! !

