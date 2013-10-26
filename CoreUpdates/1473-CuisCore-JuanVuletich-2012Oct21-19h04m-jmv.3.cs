'From Cuis 4.0 of 21 April 2012 [latest update: #1472] on 21 October 2012 at 8:00:39 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world '
	classVariableNames: 'DeferredUIMessages MaxCycleLapse '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 10/21/2012 19:58'!
worldState: aWorldState
	"
	World worldState: (World instVarNamed: 'worldState')
	"
	worldState _ aWorldState.
	worldState world: self! !


!Process methodsFor: 'UI support' stamp: 'jmv 10/21/2012 19:41'!
animatedUI
	"If we are an UI process, answer the root object of that UI.
	For a Morphic process, it is the Morphic World.
	Answer nil if not an UI process."

	^self triggerEvent: #animatedUI! !

!Process methodsFor: 'UI support' stamp: 'jmv 10/21/2012 19:37'!
animatedUI: anUIRoot
	"Let us know that we are running a certain UI.
	In Morphic, anUIRoot should be the World being run.
	We use the event system to avoid the need to add an ivar to us."

	self when: #animatedUI send: #yourself to: anUIRoot! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 10/21/2012 19:55'!
world: aPasteUpMorph

	world _ aPasteUpMorph! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/21/2012 19:46'!
spawnNewProcessX

	UIProcess _ [
		World clearWaitDelay.
		[ World doOneCycle. Processor yield. true] whileTrue: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI';
		animatedUI: World;
		resume! !

!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world'
	classVariableNames: 'DeferredUIMessages MaxCycleLapse'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend terminate)
('changing suspended state' activateReturn:value: complete: completeStep: completeTo: install: popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' calleeOf: copyStack isActiveProcess isSuspended isTerminated name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringWith: longPrintOn: printOn:)
('private' suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug debug:title: debug:title:full: debugWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('*KernelTests-Processes' suspendPrimitivelyOrFail)
('UI support' animatedUI animatedUI:)
!

