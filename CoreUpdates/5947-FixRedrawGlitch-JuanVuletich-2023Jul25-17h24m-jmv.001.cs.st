'From Cuis 6.0 [latest update: #5946] on 25 July 2023 at 5:27:07 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 7/25/2023 17:20:35'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		interruptedProcessUI notNil ifTrue: [
			(interruptedProcess name withoutPrefix: 'Formerly: ')
				ifNotNil: [ :originalName | interruptedProcess name: originalName ].
			interruptedProcess priority: Processor userSchedulingPriority.
			UISupervisor isUIProcessRunning
				ifTrue: [ mustTerminateActive := true]
				ifFalse: [ UISupervisor stopUIProcess]].
		interruptedProcess resume ].
	interruptedProcess := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	mustRedisplay ifNotNil: [ :w |
		[UISupervisor whenUIinSafeState: [ w invalidateBounds ]]
			forkAt: Processor userSchedulingPriority-1 ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 7/25/2023 17:26:41'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	
	self isTopWindow ifTrue: [
		self comeToFront.
		^self ].

	oldTop := TopWindow.
	TopWindow := self.
	self needsRedraw: true.

	oldTop ifNotNil: [
		oldTop invalidateTitleArea.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow := oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self comeToFront.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !

