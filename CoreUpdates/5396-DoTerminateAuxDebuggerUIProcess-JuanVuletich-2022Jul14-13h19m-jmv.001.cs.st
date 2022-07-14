'From Cuis 6.0 [latest update: #5394] on 14 July 2022 at 1:27:16 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 7/14/2022 13:27:09'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		(UISupervisor isUIProcessRunning and: [ UISupervisor ui == interruptedProcessUI]) ifTrue: [
			UISupervisor uiProcess: interruptedProcess.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !

