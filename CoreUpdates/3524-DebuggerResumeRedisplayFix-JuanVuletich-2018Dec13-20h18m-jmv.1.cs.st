'From Cuis 5.0 of 7 November 2016 [latest update: #3523] on 13 December 2018 at 8:18:41 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 12/13/2018 20:18:10'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		Processor activeProcess animatedUI = interruptedProcessUI ifTrue: [
			interruptedProcess animatedUI: interruptedProcessUI.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, then it will terminate and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !

