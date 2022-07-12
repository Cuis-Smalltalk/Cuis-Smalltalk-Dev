'From Cuis 6.0 [latest update: #5389] on 12 July 2022 at 2:49:39 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:48:05'!
uiProcess: aProcess
	UIProcess _ aProcess! !


!Debugger methodsFor: 'private' stamp: 'jmv 7/12/2022 14:49:29'!
resumeProcess
	| mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	interruptedProcess isTerminated ifFalse: [
		(UISupervisor isUIProcessRunning and: [ UISupervisor ui == interruptedProcessUI]) ifTrue: [
			UISupervisor uiProcess: interruptedProcess ].
		interruptedProcess resume ].
	"if old process was resumed, just let the current one terminate"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.! !

