'From Cuis 6.0 [latest update: #5463] on 22 August 2022 at 12:34:06 pm'!

!Debugger class methodsFor: 'opening' stamp: 'HAW 8/22/2022 12:31:12'!
openContext: aContext label: label preDebugMessage: contentsStringOrNil
	"Open a notifier (a PreDebugWindow) in response to an error, halt, or notify.
	A PreDebugWindow just shows a short view of the sender stack and provides a menu that lets the user open a full debugger.
	The context argument is in the active process stack."

	| debugger |
	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk logError: label inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: label.

	debugger _ self new.
	debugger 		process: Processor activeProcess context: aContext.
	debugger openWindowLabel: label usePreDebugWindow: true preDebugMessage: contentsStringOrNil.! !

!Debugger class methodsFor: 'opening' stamp: 'HAW 8/22/2022 11:57:45'!
openDebugging: aBlock to: aCompiledMethod label: aString
	"Create a new process to debug aBlock, and step into it."

	| processToDebug debugger |
	debugger _ self new.
	processToDebug _ aBlock newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: aString.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !

!Debugger class methodsFor: 'opening' stamp: 'HAW 8/22/2022 12:31:03'!
openProcess: process context: context label: labelString
	"Open a Debugger on an existing process."

	| debugger logFile label |
	<primitive: 19> "Simulation guard"
	label _ labelString,
					' - Process: ', process name,
					' - Priority: ', process priority printString.
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		logFile _ 'CuisDebug'.
		(labelString beginsWith: 'Space is low')
			ifTrue: [logFile _ 'LowSpaceDebug'].
		Smalltalk logError: label inContext: context to: logFile ].

	debugger _ self new.
	debugger process: process context: context.
	debugger openWindowLabel: label usePreDebugWindow: false preDebugMessage: nil.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !

