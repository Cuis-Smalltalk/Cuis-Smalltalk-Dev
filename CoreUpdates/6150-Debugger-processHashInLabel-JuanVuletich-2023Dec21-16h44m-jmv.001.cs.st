'From Cuis6.0 [latest update: #6147] on 21 December 2023 at 4:44:57 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 12/21/2023 16:04:45'!
openProcess: process context: context label: labelString
	"Open a Debugger on an existing process."

	| debugger logFile label |
	<primitive: 19> "Simulation guard"
	label := labelString,
					' - Process: ', process name,
					' - Priority: ', process priority printString,
					' (', process hash printString, ')'.
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		logFile := 'CuisDebug'.
		(labelString beginsWith: 'Space is low')
			ifTrue: [logFile := 'LowSpaceDebug'].
		Smalltalk logError: label inContext: context to: logFile ].

	debugger := self new.
	debugger process: process context: context.
	debugger openWindowLabel: label usePreDebugWindow: false preDebugMessage: nil.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !

