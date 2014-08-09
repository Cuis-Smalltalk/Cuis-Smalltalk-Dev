'From Cuis 4.2 of 25 July 2013 [latest update: #2074] on 8 August 2014 at 9:28:07.714375 am'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString interruptedProcessUI '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!Debugger methodsFor: 'accessing' stamp: 'jmv 8/7/2014 20:32'!
interruptedProcessUI: aWorld
	interruptedProcessUI _ aWorld! !


!Debugger methodsFor: 'private' stamp: 'jmv 8/7/2014 20:36'!
resumeProcess
	| mustTerminateActive |
	savedCursor
		ifNotNil: [Sensor currentCursor: savedCursor].
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
	self runningWorld ifNotNil: [ :w | w displayWorld ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, then it will terminate and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/7/2014 20:30'!
openFullNoSuspendLabel: aString
	"Create and schedule a full debugger with the given label. Do not terminate the current active process."

	self openFullMorphicLabel: aString.
	interruptedProcessUI _ ProjectX newProcessIfUIX: interruptedProcess! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/7/2014 20:30'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Sensor currentCursor.
	Sensor currentCursor: Cursor normal.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	interruptedProcessUI _ ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/7/2014 20:33'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	| w |
	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk logError: title inContext: context to: 'CuisDebug.log'].
	w _ ProjectX newProcessIfUIX: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [ 
		[	| debugger |
			debugger _ self new process: process context: context.
			debugger interruptedProcessUI: w.
			bool
				ifTrue: [debugger openFullMorphicLabel: title]
				ifFalse: [PreDebugWindow open: debugger label: title message: nil]
		] on: Error do: [ :ex |
			self primitiveError:
				'Error while trying to open Debugger', String newLineString,
				'Orginal error: ', 
				title asString, '.', String newLineString,
				'	Debugger error: ', 
				([ex description] on: Error do: ['a ', ex class printString]), ':'
		]
	].
	process suspend! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 8/7/2014 20:29'!
newProcessIfUIX: suspendedProcess
	"Answer the UI we created a new process for"

	suspendedProcess animatedUI ifNotNil: [ :aWorld |
		self spawnNewMorphicProcessFor: aWorld.
		^aWorld ].
	^nil! !

!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString interruptedProcessUI'
	classVariableNames: 'ErrorRecursion'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
