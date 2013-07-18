'From Cuis 4.1 of 12 December 2012 [latest update: #1694] on 9 May 2013 at 11:40:59.293093 pm'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor errorWasInUIProcess labelString '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 5/9/2013 23:38'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk logError: title inContext: context to: 'CuisDebug.log'].
	ProjectX newProcessIfUIX: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [ 
		[	| debugger |
			debugger _ self new process: process controller: nil context: context.
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


!Process methodsFor: 'debugging' stamp: 'jmv 5/9/2013 23:27'!
debugFullWithTitle: title
	"Open debugger on self"

	| context |
	context _ self isActiveProcess ifTrue: [thisContext] ifFalse: [self suspendedContext].
	self debug: context title: title full: true! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 5/9/2013 23:35'!
catchThePig: aProcess
	| rule |
	"nickname, allow-stop, allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) first.

	aProcess animatedUI notNil
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [
			rule ifFalse: [ ^self ].
			ProcessBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]! !


!Debugger methodsFor: 'initialize' stamp: 'jmv 5/9/2013 23:37'!
openFullNoSuspendLabel: aString
	"Create and schedule a full debugger with the given label. Do not terminate the current active process."

	self openFullMorphicLabel: aString.
	ProjectX newProcessIfUIX: interruptedProcess! !

!Debugger methodsFor: 'initialize' stamp: 'jmv 5/9/2013 23:37'!
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

	ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !

!Debugger methodsFor: 'private' stamp: 'jmv 5/9/2013 23:37'!
process: aProcess controller: aController context: aContext
	super initialize.
	Smalltalk at: #MessageTally ifPresentAndInMemory: [ :c | c terminateTimerProcess ].
	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.
	externalInterrupt _ false.
	selectingPC _ true! !

!Debugger methodsFor: 'private' stamp: 'jmv 5/9/2013 23:40'!
resumeProcess
	| mustTerminateActive |
	savedCursor
		ifNotNil: [Sensor currentCursor: savedCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		Processor activeProcess animatedUI ifNotNil: [ :ui |
			interruptedProcess animatedUI: ui.
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


!Process methodsFor: 'debugging' stamp: 'jmv 5/9/2013 23:27'!
debug: context title: title
	"Open debugger on self with context shown on top"

	self debug: context title: title full: false! !

!Process methodsFor: 'debugging' stamp: 'jmv 5/9/2013 23:25'!
debug: context title: title full: bool
	"Open debugger on self with context shown on top"

	| topCtxt |
	topCtxt _ self isActiveProcess ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: bool.
! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 5/9/2013 23:21'!
debugProcess: aProcess
"	self resumeProcess: aProcess."
	aProcess debugFullWithTitle: 'Interrupted from the Process Browser'.
! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 5/9/2013 23:38'!
newProcessIfUIX: suspendedProcess

	suspendedProcess animatedUI ifNotNil: [ :aWorld |
		self spawnNewMorphicProcessFor: aWorld ]! !

!methodRemoval: ProjectX class #spawnNewProcessIfThisIsUIX:!
ProjectX class removeSelector: #spawnNewProcessIfThisIsUIX:!
!methodRemoval: ProcessBrowser class #isUIProcess:!
ProcessBrowser class removeSelector: #isUIProcess:!
!methodRemoval: Process #debug!
Process removeSelector: #debug!
!methodRemoval: Process #debugWithTitle:!
Process removeSelector: #debugWithTitle:!
!methodRemoval: Debugger class #openOn:context:label:contents:fullView:!
Debugger class removeSelector: #openOn:context:label:contents:fullView:!
!methodRemoval: Debugger #errorWasInUIProcess:!
Debugger removeSelector: #errorWasInUIProcess:!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString'
	classVariableNames: 'ErrorRecursion'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
