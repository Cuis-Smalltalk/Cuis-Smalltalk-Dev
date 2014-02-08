'From Cuis 4.2 of 25 July 2013 [latest update: #1947] on 2 February 2014 at 12:45:00.499312 am'!

!Debugger methodsFor: 'private' stamp: 'jmv 2/2/2014 00:34'!
process: aProcess context: aContext

	Smalltalk at: #MessageTally ifPresentAndInMemory: [ :c | c terminateTimerProcess ].
	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.
	externalInterrupt _ false.
	selectingPC _ true! !


!ChangeSet class methodsFor: 'instance creation' stamp: 'jmv 2/2/2014 00:35'!
named: aName

	^ (self basicNew name: aName) initialize! !


!Debugger class methodsFor: 'instance creation' stamp: 'jmv 2/2/2014 00:34'!
context: aContext 
	"Answer an instance of me for debugging the active process starting with the given context."
	^ self new
		process: Processor activeProcess
		context: aContext! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 2/2/2014 00:43'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger
		process: interruptedProcess
		context: interruptedProcess suspendedContext.
	debugger externalInterrupt: true.

Preferences logDebuggerStackToFile ifTrue:
	[(aString includesSubString: 'Space') & 
		(aString includesSubString: 'low') ifTrue: [
			Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug.log']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 2/2/2014 00:43'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk logError: title inContext: context to: 'CuisDebug.log'].
	ProjectX newProcessIfUIX: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [ 
		[	| debugger |
			debugger _ self new process: process context: context.
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


!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 2/2/2014 00:36'!
existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean

	| newSet |
	^(ChangeSet changeSetNamed: aName)
		ifNotNil: [ :existing |
			existing assert: existing isForBaseSystem = aBoolean ]
		ifNil: [
			newSet _ ChangeSet named: aName.
			newSet isForBaseSystem: aBoolean.
			AllChangeSets add: newSet.
			newSet ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 2/2/2014 00:43'!
debug: aCompiledMethod receiver: anObject in: evalContext

	| guineaPig debugger context |
	guineaPig _ [
		aCompiledMethod
			valueWithReceiver: anObject
			arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ]) ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ Debugger new
		process: guineaPig
		context: context.
	debugger openFullNoSuspendLabel: 'Debug it'.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!TestCase methodsFor: 'Running' stamp: 'jmv 2/2/2014 00:44'!
openDebuggerOnFailingTestMethod
	| guineaPig context compiledMethod debugger |
	compiledMethod _ self class lookupSelector: self selector.
	guineaPig _ [ self performTest ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ Debugger new
		process: guineaPig
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.
	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
! !

!TestCase methodsFor: 'Running' stamp: 'jmv 2/2/2014 00:44'!
openDebuggerOnFailingTestMethod: semaphore
	| guineaPig context compiledMethod debugger |
	compiledMethod _ self class lookupSelector: self selector.
	guineaPig _ [ self performTest ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ TestCaseDebugger new
		process: guineaPig
		context: context.
	debugger doneSemaphore: semaphore.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.
	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
! !

!methodRemoval: ChangeSet class #basicNewNamed:!
ChangeSet class removeSelector: #basicNewNamed:!
!methodRemoval: Debugger #initialize!
Debugger removeSelector: #initialize!
!methodRemoval: Debugger #process:controller:context:!
Debugger removeSelector: #process:controller:context:!
