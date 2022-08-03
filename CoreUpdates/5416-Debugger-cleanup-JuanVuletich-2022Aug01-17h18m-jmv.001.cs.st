'From Cuis 6.0 [latest update: #5415] on 1 August 2022 at 5:52:23 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 17:40:56'!
interruptProcess: aProcess label: labelString
	"Create a Notifier on aProcess with the given label.
	This method is called only in special system processes, like the #userInterruptWatcher and the #lowSpaceWatcher."
	|  label |

	label _ labelString,
					' - Process: ', aProcess name,
					' - Priority: ', aProcess priority printString.
	aProcess isTerminated
		ifTrue: [
			UISupervisor newProcessIfUI: aProcess.
			self notify: 'Can not debug a terminated process: ', label ]
		ifFalse: [
			aProcess suspend.
			self
				openInterrupt: label 
				onProcess: aProcess ]! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 17:51:19'!
openOn: process context: context label: title fullView: bool guiRootObject: guiRootObject
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger _ self new.
		debugger 	process: process context: context.
		debugger interruptedProcessUI: guiRootObject.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription _ 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/1/2022 17:27:55'!
handleUserInterrupt
	| processToInterrupt |
	"If the UI process is not ready to run, the UI was not responsive, and most likely that's why the user pressed
	the interrupt key. Make GUI responsive again, and let them debug the process that made UI not responsive."
	processToInterrupt _ UISupervisor newUIProcessIfNeeded.
	processToInterrupt ifNil: [
		"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
		processToInterrupt _ Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	(Preferences at: #cmdDotEnabled) ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		processToInterrupt == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(processToInterrupt name beginsWith: '[system]') ifTrue: [
			 ('Process {', processToInterrupt printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			processToInterrupt isTerminated
				ifTrue: [ ('Process {', processToInterrupt printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [ Debugger interruptProcess: processToInterrupt label: 'User Interrupt' ]] fork
		].! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 8/1/2022 17:49:11'!
debug: aCompiledMethod receiver: anObject in: evalContext

	| processToDebug debugger |
	debugger _ Debugger new.
	processToDebug _ [
		aCompiledMethod
			valueWithReceiver: anObject
			arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ]).
		debugger sendProceeds.
	] newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: 'Debug it'.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/1/2022 17:25:10'!
newProcessIfUI: processBeingSuspended
	"processBeingSuspended has been suspended or is about to be.
	Answer the UI we created a new process for."

	processBeingSuspended == UIProcess ifTrue: [
		self spawnNewMorphicProcessFor: UI.
		^UI ].
	^nil
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !


!TestCase methodsFor: 'debugging - private' stamp: 'jmv 8/1/2022 17:49:07'!
openDebuggerOnFailingTestMethod

	| processToDebug compiledMethod debugger |

	compiledMethod _ self methodForTest.
	
	processToDebug _ [ [ self performTest ] ensure: [
		self tearDown.
		self tearDownResources]] newProcess name: 'debugIt'.
	
	debugger _ Debugger new
		process: processToDebug 
		context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod] whileFalse: [debugger send].
! !

!methodRemoval: UISupervisor class #userInterrupt: stamp: 'jmv 8/1/2022 17:28:00'!
UISupervisor class removeSelector: #userInterrupt:!
!methodRemoval: Debugger #openFullNoSuspendLabel: stamp: 'jmv 8/1/2022 17:49:15'!
Debugger removeSelector: #openFullNoSuspendLabel:!

!Debugger class reorganize!
('class initialization' releaseClassCachedState)
('instance creation' context: informExistingDebugger:label:)
('opening' interruptProcess:label: openContext:label:contents: openInterrupt:onProcess: openOn:context:label:fullView: openOn:context:label:fullView:guiRootObject:)
('cached state access' errorRecursion)
('constants' defaultDebugStackSize)
!

