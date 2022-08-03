'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:44:56 am'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:41:28'!
open2On: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger _ self new.
		debugger 	process: process context: context.
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


!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 10:39:22'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self class lowSpaceMessage, (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			forceOpen: self
			label: label
			message: msg ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.! !

!Debugger methodsFor: 'private' stamp: 'jmv 8/2/2022 18:00:56'!
process: aProcess context: aContext

	interruptedProcess _ aProcess.
	interruptedProcessUI _ UISupervisor animatedUIOf: interruptedProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:41:55'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		self open2On: process context: context label: title fullView: bool
		].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.
	process suspend.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/3/2022 10:42:25'!
newProcessIfUI: processBeingSuspended
	"processBeingSuspended has been suspended or is about to be.
	Answer the UI we created a new process for."

	processBeingSuspended == UIProcess ifTrue: [
		self spawnNewMorphicProcessFor: UI ]
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !

!methodRemoval: Debugger class #openOn:context:label:fullView:guiRootObject: stamp: 'jmv 8/3/2022 10:41:58'!
Debugger class removeSelector: #openOn:context:label:fullView:guiRootObject:!
!methodRemoval: Debugger #interruptedProcessUI: stamp: 'jmv 8/3/2022 10:42:58'!
Debugger removeSelector: #interruptedProcessUI:!
