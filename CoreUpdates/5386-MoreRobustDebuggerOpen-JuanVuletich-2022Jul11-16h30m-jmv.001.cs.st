'From Cuis 6.0 [latest update: #5385] on 11 July 2022 at 4:31:01 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 7/11/2022 16:11:18'!
openOn: process context: context label: title fullView: bool guiRootObject: guiRootObject
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger _ self new
			process: process
			context: context.
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


!Debugger methodsFor: 'initialization' stamp: 'jmv 7/11/2022 16:27:51'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices , (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			forceOpen: self
			label: label
			message: msg ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfNeeded.
	] fork.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 7/11/2022 16:21:56'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| guiRootObject |
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	guiRootObject _ UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		self openOn: process context: context label: title fullView: bool guiRootObject: guiRootObject
		].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfNeeded.
	] fork.
	process suspend.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/11/2022 16:25:35'!
newProcessIfUI: processBeingSuspended
	"processBeingSuspended has been suspended or is about to be.
	Answer the UI we created a new process for.
	If not an UI process, still guarantee that the UI process is runnable."

	processBeingSuspended animatedUI ifNotNil: [ :guiRootObject |
		self spawnNewMorphicProcessFor: guiRootObject.
		^guiRootObject ].
	^nil
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !

