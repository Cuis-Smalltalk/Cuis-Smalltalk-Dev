'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 11:06:35 am'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 11:05:45'!
openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil

	[
		bool
			ifTrue: [
				PreDebugWindow
					open: self
					label: label
					message: preDebugMessageOrNil ]
			ifFalse: [ self openFullMorphicLabel: label ]
	]
		on: UnhandledError
		do: [ :exOuter | | errorDescription |
			errorDescription _ 'Error while trying to open Debugger' , String newLineString , 
				'Orginal error: ' , label asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
			self primitiveError: errorDescription ]! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 11:00:11'!
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
	UISupervisor whenUIinSafeState: [
		self openWindow2Label: label usePreDebugWindow: true preDebugMessage: msg ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:04:05'!
open2On: process context: context label: label usePreDebugWindow: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	| debugger |
	"In case an error in Morphic code got us here, ensure mouse focus has been released"
	true runningWorld ifNotNil: [ :rWorld |
		rWorld activeHand ifNotNil: [ :aHand |
			aHand releaseMouseFocus ]].
	debugger _ self new.
	debugger 	process: process context: context.
	debugger openWindow2Label: label usePreDebugWindow: bool preDebugMessage: nil.! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:55:52'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	| debugger |
	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: aString.

	debugger _ Debugger new.
	debugger 		process: Processor activeProcess context: aContext.
	debugger 	openNotifierContents: contentsStringOrNil label: aString.
	Processor activeProcess suspend.! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 8/3/2022 11:05:33'!
open: model label: aString message: messageString
	|  window |

	window _ self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString print.
	aString ifNotNil: [ window setLabel: aString ].

	window openInWorld.! !

!methodRemoval: PreDebugWindow class #forceOpen:label:message: stamp: 'jmv 8/3/2022 11:05:54'!
PreDebugWindow class removeSelector: #forceOpen:label:message:!
