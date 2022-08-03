'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:50:31 am'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:48:26'!
open2On: process context: context label: title usePreDebugWindow: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	[
		| debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger _ self new.
		debugger 	process: process context: context.
		bool
			ifTrue: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]
			ifFalse: [ debugger openFullMorphicLabel: title ]
	]
		on: UnhandledError
		do: [ :exOuter | | errorDescription |
			errorDescription _ 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asPlainString , '.' , String newLineString , '	Debugger error: ' ,
				([ exOuter description ]
					on: UnhandledError
					do: [ :exInner |
						exInner return: 'a ' , exInner class printString ]) , ':'.
			self primitiveError: errorDescription ]! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:48:52'!
openProcess: process context: context label: title
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
		self open2On: process context: context label: title usePreDebugWindow: false 	].
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


!Process methodsFor: 'debugging' stamp: 'jmv 8/2/2022 17:34:31'!
debugContext: context title: title
	"Open debugger on self with context shown on top"

	| topCtxt |
	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	topCtxt _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openProcess: self context: context label: title.! !

!methodRemoval: Debugger class #openOn:context:label:fullView: stamp: 'jmv 8/3/2022 10:46:37'!
Debugger class removeSelector: #openOn:context:label:fullView:!
!methodRemoval: Debugger class #open2On:context:label:fullView: stamp: 'jmv 8/3/2022 10:48:55'!
Debugger class removeSelector: #open2On:context:label:fullView:!
