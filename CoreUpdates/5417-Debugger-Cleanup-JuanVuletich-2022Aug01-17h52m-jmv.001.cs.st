'From Cuis 6.0 [latest update: #5415] on 1 August 2022 at 5:58:06 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 17:54:08'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| guiRootObjectOrNil |
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	guiRootObjectOrNil _ UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		self openOn: process context: context label: title fullView: bool guiRootObject: guiRootObjectOrNil
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

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 17:54:15'!
openOn: process context: context label: title fullView: bool guiRootObject: guiRootObjectOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger _ self new.
		debugger 	process: process context: context.
		debugger interruptedProcessUI: guiRootObjectOrNil.
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

