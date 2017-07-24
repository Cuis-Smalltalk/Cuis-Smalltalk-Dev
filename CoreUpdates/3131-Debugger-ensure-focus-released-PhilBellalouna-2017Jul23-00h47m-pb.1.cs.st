'From Cuis 5.0 of 7 November 2016 [latest update: #3128] on 23 July 2017 at 12:48:38 am'!

!Debugger class methodsFor: 'opening' stamp: 'pb 7/23/2017 00:47:24'!
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
			"In case an error in Morphic code got us here, ensure mouse focus has been released"
			true runningWorld activeHand releaseMouseFocus.
			debugger _ self new process: process context: context.
			debugger interruptedProcessUI: w.
			bool
				ifTrue: [debugger openFullMorphicLabel: title]
				ifFalse: [PreDebugWindow open: debugger label: title message: nil]
		] on: UnhandledError do: [ :exOuter |
			| errorDescription |
			errorDescription _ 'Error while trying to open Debugger', String newLineString,
				'Orginal error: ', title asString, '.', String newLineString,
				'	Debugger error: ', 
				([exOuter description]
					on: UnhandledError
					do: [:exInner | exInner return: 'a ', exInner class printString]), ':'.
			self primitiveError: errorDescription
		]
	].
	process suspend! !

