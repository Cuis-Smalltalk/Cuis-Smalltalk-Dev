'From Cuis 5.0 [latest update: #4451] on 15 November 2020 at 10:30:55 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/15/2020 08:26:13'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	| answer |
	answer _ self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed' print]]
		ifFail: nil
		profiled: aBoolean.
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^answer! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/15/2020 08:25:28'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result receiver context methodAndCompiler |

	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver _ provider doItReceiver.
			context _ provider doItContext]
		ifFalse: [receiver _ context _ nil].

	methodAndCompiler _ self compileSelectionFor: receiver in: context ifFail: [^ failBlock value].

	result _ (methodAndCompiler at: #compiler)
		evaluateMethod: (methodAndCompiler at: #method)
		to: receiver
		logged: true
		profiled: doProfile.
	
	^ aBlock value: result! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/15/2020 08:27:53'!
exploreIt

	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ morph flash ]
		profiled: false.
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/15/2020 08:26:35'!
inspectSelectionOrLine

	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false.
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

