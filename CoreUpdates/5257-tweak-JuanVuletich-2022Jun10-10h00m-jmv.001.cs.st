'From Cuis 6.0 [latest update: #5256] on 10 June 2022 at 10:04:03 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/10/2022 10:04:00'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	| answer |
	answer _ self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed (remove handler in #doItProfiling: if you prefer the debugger).' print]]
		ifFail: nil
		profiled: aBoolean.
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^answer! !

