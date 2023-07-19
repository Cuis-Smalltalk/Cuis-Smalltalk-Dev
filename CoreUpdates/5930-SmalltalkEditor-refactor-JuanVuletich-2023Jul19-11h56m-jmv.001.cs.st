'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 12:06:07 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:02:20'!
doIt

	self doItProfiling: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:02:52'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed (remove handler in #doItProfiling: if you prefer the debugger).' print]]
		ifFail: nil
		profiled: aBoolean.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:04:01'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and evaluate aBlock with the result
	3 +4
	"	
	
	self lineSelectAndEmptyCheck: [
		^ self ].
	self evaluate: self selection string andDo: aBlock ifFail: failBlock profiled: doProfile! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:02:24'!
profileIt

	self doItProfiling: true! !

