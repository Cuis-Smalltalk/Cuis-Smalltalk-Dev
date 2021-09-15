'From Cuis 5.0 [latest update: #4179] on 24 May 2020 at 9:15:22 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'sqr 5/24/2020 21:13:36'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed' print]]
		ifFail: nil
		profiled: aBoolean! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'sqr 5/24/2020 21:13:48'!
doIt

	^self doItProfiling: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'sqr 5/24/2020 21:14:03'!
profileIt

	^self doItProfiling: true! !

