'From Cuis 5.0 of 7 November 2016 [latest update: #3593] on 1 February 2019 at 10:32:03 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 2/1/2019 10:27:32'!
doIt
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			result print ]
		ifFail: nil
		profiled: false! !


!Transcript class methodsFor: 'private' stamp: 'jmv 2/1/2019 10:27:12'!
finishEntry
	| newEntry |
	self unfinishedEntrySize > 1 ifTrue: [
		newEntry _ unfinishedEntry contents.
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		self addEntry: newEntry.
		self display ].! !

