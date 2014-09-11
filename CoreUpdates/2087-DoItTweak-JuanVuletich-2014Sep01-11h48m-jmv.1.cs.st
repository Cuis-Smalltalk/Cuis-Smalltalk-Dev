'From Cuis 4.2 of 25 July 2013 [latest update: #2085] on 1 September 2014 at 3:01:26.210345 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 9/1/2014 11:52'!
doIt
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self evaluateSelectionAndDo: [ :result |
		(result printString) print
		] ifFail: nil! !

