'From Cuis 5.0 [latest update: #4503] on 29 December 2020 at 12:16:28 pm'!

!Debugger methodsFor: 'private' stamp: 'HAW 12/29/2020 12:15:11'!
selectedContext
	contextStackIndex = 0
		ifTrue: [^contextStackTop]
		ifFalse: [^contextStack ifNotNil: [ :aContextStack | aContextStack at: contextStackIndex]]! !

