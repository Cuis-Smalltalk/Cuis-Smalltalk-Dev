'From Cuis7.1 [latest update: #6367] on 15 May 2024 at 5:20:24 pm'!

!MessageSend methodsFor: 'evaluating' stamp: 'jmv 5/15/2024 15:51:16'!
valueWithPossibleArgument: anObject

	^selector last = $:
		ifFalse: [ receiver perform: selector ]
		ifTrue: [ receiver perform: selector with: anObject ]! !

