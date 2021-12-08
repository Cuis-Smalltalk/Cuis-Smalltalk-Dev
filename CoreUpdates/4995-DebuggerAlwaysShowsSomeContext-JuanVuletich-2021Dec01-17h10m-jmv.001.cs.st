'From Cuis 5.0 [latest update: #4994] on 1 December 2021 at 5:11:46 pm'!

!Debugger methodsFor: 'context stack (message list)' stamp: 'jmv 12/1/2021 17:09:14'!
toggleContextStackIndex: anInteger 
	"If anInteger is the same as the index of the selected context, deselect it, and select the top context. 
	Otherwise, the context whose index is anInteger becomes the selected context."

	self contextStackIndex: 
		(contextStackIndex = anInteger
			ifTrue: [1]
			ifFalse: [anInteger max: 1])
		oldContextWas:
		(contextStackIndex = 0
			ifTrue: [nil]
			ifFalse: [contextStack at: contextStackIndex])! !

