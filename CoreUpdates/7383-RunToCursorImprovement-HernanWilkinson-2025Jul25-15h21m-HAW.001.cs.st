'From Cuis7.5 [latest update: #7370] on 25 July 2025 at 3:21:37 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/25/2025 15:21:09'!
runToCursorAt: aPositionToStop

	| runToCursorContext |
	
	runToCursorContext := self selectedContext home.
	self assert: runToCursorContext mapsPcTo: aPositionToStop.
	
	[ (self pcRange includes: aPositionToStop) 
		ifTrue: [ self stepIntoBlock ]
		ifFalse: [ self doStep ].
	self isPcAt: aPositionToStop in: runToCursorContext ] whileFalse! !

