'From Cuis 4.2 of 25 July 2013 [latest update: #1976] on 19 April 2014 at 5:49:11.178251 pm'!

!BlockNode methodsFor: 'printing' stamp: 'jmv 4/19/2014 17:22'!
printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines _ (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [ arguments size > 0 ].
	Preferences prettyPrintRectangularBlocks
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
							statements size = 1 and: [ statements first isComplex ]])
						ifTrue: [ aStream newLineTab: level ]
						ifFalse: [ aStream space ]]
				ifFalse: [
					(statements size = 1 and: [
							statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ]].
	((self
		printStatementsOn: aStream
		indent: level) > 0 and: [
			aStream peekLast ~= $]])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !

