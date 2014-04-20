'From Cuis 4.2 of 25 July 2013 [latest update: #1971] on 25 March 2014 at 12:24:46.174622 am'!

!BlockNode methodsFor: 'printing' stamp: 'jmv 3/25/2014 00:22'!
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
				ifTrue: [ aStream newLineTab: level ]
				ifFalse: [ aStream space ]].
	((self
		printStatementsOn: aStream
		indent: level) > 0 and: [
			aStream peekLast ~= $]])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !


!CascadeNode methodsFor: 'printing' stamp: 'jmv 3/24/2014 23:44'!
printOn: aStream indent: level precedence: p
	p > 0 ifTrue: [ aStream nextPut: $( ].
	messages first
		printReceiver: receiver
		on: aStream
		indent: level.
	1
		to: messages size
		do: [ :i | 
			aStream newLineTab: level + 1.
			(messages at: i)
				printOn: aStream
				indent: level+1.
			i < messages size ifTrue: [ aStream nextPut:$; ] ].
	p > 0 ifTrue: [ aStream nextPut: $) ]! !


!MessageNode methodsFor: 'printing' stamp: 'jmv 3/24/2014 23:30'!
printKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue: [
		receiver ifNotNil: [ aStream space ].
		aStream nextPutAll: key.
		^ self ].
	keywords _ key keywords.
	doCrTab _ args size > 1.
	1
		to: (args size min: keywords size)
		do: [ :i |
			arg _ args at: i.
			kwd _ keywords at: i.
			doCrTab
				ifTrue: [
					aStream newLineTab: level + 1.
					indent _ 1
					"newline after big args" ]
				ifFalse: [
					receiver ifNotNil: [ aStream space ].
					indent _ 0 ].
			aStream nextPutAll: kwd.
			arg printsInNewLine
				ifTrue: [
					aStream newLineTab: level + indent + 1 ]
				ifFalse: [
					aStream space ].
			arg
				printOn: aStream
				indent: level + 1 + indent
				precedence:
					(precedence = 2
						ifTrue: [ 1 ]
						ifFalse: [ precedence ]) ].! !

