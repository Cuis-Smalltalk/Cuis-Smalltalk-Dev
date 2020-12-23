'From Cuis 5.0 [latest update: #4494] on 23 December 2020 at 10:11:41 am'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 12/22/2020 11:41:26'!
withPreviousCyclicDo: twoArgBlock
	"Evaluate the block with each element and the one before it.
	For the first element, previous is the last one. i.e. each object is at some point the first of the pair, and at some other point the second of the pair
	(1 to: 10) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1 2 3) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1 2) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#(1) asArray withPreviousCyclicDo: [ :each :previous | {previous. each} print ]
	#() withPreviousCyclicDo: [ :a :b | {a. b} print ]
	"
	| previous |
	self size < 2 ifTrue: [^self ].
	previous _ self last.
	self do: [ :each |
		twoArgBlock value: each value: previous.
		previous _ each ].! !

