'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 10 September 2017 at 4:47:52 pm'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/10/2017 16:44:03'!
withNextDo: twoArgBlock
	"Evaluate the block with each element and the one following it.
	For the last element, next is nil
	(1 to: 10) asArray withNextDo: [ :each :next | {each. next} print ]
	#() withNextDo: [ :a :b | {a. b} print ]
	"
	| first previous |
	first _ true.
	self do: [ :each |
		first ifTrue: [
			first _ false ]
		ifFalse: [
			twoArgBlock value: previous value: each ].
		previous _ each ].
	first ifFalse: [
		twoArgBlock value: previous value: nil ]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/10/2017 16:44:25'!
withPreviousDo: twoArgBlock
	"Evaluate the block with each element and the one before it.
	For the first element, previous is nil
	(1 to: 10) asArray withPreviousDo: [ :each :previous | {previous. each} print ]
	#() withPreviousDo: [ :a :b | {a. b} print ]
	"
	| previous |
	previous _ nil.
	self do: [ :each |
		twoArgBlock value: each value: previous.
		previous _ each ].! !

