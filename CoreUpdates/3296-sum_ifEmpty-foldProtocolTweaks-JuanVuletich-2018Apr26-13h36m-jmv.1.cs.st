'From Cuis 5.0 of 7 November 2016 [latest update: #3295] on 26 April 2018 at 1:38:13 pm'!

!Object methodsFor: 'evaluating' stamp: 'jmv 4/26/2018 13:11:27'!
valueWithPossibleArgument: anArg

	^self! !


!Collection methodsFor: 'enumerating' stamp: 'jmv 4/26/2018 13:33:07'!
collect: aBlock andFold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b] ifEmpty: nil
	#() collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	"

	| first eachValue answer |
	first _ true.
	self do: [ :each |
		eachValue _  aBlock value: each.
		first
			ifTrue: [
				first _ false.
				answer _ eachValue ]
			ifFalse: [
				answer _ aTwoArgBlock
					value: answer
					value: eachValue ]].
	first ifTrue: [ answer _ emptyBlockOrValue valueWithPossibleArgument: self ].
	^ answer! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 4/26/2018 13:17:08'!
fold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: 7
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: emptyBlockOrValue! !

!Collection methodsFor: 'statistics' stamp: 'jmv 4/26/2018 13:30:54'!
sum: aBlock ifEmpty: emptyBlockOrValue
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ] ifEmpty: emptyBlockOrValue! !


!Collection methodsFor: 'enumerating' stamp: 'jmv 4/26/2018 13:16:40'!
collect: aBlock andFold: aTwoArgBlock
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b]
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b]
	"

	^ self
		collect: aBlock
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 4/26/2018 13:16:51'!
fold: aTwoArgBlock
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b]
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 4/26/2018 13:22:28'!
reduce: aTwoArgBlock
	"Apply the argument, binaryBlock cumulatively to the elements of the receiver.
	For sequenceable collections the elements will be used in order, for unordered
	collections the order is unspecified."

	^self fold: aTwoArgBlock! !

!Collection methodsFor: 'statistics' stamp: 'jmv 4/26/2018 13:27:14'!
product
	"Compute the product of all the elements in the receiver"

	^self fold: [ :a :b | a * b] ifEmpty: 1! !

!Collection methodsFor: 'statistics' stamp: 'jmv 4/26/2018 13:30:08'!
sum
	"Compute the sum of all the elements in the receiver"

	^self fold: [ :a :b | a + b]! !

