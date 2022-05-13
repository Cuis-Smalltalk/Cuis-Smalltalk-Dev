'From Cuis 6.0 [latest update: #5160] on 13 May 2022 at 6:50:07 pm'!

!Bag methodsFor: 'enumerating' stamp: 'len 5/3/2022 06:51:53'!
allSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	contents keysDo: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true! !

!Bag methodsFor: 'enumerating' stamp: 'len 5/3/2022 06:52:25'!
anySatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false"

	contents keysDo: [:each| (aBlock value: each) ifTrue: [^ true]].
	^ false! !

!Bag methodsFor: 'statistics' stamp: 'len 5/3/2022 07:06:18'!
max: aBlock
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	contents keysDo: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer! !

!Bag methodsFor: 'statistics' stamp: 'len 5/3/2022 07:07:06'!
min: aBlock
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	contents keysDo: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer! !

!Bag methodsFor: 'statistics' stamp: 'len 5/3/2022 07:06:25'!
product: aBlock
	| answer |
	self emptyCheck.
	self withOccurrencesDo: [:each :times| answer _ answer isNil ifTrue: [each raisedTo: times] ifFalse: [answer * (each raisedTo: times)]].
	^ answer! !

!Bag methodsFor: 'statistics' stamp: 'len 5/3/2022 07:22:21'!
sum: aBlock ifEmpty: emptyBlockOrValue
	| answer |
	self isEmpty ifTrue: [^ emptyBlockOrValue value].
	self withOccurrencesDo: [:each :times| answer _ answer isNil ifTrue: [each * times] ifFalse: [each * times + answer]].
	^ answer! !


!Collection methodsFor: 'enumerating' stamp: 'len 5/3/2022 07:34:31'!
detect: aBlock ifFound: foundBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the evaluation of foundBlock at the first element for which aBlock evaluates to true."
	^ foundBlock value: (self detect: aBlock ifNone: [^ exceptionBlock value])! !

!Collection methodsFor: 'statistics' stamp: 'len 5/3/2022 07:12:30'!
max
	^ self max: [:each| each]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/3/2022 07:12:17'!
min
	^ self min: [:each| each]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/3/2022 07:11:30'!
product
	"Answer the product of all elements in the receiver."
	^ self product: [:each| each]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/3/2022 07:11:43'!
sum
	"Answer the sum of all elements in the receiver."
	^ self sum: [:each| each]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/3/2022 07:15:20'!
sum: aBlock
	^ self sum: aBlock ifEmpty: [self errorEmptyCollection]! !

!methodRemoval: Bag #sum stamp: 'len 5/13/2022 18:49:18'!
Bag removeSelector: #sum!

!Bag reorganize!
('accessing' at: at:put: cumulativeCounts size sortedCounts sortedElements)
('adding' add: add:withOccurrences: addAll:)
('copying' postCopy)
('enumerating' allSatisfy: anySatisfy: asSet collect: count: detect:ifNone: do: select: withOccurrencesDo:)
('private' contents setContents:)
('removing' remove:ifAbsent:)
('testing' includes: is: occurrencesOf:)
('comparing' =)
('statistics' max: min: product: sum:ifEmpty:)
!

