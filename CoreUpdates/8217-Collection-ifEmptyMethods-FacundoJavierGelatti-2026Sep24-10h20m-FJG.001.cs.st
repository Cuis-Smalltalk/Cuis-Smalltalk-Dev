'From Cuis7.9 [latest update: #8216] on 24 September 2026 at 10:44:26 am'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'FJG 21/Sep/2026, 6:05:12 pm (UTC)'!
firstIfEmpty: emptyBlockOrValue
	"Answer the first element of the receiver.
	If the receiver is empty, answer the result of evaluating emptyBlockOrValue."

	^ self ifEmpty: emptyBlockOrValue ifNotEmpty: [ self at: 1 ]! !


!Collection methodsFor: 'enumerating' stamp: 'FJG 21/Sep/2026, 6:05:12 pm (UTC)'!
collect: aBlock andFold: aTwoArgBlock
	"Evaluate aTwoArgBlock with the values of aBlock on the first two elements of the receiver,
	 then with the result of the first evaluation and the value of aBlock on the next element,
	 and so on.  Answer the result of the final evaluation.
	 If the receiver has a single element, answer the value of aBlock on that element.
	 If the receiver is empty, raise an error."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b]
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b]
	"

	^ self
		collect: aBlock
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 24/Sep/2026, 1:31:24 pm (UTC)'!
collect: aBlock andFold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate aTwoArgBlock with the values of aBlock on the first two elements of the receiver,
	 then with the result of the first evaluation and the value of aBlock on the next element,
	 and so on.  Answer the result of the final evaluation.
	 If the receiver has a single element, answer the value of aBlock on that element.
	 If the receiver is empty, answer the result of evaluating emptyBlockOrValue."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty: nil
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b] ifEmpty: nil
	#() collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty: nil
	"

	| first answer |
	first := true.
	self do: [ :each |
		| eachValue |
		eachValue :=  aBlock value: each.
		first
			ifTrue: [
				first := false.
				answer := eachValue ]
			ifFalse: [
				answer := aTwoArgBlock
					value: answer
					value: eachValue ]].
	first ifTrue: [ answer := emptyBlockOrValue value ].
	^ answer! !

!Collection methodsFor: 'enumerating' stamp: 'FJG 21/Sep/2026, 6:05:12 pm (UTC)'!
fold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation.
	 If the receiver has a single element, answer that element.
	 If the receiver is empty, answer the result of evaluating emptyBlockOrValue."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: 7
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: emptyBlockOrValue! !

!Collection methodsFor: 'statistics' stamp: 'jmv 24/Sep/2026, 1:42:38 pm (UTC)'!
average: aBlock ifEmpty: emptyBlockOrValue 
	"Answer the average of evaluating aBlock on all the elements of the receiver.
	If the receiver is empty, answer the result of evaluating emptyBlockOrValue."
	
	^ (self sum: aBlock ifEmpty: [ ^emptyBlockOrValue value ]) / self size! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'FJG 21/Sep/2026, 6:05:12 pm (UTC)'!
lastIfEmpty: emptyBlockOrValue
	"Answer the last element of the receiver.
	If the receiver is empty, answer the result of evaluating emptyBlockOrValue."

	^ self ifEmpty: emptyBlockOrValue ifNotEmpty: [ self at: self size ]! !

