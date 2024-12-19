'From Cuis7.3 [latest update: #6935] on 19 December 2024 at 1:45:57 pm'!

!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 12/19/2024 13:42:07'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				| contextReceiver |
				contextReceiver := context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					| selector selfMethod otherObjectMethod |
					selector := context method selector.
					selfMethod := self class lookupSelector: selector.
					otherObjectMethod := otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						| errorMessage |
						errorMessage := contextReceiver class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#become: aborted.' print.
							Error signal: errorMessage, ' #become: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!Object methodsFor: 'system primitives' stamp: 'jmv 12/19/2024 13:41:42'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:
	"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					| selector oldMethod newMethod |
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						| errorMessage |
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward: aborted.' print.
							Error signal: errorMessage, ' #becomeForward: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 12/19/2024 13:41:53'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:copyHash:
	"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					| selector oldMethod newMethod |
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						| errorMessage |
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward:copyHash: aborted.' print.
							Error signal: errorMessage, ' #becomeForward:copyHash: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!Collection methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:42:25'!
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
	first ifTrue: [ answer := emptyBlockOrValue valueWithPossibleArgument: self ].
	^ answer! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:43:03'!
detectMax: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	| maxElement maxValue |
	self do: [ :each | 
		maxValue
			ifNil: ["first element"
				maxElement := each.
				maxValue := aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				| val |
				(val := aBlock value: each) > maxValue ifTrue: [
					maxElement := each.
					maxValue := val]]].
	^ maxElement! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:43:55'!
detectMin: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the lowest number.
	If collection empty, return nil."

	| minElement minValue |
	self do: [:each | 
		minValue
			ifNil: ["first element"
				minElement := each.
				minValue := aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				| val |
				(val := aBlock value: each) < minValue ifTrue: [
					minElement := each.
					minValue := val]]].
	^ minElement! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:44:32'!
groupsOf: n atATimeDo: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | 
			| args |
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].! !


!Array methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:44:46'!
collect: collectBlock thenSelect: selectBlock
	"Evaluate collectBlock with each my elements as the argument. Collect the 
	resulting values into a collection that is like me, but only those elements for which
	selectBlock evaluates to true. Answer the new collection.
	Overriden for performance."

	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			| newElement |
			newElement := collectBlock value: (self at: index).
			(selectBlock value: newElement)
				ifTrue: [ strm nextPut: newElement ]]]! !

!Array methodsFor: 'enumerating' stamp: 'jmv 12/19/2024 13:45:00'!
select: selectBlock thenCollect: collectBlock
	"Evaluate selectBlock with each my elements as the argument. For those who evaluate to true,
	collect the result of evaluating collectBlock on them into a collection that is like me.
	Answer the new collection.
	Overriden for performance."

	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			| each |
			each := self at: index.
			(selectBlock value: each) ifTrue: [
				strm nextPut: (collectBlock value: each) ]]]! !

