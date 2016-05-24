'From Cuis 4.2 of 25 July 2013 [latest update: #2773] on 24 May 2016 at 9:41:09.347088 am'!

!BlockClosure commentStamp: 'dhn 5/14/2016 22:07' prior: 0!
I am a block closure for Eliot's closure implementation. Not to be confused with the old BlockClosure (they were never part of Cuis anyway).

This is a closure converted image. With full closure support, you can finally use recursive blocks like here:

| fac |
	fac := [:n| n > 1 ifTrue:[n * (fac value: n-1)] ifFalse:[1]].
	fac value: 5. "120"

and close over temps correctly, such as here:

	(1 to: 10) do:[:i| WorldState addDeferredUIMessage:[Transcript newLine; show: i]].

Another good example:

| fib |
fib := [:n| n < 2 ifTrue:[1] ifFalse:[(fib value:n-1) + (fib value:n-2)]].
fib value: 10. "89"!


!SequenceableCollection methodsFor: 'enumerating' stamp: 'dhn 5/14/2016 22:29'!
asDigitsToPower: anInteger do: aBlock
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number.

	(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	aCollection _ Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'dhn 5/14/2016 22:33'!
combinations: kk atATimeDo: aBlock
	"Take the items in the receiver, kk at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size take: kk) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	aCollection _ Array new: kk.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'dhn 5/14/2016 22:35'!
pairsDo: aBlock 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript newLine; show: b, ' is number ', a printString]
"! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'dhn 5/14/2016 22:37'!
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self copy permutationsStartingAt: 1 do: aBlock! !


!Interval methodsFor: 'enumerating' stamp: 'dhn 5/14/2016 22:18'!
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self asArray permutationsDo: aBlock
! !


!Number methodsFor: 'printing' stamp: 'dhn 5/14/2016 22:21'!
isOrAreStringWith: aNoun
	| result |
	result _ self = 1
		ifTrue:
			[' is one ']
		ifFalse:
			[self = 0
				ifTrue:
					[' are no ']
				ifFalse:
					[' are ', self printString, ' ']].
	result _ result, aNoun.
	self = 1 ifFalse: [result _ result, 's'].
	^ result

"
#(0 1 2 98.6) do:
	[:num | Transcript newLine; show: 'There', (num isOrAreStringWith: 'way'), ' to skin a cat'].
"! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 08:51'!
labelArray: labelArray lines: lineArray
	"Answer an instance of me whose items are in labelArray, with lines 
	drawn after each item indexed by anArray. 2/1/96 sw"

	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].
	^ self
		labels: (String streamContents: 
			[:stream |
			labelArray do: [:each | stream nextPutAll: each; newLine].
			stream skip: -1 "remove last newline"])
		lines: lineArray

"Example:
	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #()) startUpWithCaption: 'Please pick one.'
"! !

