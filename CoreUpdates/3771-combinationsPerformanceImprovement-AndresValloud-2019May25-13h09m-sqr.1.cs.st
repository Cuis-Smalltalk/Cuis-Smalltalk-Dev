'From Cuis 5.0 of 7 November 2016 [latest update: #3768] on 25 May 2019 at 1:24:40 pm'!

!SequenceableCollection methodsFor: 'private' stamp: 'sqr 5/25/2019 13:19:43'!
combinationsAt: j upTo: k in: aCollection after: m upTo: n do: aBlock
	"Choose k of N items and put in aCollection.  j-1 already chosen.  Indexes of items are in numerical order to avoid duplication.  In this slot, we are allowed to use items in self indexed by m+1 up to n.  m is the index used for position j-1."
	"(1 to: 6) combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	m+1 to: n do: [:index | 
		aCollection at: j put: (self at: index).
		j = k
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self combinationsAt: j + 1 upTo: k in: aCollection after: index upTo: n do: aBlock]]! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'sqr 5/25/2019 13:20:59'!
combinations: k atATimeDo: aBlock
	"Take the items in the receiver, k at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size choose: k) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	aCollection _ Array new: k.
	self combinationsAt: 1 upTo: k in: aCollection after: 0 upTo: self size do: aBlock! !

!methodRemoval: SequenceableCollection #combinationsAt:in:after:do:!
SequenceableCollection removeSelector: #combinationsAt:in:after:do:!
!methodRemoval: SequenceableCollection #combinationsAt:upTo:in:after:do:!
SequenceableCollection removeSelector: #combinationsAt:upTo:in:after:do:!
