'From Cuis 5.0 of 7 November 2016 [latest update: #3766] on 25 May 2019 at 9:51:28 am'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/19/2017 18:58:55'!
combinations: k atATimeDo: aBlock
	"Take the items in the receiver, k at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size choose: k) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	aCollection _ Array new: k.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock! !

