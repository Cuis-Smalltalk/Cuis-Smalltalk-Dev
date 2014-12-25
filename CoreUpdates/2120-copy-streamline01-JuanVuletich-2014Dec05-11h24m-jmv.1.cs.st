'From Cuis 4.2 of 25 July 2013 [latest update: #2119] on 5 December 2014 at 11:24:28.94596 am'!

!BlockClosure methodsFor: 'private' stamp: 'jmv 12/5/2014 11:04'!
copyForSaving
	"Answer a copy of the receiver suitable for serialization.
	 Notionally, if the receiver's outerContext has been returned from then nothing
	 needs to be done and we can use the receiver. But there's a race condition
	 determining if the receiver has been returned from (it could be executing in a
	 different process). So answer a copy anyway."
	^self copy! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:10'!
shuffledBy: aRandom
	| copy | 
	copy _ self copy.
	copy size to: 1 by: -1 do: [ :i |
		copy swap: i with: ((1 to: i) atRandom: aRandom)].
	^ copy! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 12/5/2014 11:09'!
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self copy permutationsStartingAt: 1 do: aBlock! !


!Text methodsFor: 'copying' stamp: 'jmv 12/5/2014 10:59'!
copyReplaceFrom: start to: stop with: aText

	^self copy replaceFrom: start to: stop with: aText! !


!Color methodsFor: 'objects from disk' stamp: 'jmv 12/5/2014 11:07'!
objectForDataStream: refStrm

	"I am about to be written on an object file. Clear cached stuff."
	^self copy flushCache; yourself! !

