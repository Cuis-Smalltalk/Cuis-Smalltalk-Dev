'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 8:52:37 am'!

!SequenceableCollection methodsFor: 'copying' stamp: 'len 4/18/2016 22:08'!
shuffledBy: aGenerator
	"To answer a mutable collection when receiver is, for example, an Interval."
	^ (self collect: [ :each | each ]) shuffleBy: aGenerator! !

