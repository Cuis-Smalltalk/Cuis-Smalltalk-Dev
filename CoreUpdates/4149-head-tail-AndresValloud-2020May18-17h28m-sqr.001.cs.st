'From Cuis 5.0 [latest update: #4148] on 18 May 2020 at 5:29:26 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'sqr 5/17/2020 00:15:12'!
head: anInteger
	"Answer the first anInteger objects in the receiver, subject to availability"

	^self first: (anInteger min: self size)! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sqr 5/17/2020 00:15:23'!
tail: anInteger
	"Answer the last anInteger objects in the receiver, subject to availability"

	^self last: (anInteger min: self size)! !

