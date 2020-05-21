'From Cuis 5.0 [latest update: #4156] on 21 May 2020 at 5:06:14 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/21/2020 16:56:01'!
firstAvailable: numberOfObjects
	"Answer the first numberOfObjects in the receiver, subject to availability"

	^self first: (numberOfObjects min: self size)! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/21/2020 16:56:32'!
lastAvailable: numberOfObjects
	"Answer the last numberOfObjects in the receiver, subject to availability"

	^self last: (numberOfObjects min: self size)! !

!methodRemoval: SequenceableCollection #head: stamp: 'HAW 5/21/2020 16:50:32'!
SequenceableCollection removeSelector: #head:!
!methodRemoval: SequenceableCollection #tail: stamp: 'HAW 5/21/2020 16:51:34'!
SequenceableCollection removeSelector: #tail:!
