'From Cuis 5.0 [latest update: #4156] on 21 May 2020 at 5:35:24 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/21/2020 17:34:06'!
head: anInteger
	"Answer the first anInteger objects in the receiver, subject to availability"
	
	^self firstAvailable: anInteger ! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/21/2020 17:34:34'!
tail: anInteger
	"Answer the last anInteger objects in the receiver, subject to availability"
	
	^self lastAvailable: anInteger ! !

