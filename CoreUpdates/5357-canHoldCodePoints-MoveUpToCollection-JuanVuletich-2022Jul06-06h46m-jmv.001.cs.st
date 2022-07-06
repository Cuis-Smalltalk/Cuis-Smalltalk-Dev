'From Cuis 6.0 [latest update: #5356] on 6 July 2022 at 6:46:34 am'!

!Collection methodsFor: 'testing' stamp: 'jmv 7/5/2022 23:07:48'!
canHoldCodePoints
	"Why not?"
	^true! !

!methodRemoval: SequenceableCollection #canHoldCodePoints stamp: 'jmv 7/6/2022 06:46:25'!
SequenceableCollection removeSelector: #canHoldCodePoints!
