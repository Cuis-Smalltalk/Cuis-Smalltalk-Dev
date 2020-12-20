'From Cuis 5.0 [latest update: #4490] on 18 December 2020 at 9:22:56 pm'!

!StringRequestMorph methodsFor: 'private' stamp: 'MJR 12/18/2020 21:19:20'!
textBox
	
	^submorphs detect: [:aSubmorph | aSubmorph isKindOf: TextModelMorph]! !

