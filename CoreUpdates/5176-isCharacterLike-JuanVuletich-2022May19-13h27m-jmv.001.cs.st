'From Cuis 6.0 [latest update: #5174] on 19 May 2022 at 1:27:55 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 5/17/2022 16:38:18'!
isCharacterLike
	"Overridden to return true in Character and UnicodeCodePoint."
	^ false! !


!Character methodsFor: 'testing' stamp: 'jmv 5/17/2022 16:38:27'!
isCharacterLike
	^true! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/17/2022 16:37:43'!
isCharacterLike
	^true! !

