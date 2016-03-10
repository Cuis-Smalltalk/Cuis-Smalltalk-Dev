'From Cuis 4.2 of 25 July 2013 [latest update: #2703] on 10 March 2016 at 9:19:22.990743 am'!

!Metaclass class methodsFor: 'anti-corruption' stamp: 'jmv 3/10/2016 09:18'!
isScarySelector: newbieSelector

	"Return true if newbieSelector is already a part of Metaclass protocol."
	(#(new new:) includes: newbieSelector) ifTrue: [ ^false ].
	(Metaclass includesSelector: newbieSelector) ifTrue: [^ true].
	(ClassDescription includesSelector: newbieSelector) ifTrue: [^ true].
	(Behavior includesSelector: newbieSelector) ifTrue: [^ true].
	^ false
! !

