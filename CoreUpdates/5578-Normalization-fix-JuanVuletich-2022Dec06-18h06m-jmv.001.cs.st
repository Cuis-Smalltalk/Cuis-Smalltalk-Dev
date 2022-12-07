'From Cuis 6.0 [latest update: #5577] on 6 December 2022 at 6:08:52 pm'!

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/6/2022 18:08:40'!
combiningClass: numericCodePoint
	(self isDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CannonicalCombiningClasses at: numericCodePoint ifAbsent: [ 	0 ]! !

