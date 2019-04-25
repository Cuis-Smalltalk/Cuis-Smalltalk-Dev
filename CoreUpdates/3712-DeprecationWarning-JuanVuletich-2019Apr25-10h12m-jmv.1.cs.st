'From Cuis 5.0 of 7 November 2016 [latest update: #3710] on 25 April 2019 at 10:25:07 am'!

!Object methodsFor: 'private' stamp: 'jmv 4/25/2019 10:24:39'!
deprecatedMethod
	"Warn that this method is deprecated and should not be used"

	'========' print.
	thisContext sender print.
	'--------------' print.
	'This method is deprecated. It will be removed from the system. Please change this and any other related senders.' print.
	'--------------' print.
	thisContext sender printStack: 6.
	'========' print.! !


!AbstractFont methodsFor: 'accessing' stamp: 'jmv 4/25/2019 10:21:53'!
height
	"Answer the line spacing. Prefer calling #lineSpacing, that is more explicit.
	Besides, #height is false polymorphism, Fonts are not interchangeable with Form or Rectangle!!"

	self deprecatedMethod.
	^self lineSpacing! !

