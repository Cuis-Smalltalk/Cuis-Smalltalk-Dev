'From Cuis 5.0 of 7 November 2016 [latest update: #3080] on 13 May 2017 at 7:48:17 pm'!

!Week methodsFor: 'public protocol' stamp: 'jmv 5/13/2017 14:37:37'!
weekNumber

	| thursday |
	thursday _ self start + 3 days.
	^thursday dayOfYear-1 // 7 + 1! !

!Week methodsFor: 'public protocol' stamp: 'jmv 5/13/2017 14:37:18'!
yearNumber

	| thursday |
	thursday _ self start + 3 days.
	^thursday yearNumber! !


!Year methodsFor: 'public protocol' stamp: 'jmv 5/13/2017 14:51:58'!
yearNumber

	^ start yearNumber! !


!Week methodsFor: 'squeak protocol' stamp: 'jmv 5/13/2017 14:38:20'!
printOn: aStream
	"
		'2008-W52' asWeek.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
	"
	self yearNumber printOn: aStream.
	aStream nextPutAll: '-W'.
	self weekNumber printOn: aStream length: 2 zeroPadded: true! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/13/2017 14:59:27'!
indexOfDay: aSymbol
	"
	(Week indexOfDay: #Sunday) = 7
	(Week nameOfDay: 7) = #Sunday 
	"

	^ self dayNames indexOf: aSymbol! !

