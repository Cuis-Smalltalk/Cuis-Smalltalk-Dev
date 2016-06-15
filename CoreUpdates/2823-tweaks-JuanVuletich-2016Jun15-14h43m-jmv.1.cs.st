'From Cuis 4.2 of 25 July 2013 [latest update: #2822] on 15 June 2016 at 3:40:55.482932 pm'!

!AndreasSystemProfiler methodsFor: 'accessing result' stamp: 'jmv 6/15/2016 15:10'!
totalMilliseconds
	^ totalTime! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 6/15/2016 15:16'!
printHMSOn: aStream separator: aChar
	"Print just hh:mm:ss"
	aStream
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self second asString padded: #left to: 2 with: $0).
! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 6/15/2016 15:16'!
printHMSOn: aStream
	"Print just hh:mm:ss"
	self printHMSOn: aStream separator: $:! !

