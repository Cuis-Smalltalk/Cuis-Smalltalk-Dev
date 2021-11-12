'From Cuis 5.0 [latest update: #4967] on 9 November 2021 at 4:30:30 pm'!

!Time class methodsFor: 'primitives' stamp: 'jmv 11/9/2021 15:35:03'!
primUtcWithOffset
	"Answer a two element array.
	Prefer #primUtcWithOffset:

	First element is the number of microseconds since the UTC Posix (Unix) epoch, 
	i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	It might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	
	Second element is the current seconds offset from GMT in the local time zone.

	Recent VMs implement this. Older Interpreters might not.
	"
	"
	Time primUtcWithOffset
	Time primUtcWithOffset first / 1000 / 1000 / 60 / 60 / 24 / 365.25
	Time primUtcWithOffset second / 60 / 60.0
		
	(Time primUtcWithOffset first / 1000 / 1000 + Time primUtcWithOffset second) / 60 / 60 / 24 / 365.25
	(DateAndTime now - (DateAndTime year: 1970 month: 1 day: 1)) totalSeconds / 60 / 60 / 24 / 365.25
	"
	<primitive: 'primitiveUtcWithOffset'>
	^nil! !

