'From Cuis 4.2 of 25 July 2013 [latest update: #2081] on 23 August 2014 at 10:52:39.973233 pm'!

!Number methodsFor: 'converting' stamp: 'jmv 8/23/2014 22:51'!
degreesToRadiansMinutes: minutes seconds: seconds
	" deg degreesToRadiansWithMinutes: min seconds: secs
	Take receiver and arguments as degrees, minutes and seconds. Answer radians"

	^(seconds / 60.0 + minutes / 60.0 + self) degreesToRadians! !


!Point methodsFor: 'polar coordinates' stamp: 'jmv 8/21/2014 16:54'!
rho
	"Answer the receiver's radius in polar coordinate system."

	^(self dotProduct: self) sqrt! !

!Point methodsFor: 'private' stamp: 'jmv 8/21/2014 16:52'!
setRho: rho theta: radians

	x _ rho asFloat * radians cos.
	y _ rho asFloat * radians sin! !


!Point class methodsFor: 'instance creation' stamp: 'jmv 8/21/2014 16:53'!
rho: rho theta: radians
	"Answer an instance of me with polar coordinates rho and theta."

	^self new setRho: rho theta: radians! !


!Point methodsFor: 'polar coordinates' stamp: 'jmv 8/21/2014 16:55'!
r
	"Answer the receiver's radius in polar coordinate system."

	^self rho! !

!Point methodsFor: 'private' stamp: 'jmv 8/21/2014 16:53'!
setR: rho degrees: degrees 

	self setRho: rho theta: degrees asFloat degreesToRadians! !

