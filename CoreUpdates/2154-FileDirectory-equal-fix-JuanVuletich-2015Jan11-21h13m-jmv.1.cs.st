'From Cuis 4.2 of 25 July 2013 [latest update: #2153] on 11 January 2015 at 9:13:46.546308 pm'!

!FileDirectory methodsFor: 'comparing' stamp: 'jmv 1/11/2015 21:03'!
= aDirectory

	"Any object is equal to itself"
	self == aDirectory ifTrue: [ ^ true ].
	(aDirectory is: #FileDirectory) ifFalse: [ ^false ].

	"Compare two FileDirectory instances."
	^ (pathName asString 
			compare: aDirectory pathName asString 
			caseSensitive: (self isCaseSensitive | aDirectory isCaseSensitive)) = 2! !

