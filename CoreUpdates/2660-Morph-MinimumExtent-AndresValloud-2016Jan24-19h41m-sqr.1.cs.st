'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 24 January 2016 at 7:48:47.717287 pm'!

!Morph methodsFor: 'geometry' stamp: 'sqr 1/13/2016 22:08'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least"

	self flag: #jmvVer2.	"in owner's coordinates?"
	^self valueOfProperty: #minimumExtent ifAbsent: [1@1]! !

