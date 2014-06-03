'From Cuis 4.2 of 25 July 2013 [latest update: #1982] on 2 June 2014 at 10:57:44.685417 am'!

!DirectoryEntry methodsFor: 'access' stamp: 'bp 6/2/2014 10:49'!
splitNameVersionExtension
	" answer an array with the root name, version # and extension."
	^directory splitNameVersionExtensionFor: self name! !

