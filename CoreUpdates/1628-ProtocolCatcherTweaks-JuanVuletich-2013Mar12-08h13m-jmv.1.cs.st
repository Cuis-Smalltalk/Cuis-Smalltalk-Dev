'From Cuis 4.1 of 12 December 2012 [latest update: #1626] on 12 March 2013 at 8:13:36 am'!

!ProtocolCatcher methodsFor: 'accessing' stamp: 'jmv 3/5/2013 22:20'!
realObject: anObject
	realObject _ anObject! !


!ProtocolCatcher class methodsFor: 'instance creation' stamp: 'jmv 3/5/2013 22:20'!
on: anObject
	| catcher |
	catcher _ self new.
	catcher realObject: anObject.
	^catcher! !

