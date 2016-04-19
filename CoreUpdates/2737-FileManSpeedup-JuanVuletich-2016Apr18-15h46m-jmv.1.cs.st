'From Cuis 4.2 of 25 July 2013 [latest update: #2734] on 18 April 2016 at 4:02:10.50531 pm'!

!FmDirectoryEntry methodsFor: 'testing' stamp: 'jmv 4/18/2016 16:00'!
exists
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^true ].
	
	pathName _ self pathName.
	pathName = self fileAccessor slash ifTrue: [ ^ true ].

	^self fileAccessor basicDirectoryExists: pathName! !

