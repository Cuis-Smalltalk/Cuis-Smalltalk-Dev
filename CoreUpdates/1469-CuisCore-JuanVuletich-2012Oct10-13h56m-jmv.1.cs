'From Cuis 4.0 of 21 April 2012 [latest update: #1468] on 10 October 2012 at 4:25:25 pm'!

!CodeFile methodsFor: 'initialize' stamp: 'jmv 10/10/2012 13:56'!
fromFileNamed: aName
	| stream |
	fullName _ aName.
	stream _ FileStream readOnlyFileNamed: aName.
	[ self buildFrom: stream ] ensure: [ stream close ]! !

