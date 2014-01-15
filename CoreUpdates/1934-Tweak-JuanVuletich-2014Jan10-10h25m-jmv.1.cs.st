'From Cuis 4.2 of 25 July 2013 [latest update: #1933] on 10 January 2014 at 10:25:44.049918 am'!

!Clipboard methodsFor: 'private' stamp: 'jmv 1/10/2014 10:25'!
stringOrIdFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isString ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject! !

