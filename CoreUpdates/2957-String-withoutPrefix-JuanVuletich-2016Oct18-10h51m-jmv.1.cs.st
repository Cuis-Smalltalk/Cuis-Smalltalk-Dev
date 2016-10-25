'From Cuis 4.2 of 25 July 2013 [latest update: #2953] on 18 October 2016 at 2:25:55 pm'!

!String methodsFor: 'converting' stamp: 'jmv 10/18/2016 10:57:38'!
withoutPrefix: aString
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]! !

