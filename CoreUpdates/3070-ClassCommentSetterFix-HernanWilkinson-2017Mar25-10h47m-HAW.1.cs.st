'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 25 March 2017 at 10:48:23 am'!

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'HAW 3/25/2017 10:48:00'!
classComment: aString 
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	aString ifNil: [ ^classComment _ nil ].
	
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [aString size = 0
			ifTrue: [classComment _ nil]
			ifFalse: [
				self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"! !

