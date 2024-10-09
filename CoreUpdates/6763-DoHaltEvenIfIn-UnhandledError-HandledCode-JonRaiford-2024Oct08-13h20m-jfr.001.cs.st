'From Cuis7.1 [latest update: #6761] on 8 October 2024 at 01:28:41 PM'!

!Halt methodsFor: 'description' stamp: 'jfr 10/8/2024 13:27:48'!
noHandler
	"Halt should not be caught by an UnhandledError handler"
	
	UnhandledError defaultActionForException: self! !


!UnhandledError class methodsFor: 'as yet unclassified' stamp: 'jfr 10/8/2024 13:25:39'!
defaultActionForException: anError

	^ self new
		exception: anError;
		defaultAction! !

