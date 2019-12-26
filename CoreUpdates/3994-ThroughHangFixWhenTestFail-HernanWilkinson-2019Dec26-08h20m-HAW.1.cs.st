'From Cuis 5.0 [latest update: #3993] on 26 December 2019 at 8:24:25 am'!

!Process methodsFor: 'debugging' stamp: 'HAW 12/26/2019 08:17:53'!
debug: context title: title full: bool
	"Open debugger on self with context shown on top"

	| topContext |
	
	topContext _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topContext notNil and: [ topContext hasContext: context ]) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: bool.
! !


!TestFailure methodsFor: 'Camp Smalltalk' stamp: 'HAW 12/26/2019 08:21:50'!
defaultAction

	self noHandler! !

!methodRemoval: SyntaxErrorNotification #defaultAction stamp: 'HAW 12/26/2019 08:21:38'!
SyntaxErrorNotification removeSelector: #defaultAction!
