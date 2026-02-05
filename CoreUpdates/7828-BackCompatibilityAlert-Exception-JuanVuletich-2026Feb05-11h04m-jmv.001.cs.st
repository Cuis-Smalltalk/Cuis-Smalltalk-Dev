'From Cuis7.7 [latest update: #7827] on 5 February 2026 at 11:04:47 am'!
!classDefinition: #BackCompatibilityAlert category: #'Exceptions-Extensions'!
Exception subclass: #BackCompatibilityAlert
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Extensions'!

!BackCompatibilityAlert commentStamp: '<historical>' prior: 0!
BackCompatibilityAlert is the exception signaled when some behavior in Cuis has recently changed creating back compatibility issues.
It is only raised when the new answer is different from the old one, and its purpose is to direct developers attention to the code that needs fixing.!


!BackCompatibilityAlert methodsFor: 'description' stamp: 'jmv 5/Feb/2026 10:22:27'!
isResumable

	^true! !

!BackCompatibilityAlert methodsFor: 'priv handling' stamp: 'jmv 5/Feb/2026 10:22:27'!
defaultAction

	self noHandler! !


!BackCompatibilityAlert class methodsFor: 'services' stamp: 'jmv 5/Feb/2026 10:37:38'!
checkOldAnswer: oldAnswer newAnswer: newAnswer
	| message lines |
	newAnswer = oldAnswer ifFalse: [
		message := thisContext sender printString, ' has changed behavior. Old answer was: ', oldAnswer printString,
			'. New answer is: ', newAnswer printString, '. You need to adjust calling code.'.
		lines := message substringsSeparatedBy: $..
		'' print.
		('=== BackCompatibilityAlert: ', lines first) print.
		2 to: lines size-1 do: [ :i | (lines at: i) print ].
		'-----' print.
		thisContext printStack: 3.
		BackCompatibilityAlert signal: message.
		].! !

