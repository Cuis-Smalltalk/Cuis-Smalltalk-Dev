"
ZeroDivide may be signaled when a mathematical division by 0 is attempted.
"
Class {
	#name : 'ZeroDivide',
	#superclass : 'ArithmeticMessageError',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'library membership' }
ZeroDivide class >> includeInMinimalKernel01 [
	"Making this method part of the MinimalKernel01 library means
	this class is also included, even if no actually used method is added."
]

{ #category : 'signaling' }
ZeroDivide >> floatErrorValue [
	| answerClass answerSign |

	receiver isZero ifTrue: [
		answerClass := receiver isComplex ifTrue: [ receiver class ] ifFalse: [ Float ].
		^ answerClass nan ].

	receiver isComplex ifFalse: [
		answerSign := arguments first isComplex
			ifTrue: [ receiver sign ]
			ifFalse: [ (receiver * arguments first) sign ].
		^ answerSign = -1
			ifTrue: [ Float negativeInfinity ]
			ifFalse: [ Float infinity ]].

	^ receiver class infinity
]

{ #category : 'exceptionDescription' }
ZeroDivide >> isResumable [
	
"Determine whether an exception is resumable."

	^true
]
