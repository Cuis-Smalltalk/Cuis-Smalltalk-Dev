Class {
	#name : 'WeakActionSequence',
	#superclass : 'Array',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'converting' }
WeakActionSequence >> asActionSequence [

	^self
]

{ #category : 'converting' }
WeakActionSequence >> asMinimalRepresentation [

	| valid |
	valid := self reject: [:e | e isReceiverOrAnyArgumentGarbage ].
	valid size = 0
		ifTrue: [ ^nil ].
	valid size = 1
		ifTrue: [ ^valid first ].
	^valid
]

{ #category : 'printing' }
WeakActionSequence >> printOn: aStream [

	self size < 2 ifTrue: [^super printOn: aStream].
	aStream nextPutAll: '#('.
	self
		do: [:each | each printOn: aStream]
		separatedBy: [aStream newLine].
	aStream nextPut: $)
]

{ #category : 'evaluating' }
WeakActionSequence >> value [
	"Do the same as my parent, but make sure that all actions that do not  
	give errors are evaluated before resignaling the ones that gave errors  
	(giving the chance to clients to handle them)."

	^self valueStartingFrom: 1
]

{ #category : 'evaluating' }
WeakActionSequence >> valueStartingFrom: startIndex [
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each value ]
			on: UnhandledError
			do: [:exc | 
				self valueStartingFrom: index + 1.
				exc pass]].
	^ answer
]

{ #category : 'evaluating' }
WeakActionSequence >> valueWithArguments: anArray  [
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	^self valueWithArguments: anArray startingFrom: 1
]

{ #category : 'evaluating' }
WeakActionSequence >> valueWithArguments: anArray startingFrom: startIndex [
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each valueWithArguments: anArray ]
			on: UnhandledError
			do: [:exc | 
				self valueWithArguments: anArray startingFrom: index + 1.
				exc pass]].
	^ answer
]
