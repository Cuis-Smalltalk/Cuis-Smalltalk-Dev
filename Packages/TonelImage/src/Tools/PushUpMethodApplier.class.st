Class {
	#name : 'PushUpMethodApplier',
	#superclass : 'TransferMethodApplier',
	#instVars : [
		'subclassesToRemoveMethodsFrom',
		'dependantMethods'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> askMethodsToPushUpFrom: aDependantMethods [

	| answer |

	answer := PopUpMenu 
		withCaption: (self withDependantMethodsMessage: aDependantMethods) 
		chooseFrom: #('Only PushUp this method' 'PushUp this method and all the dependant methods').

	answer = 1 ifTrue: [ ^dependantMethods := Set new ].
	answer = 2 ifTrue: [ ^dependantMethods := aDependantMethods ].
	self endRequest 
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> askMethodsToRemoveFrom: withEquivalentMethod [

	| answer |
	
	answer := PopUpMenu 
		withCaption: (self withEquivalentMethodsMessage: withEquivalentMethod) 
		chooseFrom: #('Only PushUp' 'PushUp and delete all equivalent methods').

	answer = 1 ifTrue: [ ^subclassesToRemoveMethodsFrom := Dictionary new ].
	answer = 2 ifTrue: [ ^subclassesToRemoveMethodsFrom := withEquivalentMethod ].
	self endRequest 
]

{ #category : 'refactoring - creation' }
PushUpMethodApplier >> createRefactoring [

	^self refactoringClass for: methodToTransfer additionallyRemovingFrom: subclassesToRemoveMethodsFrom pushingUp: dependantMethods
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> implementsEquivalentMethod: aSubclass [

	^ aSubclass ~= methodToTransfer methodClass and: [
		aSubclass
			compiledMethodAt: methodToTransfer selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: methodToTransfer ]
			ifAbsent: [ false ]]
]

{ #category : 'initialization' }
PushUpMethodApplier >> initialize  [

	subclassesToRemoveMethodsFrom := Dictionary new.
	dependantMethods := Set new.
]

{ #category : 'refactoring - creation' }
PushUpMethodApplier >> refactoringClass [

	^PushUpMethod
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> requestRefactoringParameters [

	self 
		selectDependantMethods;
		selectSiblingsWithEquivalentMethod
		
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> selectDependantMethods [

	| methods |

	methods := ((self refactoringClass selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: methodToTransfer )
	                   collect: [ :selector | methodToTransfer methodClass >> selector ]) asSet.

	methods ifNotEmpty: [ self askMethodsToPushUpFrom: methods ].
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> selectSiblingsWithEquivalentMethod [

	| methods withEquivalentMethod |
	
	methods := Set withAll: dependantMethods.
	methods add: methodToTransfer.
	
	withEquivalentMethod := self refactoringClass equivalentMethodsOfSiblingsOfAll: methods.

	withEquivalentMethod ifNotEmpty: [ self askMethodsToRemoveFrom: withEquivalentMethod ].

]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> withDependantMethodsMessage: aDependantMethods [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: methodToTransfer selector;
			nextPutAll: ' has the following dependant method(s): ';
			newLine;
			nextPutAll: (aDependantMethods collect: [:method | method selector ] ) asCommaStringAnd;
			newLine;
			nextPutAll: 'How do you want to proceed?' ]
]

{ #category : 'refactoring - parameters request' }
PushUpMethodApplier >> withEquivalentMethodsMessage: withEquivalentMethod [

	^ String streamContents: [ :stream |		
		withEquivalentMethod associationsDo: [:assoc |
			stream
				nextPutAll: assoc key selector;
				nextPutAll: ' has equivalent methods declared in ';
				nextPutAll: assoc value asCommaStringAnd;
				newLine.
		].
		stream nextPutAll: 'How do you want to proceed?' ].
]
