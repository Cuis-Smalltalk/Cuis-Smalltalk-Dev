Class {
	#name : 'PushUpMethod',
	#superclass : 'TransferMethod',
	#instVars : [
		'subclassesToRemoveMethodFrom',
		'methods'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'warnings' }
PushUpMethod class >> accessingInstanceVariableWarningMessage: accessedInstanceVariables [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The instance variable(s) ';
			nextPutAll: accessedInstanceVariables asCommaStringAnd;
			nextPutAll: ' will be pushed up' ]
]

{ #category : 'pre-conditions' }
PushUpMethod class >> assert: subclassesToRemoveMethodFrom areSiblingsOf: aClassToPushUpMethodFrom [

	| noSiblings |
	
	noSiblings := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass superclass = aClassToPushUpMethodFrom superclass ].
	
	noSiblings ifNotEmpty: [ self signalNoSiblings: noSiblings ].
]

{ #category : 'pre-conditions' }
PushUpMethod class >> assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom [
	
	| notEquivalent |
	
	notEquivalent := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass >> aMethodToPushUp selector equivalentTo: aMethodToPushUp ].
	
	notEquivalent ifNotEmpty: [ self signalNotEquivalentMethodsFor: notEquivalent ].
]

{ #category : 'pre-conditions' }
PushUpMethod class >> assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom [
	
	| subclassesNotImplementing |
	
	subclassesNotImplementing := subclassesToRemoveMethodFrom reject: [ :aSubclass | 
		aSubclass includesSelector: aMethodToPushUp selector ].
	
	subclassesNotImplementing ifNotEmpty: [
		self signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing ]
	
]

{ #category : 'pre-conditions' }
PushUpMethod class >> assert: aMethodToRemove isValidToRemoveFrom: subclassesToRemoveMethodFrom [

	self assert: subclassesToRemoveMethodFrom areSiblingsOf: aMethodToRemove methodClass.
	self assert: aMethodToRemove isImplementedIn: subclassesToRemoveMethodFrom.
	self assert: aMethodToRemove isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom.


]

{ #category : 'pre-conditions' }
PushUpMethod class >> assertIsValidToPushUp: methods [

	self warnIfReferencesInstanceVariables: methods.
	self warnIfDifferentImplementationInSuperclass: methods
]

{ #category : 'pre-conditions' }
PushUpMethod class >> assertIsValidToRemove: subclassesToRemoveMethodFrom [

	subclassesToRemoveMethodFrom associationsDo: [:assoc | 
		self assert: assoc key isValidToRemoveFrom: assoc value].


]

{ #category : 'implemented superclass' }
PushUpMethod class >> checkIfMethodIsImplementedInSuperclass: aMethodToTransfer [

	^aMethodToTransfer methodClass superclass compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superClassMethod | (self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superClassMethod) not]
		ifAbsent: [ false ]
]

{ #category : 'equivalent methods' }
PushUpMethod class >> equivalentMethodsOfSiblingsOfAll: methods [

	| allEquivalentMethods |
	
	allEquivalentMethods := Dictionary new.
	
	methods do: [:method | | equivalentMethods |
		equivalentMethods := method methodClass superclass subclasses select: [ :aSubclass | self is: method equivalentIn: aSubclass ].
		equivalentMethods ifNotEmpty: [ allEquivalentMethods at: method put: equivalentMethods ]].

	^allEquivalentMethods 
]

{ #category : 'exceptions' }
PushUpMethod class >> errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing [

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method because is not implemented in:';
			newLine.

		subclassesNotImplementing asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
		
]

{ #category : 'exceptions' }
PushUpMethod class >> errorMessageForNoSiblings: noSiblings [

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method from classes that are not siblings:';
			newLine.

		noSiblings asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			
]

{ #category : 'exceptions' }
PushUpMethod class >> errorMessageForNotEquivalentMethods: notEquivalent [

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove not equivalent methods from';
			newLine.

		notEquivalent asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			
]

{ #category : 'instance creation' }
PushUpMethod class >> for: aMethodToPushUp [

	^self for: aMethodToPushUp additionallyRemovingFrom: Dictionary new pushingUp: Set new.
]

{ #category : 'instance creation' }
PushUpMethod class >> for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom pushingUp: dependantMethods [
	
	dependantMethods add: aMethodToPushUp.

	self assertIsValidToPushUp: dependantMethods.
	self assertIsValidToRemove: subclassesToRemoveMethodFrom.
	
	^self new 
		initializeForAll: dependantMethods 
		of: aMethodToPushUp 
		additionallyRemovingFrom: subclassesToRemoveMethodFrom.
]

{ #category : 'equivalent methods' }
PushUpMethod class >> is: aMethod equivalentIn: aSubclass [

	^ aSubclass ~= aMethod methodClass and: [
		aSubclass
			compiledMethodAt: aMethod selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: aMethod ]
			ifAbsent: [ false ]]
]

{ #category : 'warnings' }
PushUpMethod class >> methodWithDifferentImplementationInSuperclass: methods [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The method(s): ';
			nextPutAll: methods asCommaStringAnd;
			nextPutAll: ' have an implementation not equivalent in the superclass and will be pushed up' ]
]

{ #category : 'instance variables' }
PushUpMethod class >> selectAccessedInstanceVariables: aMethodToPushUp [

	^aMethodToPushUp methodClass instVarNames
		select: [ :instVarName |  aMethodToPushUp accessesInstanceVariable: instVarName ]
]

{ #category : 'instance variables' }
PushUpMethod class >> selectAccessedInstanceVariablesThatCanBePushedUpFrom: aMethodToPushUp [

	| accessedInstanceVariables |

	accessedInstanceVariables := self selectAccessedInstanceVariables: aMethodToPushUp.

	accessedInstanceVariables do: [ :variable | PushUpInstanceVariable assertCanMove: variable from: aMethodToPushUp methodClass ].

	^accessedInstanceVariables
]

{ #category : 'dependant methods' }
PushUpMethod class >> selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: aMethodToTransfer [

	| selectors |

	selectors := Set new.
	self selectDependantSelectorsFrom: aMethodToTransfer using: selectors.
	
	selectors remove: aMethodToTransfer selector.
	
	^selectors
]

{ #category : 'dependant methods' }
PushUpMethod class >> selectDependantSelectorsFrom: aMethod using: visitedSelectors [

	| selectors |

	visitedSelectors add: aMethod selector.

	selectors := self selectorsSentToSelf: aMethod.

	(selectors difference: visitedSelectors) do: [:selector | 
		self selectDependantSelectorsFrom: aMethod methodClass >> selector using: visitedSelectors]
]

{ #category : 'instance variables' }
PushUpMethod class >> selectInstanceVariablesToPushUpFrom: methods [

	^ (methods collect: [:method | self selectAccessedInstanceVariablesThatCanBePushedUpFrom: method ]) asArray flatten asSet 

]

{ #category : 'dependant methods' }
PushUpMethod class >> selectorsSentToSelf: aMethod [

	^ aMethod methodClass selectors select: [ :method | aMethod methodNode sendsMessageToSelf: method]
]

{ #category : 'exceptions' }
PushUpMethod class >> signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing [
	
	self refactoringError: (self errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing)
]

{ #category : 'exceptions' }
PushUpMethod class >> signalNoSiblings: noSiblings  [

	self refactoringError: (self errorMessageForNoSiblings: noSiblings)
]

{ #category : 'exceptions' }
PushUpMethod class >> signalNotEquivalentMethodsFor: notEquivalent [

	self refactoringError: (self errorMessageForNotEquivalentMethods: notEquivalent)
]

{ #category : 'warnings' }
PushUpMethod class >> warnIfDifferentImplementationInSuperclass: methods [

	| methodsImplementedInSuperclass |
	
	methodsImplementedInSuperclass := methods 	
		select: [ :method | self checkIfMethodIsImplementedInSuperclass: method ]
		thenCollect: [ :method | method selector].
		
	methodsImplementedInSuperclass ifNotEmpty: [ 
		self refactoringWarning: (self methodWithDifferentImplementationInSuperclass: methodsImplementedInSuperclass)].
	
	

]

{ #category : 'warnings' }
PushUpMethod class >> warnIfReferencesInstanceVariables: methods [

	| instanceVariablesToPushUp |
	
	instanceVariablesToPushUp := self selectInstanceVariablesToPushUpFrom: methods.
	
	instanceVariablesToPushUp ifNotEmpty: [ 
		self refactoringWarning: (self accessingInstanceVariableWarningMessage: instanceVariablesToPushUp)].


]

{ #category : 'applying - private' }
PushUpMethod >> compileMethodInSuperclass: aMethod [

	aMethod methodClass superclass
		compile: aMethod sourceCode
		classified: (self methodCategory: aMethod).
]

{ #category : 'initialization' }
PushUpMethod >> initializeForAll: aMethods of: aMethodToPushUp additionallyRemovingFrom: aSubclassesToRemoveMethodFrom [

	self initializeFor: aMethodToPushUp.
	methods := aMethods.
	subclassesToRemoveMethodFrom := aSubclassesToRemoveMethodFrom.
]

{ #category : 'applying - private' }
PushUpMethod >> moveMethod [

	self 
		pushUpAccessedVariables;
		pushUpMethods.
]

{ #category : 'applying - private' }
PushUpMethod >> pushUpAccessedVariables [

	|instanceVariablesToPushUp methodClass|
	
	instanceVariablesToPushUp := self class selectInstanceVariablesToPushUpFrom: methods.
	methodClass := method methodClass.

	instanceVariablesToPushUp do: [:var | (PushUpInstanceVariable named: var from: methodClass) apply ]
]

{ #category : 'applying - private' }
PushUpMethod >> pushUpMethods [

	methods do: [ :methodToPushUp | 
		self 
			compileMethodInSuperclass: methodToPushUp;
			removeMethod: methodToPushUp ]
]

{ #category : 'applying - private' }
PushUpMethod >> removeMethod [

	subclassesToRemoveMethodFrom keysAndValuesDo: [ :methodWithSelectorToRemove :classesToRemoveMethodFrom |
		classesToRemoveMethodFrom do: [ :aClassToRemoveMethodFrom | 
			aClassToRemoveMethodFrom removeSelector: methodWithSelectorToRemove selector ]].
	
	
]
