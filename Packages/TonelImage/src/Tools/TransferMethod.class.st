Class {
	#name : 'TransferMethod',
	#superclass : 'Refactoring',
	#instVars : [
		'method'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'warnings' }
TransferMethod class >> methodImplementedInSuperclassWarningMessage [

	^'Method with different implementation already exist in superclass'.

	
]

{ #category : 'warnings' }
TransferMethod class >> shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superclassMethod [

	^ (superclassMethod equivalentTo: aMethodToTransfer) or: [ superclassMethod isAbstract ]
]

{ #category : 'warnings' }
TransferMethod class >> warnIfMethodIsImplementedInSuperClass: aMethodToTransfer [

	aMethodToTransfer methodClass superclass
		compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superclassMethod | (self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superclassMethod)
			ifFalse: [ self refactoringWarning: self methodImplementedInSuperclassWarningMessage ]]
		ifAbsent: nil
]

{ #category : 'applying' }
TransferMethod >> apply [

	self
		moveMethod;
		removeMethod
 
]

{ #category : 'applying - private' }
TransferMethod >> implementSubclassResponsibilityIn: aClass [

	^ aClass
		compile: self subclassResponsibilitySourceCode
		classified: self methodCategory
]

{ #category : 'initialization' }
TransferMethod >> initializeFor: aMethodToTransfer [

	method := aMethodToTransfer
]

{ #category : 'applying - private' }
TransferMethod >> methodCategory [

	^self methodCategory: method.
]

{ #category : 'applying - private' }
TransferMethod >> methodCategory: aMethod [

	^aMethod methodClass organization categoryOfElement: aMethod selector
]

{ #category : 'applying - private' }
TransferMethod >> moveMethod [

	self subclassResponsibility 
]

{ #category : 'applying - private' }
TransferMethod >> removeMethod [

	self removeMethod: method.
 
]

{ #category : 'applying - private' }
TransferMethod >> removeMethod: aMethod [

	aMethod methodClass removeSelector: aMethod selector.
 
]

{ #category : 'accessing' }
TransferMethod >> selector [
	
	^method selector 
]

{ #category : 'applying - private' }
TransferMethod >> subclassResponsibilitySourceCode [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selectorAndArgumentsAsString;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]
]
