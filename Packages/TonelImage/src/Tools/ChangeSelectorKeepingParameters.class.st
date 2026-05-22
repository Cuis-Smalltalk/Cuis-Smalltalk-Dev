Class {
	#name : 'ChangeSelectorKeepingParameters',
	#superclass : 'ChangeSelector',
	#instVars : [
		'newSelectorKeywords',
		'currentImplementorMethodNode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector [

	aNewSelector numArgs ~= anOldSelector numArgs ifTrue: [ self signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector ]
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> assert: aNewSelector isNotEqualTo: anOldSelector [

	 aNewSelector = anOldSelector ifTrue: [ self signalNewSelectorEqualToOldSelector]
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> assert: aNewSelector isOfSameTypeAs: anOldSelector [

	(self isRenamingBetweenBinary: anOldSelector andKeywordOfOneParameter: aNewSelector) ifTrue: [ ^self ].
	(self isRenamingBetweenBinary: aNewSelector andKeywordOfOneParameter: anOldSelector) ifTrue: [ ^self ].

	aNewSelector precedence ~= anOldSelector precedence ifTrue: [
		self signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector ]
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> assertIsValidToRenameFrom: anOldSelector to: aNewSelector [

	self assertIsNotEmpty: anOldSelector signalMessageText: self oldSelectorCanNotBeEmptyErrorMessage.
	self assertIsNotEmpty: aNewSelector signalMessageText: self newSelectorCanNotBeEmptyErrorMessage.
	self assert: aNewSelector isNotEqualTo: anOldSelector.
	self assert: aNewSelector isOfSameTypeAs: anOldSelector.
	self assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector.
	
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> assertNoImplementorClassIn: implementorsCollection implements: aNewSelector [

	| classesImplementingNewSelector |

	classesImplementingNewSelector := implementorsCollection
		select: [ :anImplementor | anImplementor methodClass includesSelector: aNewSelector ]
		thenCollect: [ :anImplementor | anImplementor methodClass ].

	classesImplementingNewSelector notEmpty ifTrue: [ self signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector ]
]

{ #category : 'convertion' }
ChangeSelectorKeepingParameters class >> collectCompiledMethodsOf: aCollectionOfMethodReferences [

	^aCollectionOfMethodReferences collect: [ :aMethodReference | aMethodReference compiledMethod ].
	
	
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector [

	^'New selector #', aNewSelector, ' does not have the same number of arguments as #', anOldSelector 
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector [

	^'Can not rename because #', aNewSelector, ' is implemented in: ', classesImplementingNewSelector asCommaStringAnd 
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector [

	^'New selector #', aNewSelector, ' is not of same type as #', anOldSelector 
]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders [

	self assertIsValidToRenameFrom: anOldSelector to: aNewSelector.
	self assertAllImplementors: aCollectionOfImplementors haveSame: anOldSelector.
	self assertNoImplementorClassIn: aCollectionOfImplementors implements: aNewSelector.
	self assertAllSenders: aCollectionOfSenders send: anOldSelector.

	self warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: aCollectionOfImplementors.

	^self new initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	
]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector in: aClassToRefactor [

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		forClassAndMetaOf: aClassToRefactor theNonMetaClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)
]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization [

	| implementors senders |

	implementors := IdentitySet new.
	senders := Set new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inCategoriesAndHierarchyOf: aClass 
		organizedBy: anOrganization.

	"I have to convert senders to OrderedCollection because CompiledMethod>>#= does not compare the class
	where it is installed - Hernan"
	^self 
		from: anOldSelector 
		to: aNewSelector 
		implementors: implementors 
		senders: (self collectCompiledMethodsOf: senders asOrderedCollection)

]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector inCategoryOf: aClass organizedBy: anOrganization [

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders
		inCategory: aClass category 
		organizedBy: anOrganization.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)

]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector inHierarchyOf: aClass [

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	
	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inHierarchyOf: aClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)

	
]

{ #category : 'instance creation' }
ChangeSelectorKeepingParameters class >> from: anOldSelector to: aNewSelector inSystem: aSystem [

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inSystem: aSystem.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)
	
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> isRenamingBetweenBinary: aPotentiallyBinarySelector andKeywordOfOneParameter: aPotentiallyKeywordSelector [

	^aPotentiallyBinarySelector isInfix
		and: [ aPotentiallyKeywordSelector isKeyword
		and: [ aPotentiallyKeywordSelector numArgs = 1 ]]

]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> newSelectorCanNotBeEmptyErrorMessage [

	^'New selector can not be empty'
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> newSelectorEqualToOldSelectorErrorMessage [

	^'There is nothing to rename when new selector is equals to old selector'
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> oldSelectorCanNotBeEmptyErrorMessage [

	^'Old selector can not be empty'
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector [

	self refactoringError: (self errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector).
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector [

	self refactoringError: (self errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector).
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector [

	self refactoringError: (self errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector).
]

{ #category : 'exceptions' }
ChangeSelectorKeepingParameters class >> signalNewSelectorEqualToOldSelector [

	self refactoringError: self newSelectorEqualToOldSelectorErrorMessage.
]

{ #category : 'pre-conditions' }
ChangeSelectorKeepingParameters class >> warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: implementorsCollection [

	implementorsCollection do: [:anImplementor |
		anImplementor methodClass
			withSuperclassThatIncludesSelector: aNewSelector
			do: [ :aSuperclass | self warnImplementionOf: aNewSelector in: anImplementor methodClass willOverrideImplementationIn: aSuperclass ]
			ifNone: nil]
]

{ #category : 'warnings' }
ChangeSelectorKeepingParameters class >> warnImplementionOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass [

	self refactoringWarning: (self warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass).
]

{ #category : 'warnings' }
ChangeSelectorKeepingParameters class >> warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass [

	^'Implemention of #', aNewSelector, ' in ', aClass name, ' will override implementation in ', aSuperclass name
]

{ #category : 'create new implementors - private' }
ChangeSelectorKeepingParameters >> addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords [

	self subclassResponsibility 
]

{ #category : 'rename senders - private' }
ChangeSelectorKeepingParameters >> addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords [

	| oldSelectorKeywordsRanges |

	oldSelectorKeywordsRanges := self messageSendSelectorKeywordPositionsIn: aMethodNode.
	self addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords.
	
	
]

{ #category : 'rename senders - private' }
ChangeSelectorKeepingParameters >> addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords [

	oldSelectorKeywordsRanges do: [ :aMessageSendSelectorRanges |
		aMessageSendSelectorRanges withIndexDo: [ :aRange :index | rangesToKeywords add: aRange -> (newSelectorKeywords at: index) ]].
	
	
	
]

{ #category : 'create new implementors - private' }
ChangeSelectorKeepingParameters >> implementorNewSourceCodeOf: anImplementor [

	|  newSource rangesToNewKeywords |

	rangesToNewKeywords := OrderedCollection new.
	currentImplementorMethodNode := anImplementor notOptimizedMethodNode.

	currentImplementorMethodNode selectorKeywordsPositions withIndexDo: [ :aKeywordRange :index |
		self addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords ].

	newSource := anImplementor sourceCode copyReplacing: rangesToNewKeywords.
	^newSource
]

{ #category : 'initialization' }
ChangeSelectorKeepingParameters >> initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders [

	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.

	newSelectorKeywords := newSelector keywords.
	
]

{ #category : 'rename senders - private' }
ChangeSelectorKeepingParameters >> messageSendSelectorKeywordPositionsIn: aMethodNode [
	
	^aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].
]
