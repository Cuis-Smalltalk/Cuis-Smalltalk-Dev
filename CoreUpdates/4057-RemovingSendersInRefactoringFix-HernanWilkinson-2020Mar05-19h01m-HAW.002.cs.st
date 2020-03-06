'From Cuis 5.0 [latest update: #4056] on 5 March 2020 at 9:55:36 pm'!

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 3/5/2020 21:07:36'!
changeSenders
	
	applier senders: model messageList
	! !

!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 3/5/2020 19:21:40'!
refactor

	self changeSenders.
	super refactor ! !


!ChangeSelectorKeepingParameters class methodsFor: 'convertion' stamp: 'HAW 3/5/2020 21:22:32'!
collectCompiledMethodsOf: aCollectionOfMethodReferences

	^aCollectionOfMethodReferences collect: [ :aMethodReference | aMethodReference compiledMethod ].
	
	! !


!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/5/2020 20:59:45'!
convertSendersToCompiledMethods

	senders := senders collect: [ :aMethodReference | aMethodReference compiledMethod ]! !


!ChangeSelectorWizardStepWindow class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:05:27'!
openFrom: aChangeSelectorApplier methods: methods label: aLabel selecting: somethingToSelect 

	| window |
	
	window := self openMessageList: methods label: aLabel autoSelect: somethingToSelect.
	window initializeFrom: aChangeSelectorApplier.
	
	^window 

! !


!ChangeSelectorImplementorsStepWindow class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:04:09'!
openFrom: aChangeSelectorApplier

	^self 
		openFrom: aChangeSelectorApplier 
		methods: (self methodReferencesOf: aChangeSelectorApplier implementors) 
		label: 'Implementors of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: nil  
! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 3/5/2020 19:25:10'!
seeImplementors
	
	self changeSenders.
	self delete.
	
	ChangeSelectorImplementorsStepWindow openFrom: applier! !


!ChangeSelectorSendersStepWindow class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:05:36'!
openFrom: aChangeSelectorApplier

	^self 
		openFrom: aChangeSelectorApplier 
		methods: aChangeSelectorApplier senders asOrderedCollection 
		label: 'Senders of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: aChangeSelectorApplier oldSelector ! !


!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/5/2020 19:47:57'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	| categories |
	
	categories := Set new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inHierarchyOf: aClass 
		doingPerClass: [:aClassInHierarchy | categories add: aClassInHierarchy category ].
		
	categories do: [:aCategory | 
		self 
			addImplementorsOf: anOldSelector 
			to: implementors 
			andSendersTo: senders 
			inCategory: aCategory 
			organizedBy: anOrganization ].
! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/5/2020 19:48:17'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	| classesInCategory |
	
	classesInCategory := anOrganization classesAt: aCategory.
	classesInCategory do: [ :aPotentialClassToRefactor | 
		self 
			addImplementorsOf: anOldSelector 
			to: implementors 
			andSendersTo: senders 
			forClassAndMetaOf: aPotentialClassToRefactor ].
! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/5/2020 19:48:32'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inHierarchyOf: aClass 
		doingPerClass: [ :aClassInHierarchy | ]
		
	! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/5/2020 19:48:48'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: aBlock

	| highestClassImplementingOldSelector |
	
	highestClassImplementingOldSelector := aClass highestClassImplementing: anOldSelector ifNone: [ aClass ].
	highestClassImplementingOldSelector theNonMetaClass withAllSubclassesDo: [ :aPotentialClassToRefactor |
		aPotentialClassToRefactor isMeta ifFalse: [ 
			self 
				addImplementorsOf: anOldSelector 
				to: implementors 
				andSendersTo: senders 
				forClassAndMetaOf: aPotentialClassToRefactor.
			aBlock value: aPotentialClassToRefactor ]]
	! !

!ChangeSelector class methodsFor: 'implementors and senders' stamp: 'HAW 3/5/2020 19:49:40'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor
	
	| potentialImplementor |
	
	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].
	
	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector | 
		senders add: (MethodReference class: aPotentialClassToRefactor selector: aSelector) ]! !


!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:49:45'!
from: anOldSelector to: aNewSelector in: aClassToRefactor

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		forClassAndMetaOf: aClassToRefactor theNonMetaClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:51:08'!
from: anOldSelector to: aNewSelector inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

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
! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:51:17'!
from: anOldSelector to: aNewSelector inCategoryOf: aClass organizedBy: anOrganization

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
! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:51:28'!
from: anOldSelector to: aNewSelector inHierarchyOf: aClass

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	
	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inHierarchyOf: aClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)

	! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/5/2020 21:51:40'!
from: anOldSelector to: aNewSelector inSystem: aSystem

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self 
		addImplementorsOf: anOldSelector 
		to: implementors 
		andSendersTo: senders 
		inSystem: aSystem.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: (self collectCompiledMethodsOf: senders)
	! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/5/2020 21:06:18'!
createSenders

	^Set new
! !

!ChangeSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/5/2020 19:54:38'!
createAndApplyRefactoring

	self convertSendersToCompiledMethods.
	
	self 
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		informChangesToBrowser.
			
	shouldShowChanges ifTrue: [ self showChanges ]
	
			! !


!ChangeSelectorKeepingParameters class reorganize!
('instance creation' from:to:implementors:senders: from:to:in: from:to:inCategoriesAndHierarchyOf:organizedBy: from:to:inCategoryOf:organizedBy: from:to:inHierarchyOf: from:to:inSystem:)
('pre-conditions' assert:hasTheSameNumberOfArgumentsAs: assert:isNotEqualTo: assert:isOfSameTypeAs: assertIsValidToRenameFrom:to: assertNoImplementorClassIn:implements: isRenamingBetweenBinary:andKeywordOfOneParameter: warnIfImplementionsOf:overridesImplementationInSuperclassesOf:)
('exceptions' errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs: errorMessageForNewSelector:implementedInAll: errorMessageForNewSelector:isNotOfSameTypeAs: newSelectorCanNotBeEmptyErrorMessage newSelectorEqualToOldSelectorErrorMessage oldSelectorCanNotBeEmptyErrorMessage signalNewSelector:doesNotHaveSameNumberOfArgumentsAs: signalNewSelector:implementedInAll: signalNewSelector:isNotOfSameTypeAs: signalNewSelectorEqualToOldSelector)
('warnings' warnImplementionOf:in:willOverrideImplementationIn: warningMessageForImplementationOf:in:willOverrideImplementationIn:)
('convertion' collectCompiledMethodsOf:)
!

