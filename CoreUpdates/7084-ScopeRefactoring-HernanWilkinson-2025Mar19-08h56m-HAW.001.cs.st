'From Cuis7.3 [latest update: #6959] on 19 March 2025 at 7:11:14 pm'!
!classDefinition: #ScopedRefactoring category: #'Tools-Refactoring'!
Refactoring subclass: #ScopedRefactoring
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelector category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #ChangeSelector
	instanceVariableNames: 'oldSelector newSelector implementors senders changes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractAsParameter category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #ExtractAsParameter
	instanceVariableNames: 'sourceMethod addParameter intervals '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #InlineMethod
	instanceVariableNames: 'methodToInline messageSendsToInline updatedSendersCode methodNodeToInline replacementsByMessageSend temporariesDeclarationsByNode temporariesToDeclareByInsertionPoint implementorCompleteSourceRanges removeMethod '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ScopedRefactoringApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ScopedRefactoringApplier
	instanceVariableNames: 'scopeChoice selectedClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorApplier category: #'Tools-Refactoring'!
ScopedRefactoringApplier subclass: #ChangeSelectorApplier
	instanceVariableNames: 'oldSelector implementors senders wizardStepWindow shouldShowChanges browser '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodApplier category: #'Tools-Refactoring'!
ScopedRefactoringApplier subclass: #InlineMethodApplier
	instanceVariableNames: 'selectorToInline implementors messageSends wizardStepWindow shouldShowChanges browser shouldRemoveImplementor triggeringMessageSend shouldInlineTriggeringMessageSendOnly '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!


!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:05:52'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theNonMetaClass.
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theMetaClass.

! !

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:15:52'!
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

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:16:13'!
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

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:16:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	self
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: aClass
		doingPerClass: [ :aClassInHierarchy | ]

	! !

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:15:41'!
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

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:16:33'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem

	aSystem allBehaviorsDo: [ :aPotentialClassToRefactor |
		self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor ].
! !

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:11:50'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not register
	as allowed compiler - Hernan"
	(self canRefactor: aPotentialClassToRefactor) ifFalse: [ ^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: [ nil ].
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector |
		self addTo: senders referencesTo: anOldSelector from: aSelector in: aPotentialClassToRefactor ]! !

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:12:20'!
addTo: senders referencesTo: anOldSelector from: aSelector in: aPotentialClassToRefactor

	senders add: (MethodReference class: aPotentialClassToRefactor selector: aSelector)! !


!InlineMethod class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:17:42'!
addTo: senders referencesTo: aSelectorToInline from: aSelector in: aPotentialClassToRefactor

	| methodReference references |

	methodReference := MethodReference class: aPotentialClassToRefactor selector: aSelector.
	references := self findReferencesToSelector: aSelectorToInline in: methodReference.

	senders addAll: references! !


!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 12:26:24'!
askScope

	| scopeMenu |

	scopeMenu := PopUpMenu labelArray: self scopeOptionLabels.
	scopeChoice := scopeMenu startUpWithCaption: 'Select Refactoring Scope'.
	scopeChoice = 0 ifTrue: [ self endRequest ].
	! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 19:05:37'!
basicCalculateImplementorsAndSenders

  "Do not inline/rename/remove. Used by LiveTyping - Hernan"

	scopeChoice = 1 ifTrue: [ ^self implementorsAndSendersForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndSendersForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndSendersInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndSendersInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndSendersInSystem ].

	self error: 'Unknown scope option'

		! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 19:07:49'!
basicScopeOptionLabels

	"Do not inline/rename/remove. Used by LiveTyping - Hernan"

	^`{'In Class'. 'In Hierarchy'. 'In Root Class Category Tree'. 'In Hierarchy and its Categories'. 'In System'}`! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 19:06:10'!
calculateImplementorsAndSenders

	"Do not inline this message send, #basicCalculateImplementorsAndSenders
	is used in live typing - Hernan"
	self basicCalculateImplementorsAndSenders
		! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:32:27'!
collectionToAddSendersTo

	self subclassResponsibility! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:32:45'!
collectionToAddimplementorsTo

	self subclassResponsibility! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:14:49'!
implementorsAndSendersForClass

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		forClassAndMetaOf: selectedClass! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:15:05'!
implementorsAndSendersForHierarchy

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inHierarchyOf: selectedClass! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:15:14'!
implementorsAndSendersInCategory

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inCategory: selectedClass category
		organizedBy: SystemOrganization! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:15:23'!
implementorsAndSendersInCategoryAndHierarchy

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inCategoriesAndHierarchyOf: selectedClass
		organizedBy: SystemOrganization ! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:15:33'!
implementorsAndSendersInSystem

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inSystem: Smalltalk ! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 19:08:09'!
scopeOptionLabels

	"Do not inline #basicScopeOptionLabels it is used by
	live typing - Hernan"
	^self basicScopeOptionLabels ! !

!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:09:01'!
selectorToLookImplementorsFor

	self subclassResponsibility ! !

!ScopedRefactoringApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/19/2025 14:01:23'!
refactoringClass

	self subclassResponsibility! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:32:27'!
collectionToAddSendersTo

	^senders! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:32:45'!
collectionToAddimplementorsTo

	^implementors ! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:36:43'!
createImplementors

	^IdentitySet new.! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 18:16:56'!
selectorToLookImplementorsFor

	^oldSelector ! !


!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:34:26'!
collectionToAddSendersTo

	"This is implemented like this so we don't have to duplicate every implementorsAndUsagesInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForMessageSendsToInline ifTrue: [messageSends] ifFalse: [^Set new].! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:12:05'!
collectionToAddimplementorsTo

	"This is implemented like this so we don't have to duplicate every implementorsAndMessageSendsInXXX method to
	only calculate implementors or only usages. We always calculate both but if they are not needed they end
	up in this collection that will not be used in the refactoring process - Fernando"

	^self shouldAskForImplementors ifTrue: [implementors] ifFalse: [^Set new]! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:36:43'!
createImplementors

	^IdentitySet new.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 13:09:51'!
selectorToLookImplementorsFor

	^selectorToInline ! !

!InlineMethodApplier methodsFor: 'evaluation' stamp: 'HAW 3/19/2025 18:08:43'!
isOnMessageSend

	^triggeringMessageSend isNil not.! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:18:20'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		forClassAndMetaOf: aPotentialClassToRefactor! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:18:35'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inCategoriesAndHierarchyOf: aClass
		organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:18:49'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inCategory: aCategory
		organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:19:01'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: aClass
! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:19:13'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inSystem: aSystem
! !


!InlineMethod class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 08:59:35'!
convertToSender: aSenderSelector of: aPotentialClassToRefactor referencing: aSelector

	^self
		findReferencesToSelector: aSelector
		in: (MethodReference class: aPotentialClassToRefactor selector: aSenderSelector)! !

!InlineMethod class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:02:04'!
findReferencesToSelector: aSelectorToInline in: aMethodReference

	| references |

	references := Set new.
	aMethodReference notOptimizedMethodNode completeSourceRangesDo: [:aNode :ranges |
		(aNode isMessageNode and: [aNode selector key = aSelectorToInline]) ifTrue: [
			ranges do: [:range  | | referenceToAdd |
				referenceToAdd := MessageNodeReference
					messageNode: aNode
					selector: aMethodReference methodSymbol
					class: aMethodReference methodClass
					completeSourceRange: range.
					references add: referenceToAdd]]].

	^references.

	! !


!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 12:32:21'!
askForImplementorsAndMessageSends

	self
		initializeImplementorsAndMessageSends;
		calculateImplementorsAndSenders;
		openImplementorSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 12:32:21'!
askForImplementorsOnly

	self
		initializeImplementors;
		calculateImplementorsAndSenders;
		openImplementorSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 12:32:21'!
askForMessagesToInlineOnly

	self
		initializeMessageSends;
		calculateImplementorsAndSenders;
		openMessageSendsSelectionWindow.! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 15:34:26'!
initializeMessageSends

	messageSends := self createMessageSends
	! !

!InlineMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/19/2025 18:08:43'!
requestRefactoringParameters

	self isOnMessageSend ifTrue: [self askIfOnlyTriggeringMessageSendShouldBeInlined].
	self askIfImplementorShouldBeRemoved.! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'HAW 3/19/2025 15:34:26'!
messageSendAt: anIndex ifAbsent: ifAbsentBlock

	^messageSends at: anIndex ifAbsent: ifAbsentBlock! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'HAW 3/19/2025 15:34:26'!
messageSends: aCollectionOfMessageSends

	messageSends := aCollectionOfMessageSends reject: [:aMessageNodeReference | aMessageNodeReference isMessageListTitle ]! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'HAW 3/19/2025 15:34:26'!
messageSendsToInline

	^messageSends ! !

!InlineMethodApplier methodsFor: 'accessing' stamp: 'HAW 3/19/2025 15:34:26'!
methodsOfUsagesToInline

	^messageSends collect: [:aMessageNodeReference | aMessageNodeReference methodReference]! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/19/2025 15:34:26'!
createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor

	implementors := Array with: anImplementor.
	messageSends := #().
	shouldShowChanges := false.

	self createAndApplyRefactoring ! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/19/2025 15:34:26'!
createRefactoring

	self assert: implementors size = 1.
	^self refactoringClass
		from: (implementors anyOne)
		intoSendersAndUsages: messageSends
		removeMethod: shouldRemoveImplementor

			! !

!InlineMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/19/2025 15:34:26'!
removeMessageSendsBelongingToImplementor

	messageSends := messageSends reject: [:messageNodeReference |
		messageNodeReference compiledMethod equivalentTo: implementors anyOne
	]
! !

!InlineMethodApplier methodsFor: 'evaluation' stamp: 'HAW 3/19/2025 15:34:26'!
value

	requestExitBlock := [ ^self ].

	self requestRefactoringParametersHandlingRefactoringExceptions.

	self
		ifHasNoUsagesAndOneImplementor: [ :anImplementor |
			shouldRemoveImplementor ifFalse: [self askIfImplementorShouldBeRemovedWhenNoSenders ]. "Give the user another chance to delete the message"
			shouldRemoveImplementor ifFalse: [^self].
			self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor
		]
		ifNot: [
			self askScope.
			shouldInlineTriggeringMessageSendOnly ifTrue: [
				messageSends := OrderedCollection new.
				messageSends add: triggeringMessageSend.
				^self askForImplementorsOnly
			].
			self shouldAskForImplementors ifFalse: [implementors := IdentitySet with: (selectedClass >> selectorToInline). ^self askForMessagesToInlineOnly].
			self askForImplementorsAndMessageSends.
	]! !

!methodRemoval: InlineMethodApplier #actualImplementorsAndSendersWhenNotOnMessageSend stamp: 'HAW 3/19/2025 18:17:48'!
InlineMethodApplier removeSelector: #actualImplementorsAndSendersWhenNotOnMessageSend!
!methodRemoval: InlineMethodApplier #implementorsAndSendersInCategory stamp: 'HAW 3/19/2025 13:15:14'!
InlineMethodApplier removeSelector: #implementorsAndSendersInCategory!
!methodRemoval: InlineMethodApplier #implementorsAndMessageSendsInCategoryAndHierarchy stamp: 'HAW 3/19/2025 12:30:57'!
InlineMethodApplier removeSelector: #implementorsAndMessageSendsInCategoryAndHierarchy!
!methodRemoval: InlineMethodApplier #implementorsAndMessageSendsForHierarchy stamp: 'HAW 3/19/2025 12:29:54'!
InlineMethodApplier removeSelector: #implementorsAndMessageSendsForHierarchy!
!methodRemoval: InlineMethodApplier #implementorsAndSendersInCategoryAndHierarchy stamp: 'HAW 3/19/2025 13:15:23'!
InlineMethodApplier removeSelector: #implementorsAndSendersInCategoryAndHierarchy!
!methodRemoval: InlineMethodApplier #canNotRefactorWithoutTypeInfo stamp: 'HAW 3/19/2025 18:27:18'!
InlineMethodApplier removeSelector: #canNotRefactorWithoutTypeInfo!
!methodRemoval: InlineMethodApplier #implementorsAndSendersForClass stamp: 'HAW 3/19/2025 13:14:49'!
InlineMethodApplier removeSelector: #implementorsAndSendersForClass!
!methodRemoval: InlineMethodApplier #isActualScopeChoice stamp: 'HAW 3/19/2025 18:28:35'!
InlineMethodApplier removeSelector: #isActualScopeChoice!
!methodRemoval: InlineMethodApplier #actualImplementorsAndSendersWhenOnMessageSend stamp: 'HAW 3/19/2025 18:25:36'!
InlineMethodApplier removeSelector: #actualImplementorsAndSendersWhenOnMessageSend!
!methodRemoval: InlineMethodApplier #refactoringRequestedFromUsage stamp: 'HAW 3/19/2025 18:08:43'!
InlineMethodApplier removeSelector: #refactoringRequestedFromUsage!
!methodRemoval: InlineMethodApplier #isActualAndPossibleScope stamp: 'HAW 3/19/2025 18:32:57'!
InlineMethodApplier removeSelector: #isActualAndPossibleScope!
!methodRemoval: InlineMethodApplier #messageSendsOrDiscardCollection stamp: 'HAW 3/19/2025 13:12:29'!
InlineMethodApplier removeSelector: #messageSendsOrDiscardCollection!
!methodRemoval: InlineMethodApplier #implementorsAndSendersInSystem stamp: 'HAW 3/19/2025 13:15:33'!
InlineMethodApplier removeSelector: #implementorsAndSendersInSystem!
!methodRemoval: InlineMethodApplier #scopeOptionLabels stamp: 'HAW 3/19/2025 12:26:53'!
InlineMethodApplier removeSelector: #scopeOptionLabels!
!methodRemoval: InlineMethodApplier #calculateImplementorsAndSenders stamp: 'HAW 3/19/2025 12:32:39'!
InlineMethodApplier removeSelector: #calculateImplementorsAndSenders!
!methodRemoval: InlineMethodApplier #implementorsOrDiscardCollection stamp: 'HAW 3/19/2025 13:12:06'!
InlineMethodApplier removeSelector: #implementorsOrDiscardCollection!
!methodRemoval: InlineMethodApplier #implementorsAndMessageSendsForClass stamp: 'HAW 3/19/2025 12:29:28'!
InlineMethodApplier removeSelector: #implementorsAndMessageSendsForClass!
!methodRemoval: InlineMethodApplier #implementorsAndMessageSendsInSystem stamp: 'HAW 3/19/2025 12:31:23'!
InlineMethodApplier removeSelector: #implementorsAndMessageSendsInSystem!
!methodRemoval: InlineMethodApplier #implementorsAndMessageSendsInCategory stamp: 'HAW 3/19/2025 12:30:32'!
InlineMethodApplier removeSelector: #implementorsAndMessageSendsInCategory!
!methodRemoval: InlineMethodApplier #actualScopeOptionLabels stamp: 'HAW 3/19/2025 18:26:14'!
InlineMethodApplier removeSelector: #actualScopeOptionLabels!
!methodRemoval: InlineMethodApplier #actualImplementorsAndSenders stamp: 'HAW 3/19/2025 18:11:11'!
InlineMethodApplier removeSelector: #actualImplementorsAndSenders!
!methodRemoval: InlineMethodApplier #canNotRefactorWithIncompleteTypeInfo stamp: 'HAW 3/19/2025 18:26:55'!
InlineMethodApplier removeSelector: #canNotRefactorWithIncompleteTypeInfo!
!methodRemoval: InlineMethodApplier #implementorsAndSendersForHierarchy stamp: 'HAW 3/19/2025 13:15:06'!
InlineMethodApplier removeSelector: #implementorsAndSendersForHierarchy!
!methodRemoval: InlineMethodApplier #calculateImplementorsAndMessageSends stamp: 'HAW 3/19/2025 12:32:21'!
InlineMethodApplier removeSelector: #calculateImplementorsAndMessageSends!
!methodRemoval: InlineMethodApplier #askScope stamp: 'HAW 3/19/2025 12:26:24'!
InlineMethodApplier removeSelector: #askScope!
!methodRemoval: AddParameterApplier #withLiveTypingScopeOptionLabels stamp: 'HAW 3/19/2025 18:41:19'!
AddParameterApplier removeSelector: #withLiveTypingScopeOptionLabels!
!methodRemoval: ChangeSelectorApplier #withLiveTypingScopeOptionLabels stamp: 'HAW 3/19/2025 18:41:27'!
ChangeSelectorApplier removeSelector: #withLiveTypingScopeOptionLabels!
!methodRemoval: ChangeSelectorApplier #implementorsAndSendersInSystem stamp: 'HAW 3/19/2025 13:15:33'!
ChangeSelectorApplier removeSelector: #implementorsAndSendersInSystem!
!methodRemoval: ChangeSelectorApplier #scopeOptionLabels stamp: 'HAW 3/19/2025 12:26:53'!
ChangeSelectorApplier removeSelector: #scopeOptionLabels!
!methodRemoval: ChangeSelectorApplier #calculateImplementorsAndSenders stamp: 'HAW 3/19/2025 12:32:39'!
ChangeSelectorApplier removeSelector: #calculateImplementorsAndSenders!
!methodRemoval: ChangeSelectorApplier #implementorsAndSendersInCategory stamp: 'HAW 3/19/2025 13:15:14'!
ChangeSelectorApplier removeSelector: #implementorsAndSendersInCategory!
!methodRemoval: ChangeSelectorApplier #implementorsAndSendersForHierarchy stamp: 'HAW 3/19/2025 13:15:06'!
ChangeSelectorApplier removeSelector: #implementorsAndSendersForHierarchy!
!methodRemoval: ChangeSelectorApplier #implementorsAndSendersInCategoryAndHierarchy stamp: 'HAW 3/19/2025 13:15:23'!
ChangeSelectorApplier removeSelector: #implementorsAndSendersInCategoryAndHierarchy!
!methodRemoval: ChangeSelectorApplier #implementorsAndSendersForClass stamp: 'HAW 3/19/2025 13:14:49'!
ChangeSelectorApplier removeSelector: #implementorsAndSendersForClass!
!methodRemoval: ChangeSelectorApplier #askScope stamp: 'HAW 3/19/2025 12:26:24'!
ChangeSelectorApplier removeSelector: #askScope!
!methodRemoval: ScopedRefactoringApplier class #scopeOptionLabels stamp: 'HAW 3/19/2025 19:08:47'!
ScopedRefactoringApplier class removeSelector: #scopeOptionLabels!
!methodRemoval: ScopedRefactoringApplier #createImplementors stamp: 'HAW 3/19/2025 15:36:57'!
ScopedRefactoringApplier removeSelector: #createImplementors!
!methodMoveToSomePackage: ScopedRefactoringApplier #withLiveTypingScopeOptionLabels stamp: 'HAW 3/19/2025 18:41:36'!
ScopedRefactoringApplier removeSelectorIfInBaseSystem: #withLiveTypingScopeOptionLabels!
!methodMoveToSomePackage: ScopedRefactoringApplier #actualImplementorsAndSenders stamp: 'HAW 3/19/2025 18:11:38'!
ScopedRefactoringApplier removeSelectorIfInBaseSystem: #actualImplementorsAndSenders!
!methodMoveToSomePackage: ScopedRefactoringApplier #calculateImplementorsAndSendersWithLiveTyping stamp: 'HAW 3/19/2025 19:02:49'!
ScopedRefactoringApplier removeSelectorIfInBaseSystem: #calculateImplementorsAndSendersWithLiveTyping!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:of: stamp: 'HAW 3/19/2025 09:14:11'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:of:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:inHierarchyOf: stamp: 'HAW 3/19/2025 08:57:59'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:inHierarchyOf:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:inHierarchyOf:doingPerClass: stamp: 'HAW 3/19/2025 08:58:31'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:inHierarchyOf:doingPerClass:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:inCategory:organizedBy: stamp: 'HAW 3/19/2025 09:16:13'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:inCategory:organizedBy:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:inSystem: stamp: 'HAW 3/19/2025 08:58:49'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:inSystem:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:forClassAndMetaOf: stamp: 'HAW 3/19/2025 08:56:17'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:forClassAndMetaOf:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: stamp: 'HAW 3/19/2025 09:15:41'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:of: stamp: 'HAW 3/19/2025 08:59:15'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:of:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:inCategoriesAndHierarchyOf:organizedBy: stamp: 'HAW 3/19/2025 08:57:13'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:inCategoriesAndHierarchyOf:organizedBy:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: stamp: 'HAW 3/19/2025 09:15:52'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:inHierarchyOf: stamp: 'HAW 3/19/2025 09:16:24'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:inHierarchyOf:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:forClassAndMetaOf: stamp: 'HAW 3/19/2025 09:05:52'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:forClassAndMetaOf:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andUsagesTo:inCategory:organizedBy: stamp: 'HAW 3/19/2025 08:57:43'!
InlineMethod class removeSelector: #addImplementorsOf:to:andUsagesTo:inCategory:organizedBy:!
!methodRemoval: InlineMethod class #addImplementorsOf:to:andSendersTo:inSystem: stamp: 'HAW 3/19/2025 09:16:33'!
InlineMethod class removeSelector: #addImplementorsOf:to:andSendersTo:inSystem:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: stamp: 'HAW 3/19/2025 09:15:41'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:inSystem: stamp: 'HAW 3/19/2025 09:16:33'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:inSystem:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: stamp: 'HAW 3/19/2025 09:15:52'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:inHierarchyOf: stamp: 'HAW 3/19/2025 09:16:24'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:inHierarchyOf:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:forClassAndMetaOf: stamp: 'HAW 3/19/2025 09:05:52'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:forClassAndMetaOf:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:of: stamp: 'HAW 3/19/2025 09:05:52'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:of:!
!methodRemoval: ChangeSelector class #addImplementorsOf:to:andSendersTo:inCategory:organizedBy: stamp: 'HAW 3/19/2025 09:16:13'!
ChangeSelector class removeSelector: #addImplementorsOf:to:andSendersTo:inCategory:organizedBy:!

!ScopedRefactoring class reorganize!
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of: addTo:referencesTo:from:in:)
!

!classDefinition: #ChangeSelector category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #ChangeSelector
	instanceVariableNames: 'oldSelector newSelector implementors senders changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ChangeSelector class reorganize!
('pre-conditions' assertAllImplementors:haveSame: assertAllSenders:send: assertIsNotEmpty:signalMessageText:)
('exceptions' errorMessageForInvalidImplementors: errorMessageForInvalidSenders:of: signalInvalidImplementors: signalInvalidSenders:of:)
!

!classDefinition: #ExtractAsParameter category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #ExtractAsParameter
	instanceVariableNames: 'sourceMethod addParameter intervals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #InlineMethod
	instanceVariableNames: 'methodToInline messageSendsToInline updatedSendersCode methodNodeToInline replacementsByMessageSend temporariesDeclarationsByNode temporariesToDeclareByInsertionPoint implementorCompleteSourceRanges removeMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!InlineMethod class reorganize!
('instance creation' from:intoSendersAndUsages:removeMethod:)
('preconditions' assertCanInlineMethod:intoMessageSends: assertCanInlineWithReferencesToSelf:from: assertCanInlineWithReferencesToSuper:from: assertMethodToInlineHasOnlyOneReturn: assertNoCascadingMessages: assertSenders:belongToClassOf:ifFalse: assertSenders:haveAccessToPrivateVariablesOf:methodNode: methodToInlineReferencesSelfErrorMessage sender:belongsToClassOf: signalMethodToInlineReferencesSelf)
('errors' messageSendCanNotBeCascadeErrorMessage methodAccessPrivateVariablesNotVisibleToSenderErrorMessage methodHasMultipleReturnsErrorMessage methodToInlineReferencesSuperErrorMessage signalMessageSendCanNotBeCascade signalMethodAccessPrivateVariablesNotVisibleToSenderErrorMessage signalMethodHasMultipleReturnsErrorMessage signalMethodToInlineReferencesSuper)
('implementors and senders' addTo:referencesTo:from:in: convertToSender:of:referencing: findReferencesToSelector:in:)
!


!ScopedRefactoringApplier reorganize!
('refactoring - parameters request' askScope basicCalculateImplementorsAndSenders basicScopeOptionLabels calculateImplementorsAndSenders collectionToAddSendersTo collectionToAddimplementorsTo implementorsAndSendersForClass implementorsAndSendersForHierarchy implementorsAndSendersInCategory implementorsAndSendersInCategoryAndHierarchy implementorsAndSendersInSystem scopeOptionLabels selectorToLookImplementorsFor)
('refactoring - creation' refactoringClass)
!


!ScopedRefactoringApplier class reorganize!
!

!classDefinition: #ChangeSelectorApplier category: #'Tools-Refactoring'!
ScopedRefactoringApplier subclass: #ChangeSelectorApplier
	instanceVariableNames: 'oldSelector wizardStepWindow shouldShowChanges browser implementors senders'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ChangeSelectorApplier reorganize!
('refactoring - parameters request' askForImplementosAndSenders collectionToAddSendersTo collectionToAddimplementorsTo createImplementors createSenders implementorsIsOnlyOneTestMethod initializeImplementorsAndSenders openChangeSelectorSendersStepWindow selectorToLookImplementorsFor startWizard)
('accessing' implementors implementors: oldSelector senders senders: wizardStepWindow:)
('refactoring - changes' closeBrowser doNotShowChanges informChangesToBrowser messageSetWindowClass showChanges showChangesInMessageSetWindow)
('refactoring - creation' convertSendersToCompiledMethods createAndApplyRefactoring createAndApplyRefactoringWhenNoSendersAndOneImplementor: refactoringClass)
('initialization' initializeOn:for:in:)
('evaluation' ifHasNoSendersAndOneImplementor:ifNot: value wizardEnded)
!

!classDefinition: #InlineMethodApplier category: #'Tools-Refactoring'!
ScopedRefactoringApplier subclass: #InlineMethodApplier
	instanceVariableNames: 'selectorToInline wizardStepWindow shouldShowChanges browser shouldRemoveImplementor triggeringMessageSend shouldInlineTriggeringMessageSendOnly implementors messageSends'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!InlineMethodApplier reorganize!
('refactoring - parameters request' askForImplementorsAndMessageSends askForImplementorsOnly askForMessagesToInlineOnly askIfImplementorShouldBeRemoved askIfImplementorShouldBeRemoved: askIfImplementorShouldBeRemovedWhenNoSenders askIfOnlyTriggeringMessageSendShouldBeInlined collectionToAddSendersTo collectionToAddimplementorsTo createImplementors createMessageSends initializeImplementors initializeImplementorsAndMessageSends initializeMessageSends openImplementorSelectionWindow openMessageSendsSelectionWindow requestRefactoringParameters selectorToLookImplementorsFor)
('accessing' implementors implementors: messageSendAt:ifAbsent: messageSends: messageSendsToInline methodsOfUsagesToInline selectorToInline shouldAskForImplementors shouldAskForMessageSendsToInline wizardStepWindow:)
('refactoring - changes' closeBrowser doNotShowChanges messageSetWindowClass showChanges showChangesInMessageSetWindow)
('refactoring - creation' createAndApplyRefactoring createAndApplyRefactoringWhenNoSendersAndOneImplementor: createRefactoring refactoringClass removeMessageSendsBelongingToImplementor)
('initialization' initializeOn:for:in: initializeOn:forMessageSend:)
('evaluation' ifHasNoUsagesAndOneImplementor:ifNot: isOnMessageSend value wizardEnded)
!
