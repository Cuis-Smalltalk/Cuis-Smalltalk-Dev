'From Cuis7.9 [latest update: #8157] on 7 September 2026 at 11:59:37 am'!
!classDefinition: #ExtractMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethod
	instanceVariableNames: 'extractMethodNewMethod collectionOfReplacements newMethod '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpInstanceVariable category: #'Tools-Refactoring'!
TransferInstanceVariable subclass: #PushUpInstanceVariable
	instanceVariableNames: 'changes rootClass '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #TransferMethod category: #'Tools-Refactoring'!
Refactoring subclass: #TransferMethod
	instanceVariableNames: 'method changes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MethodReference methodsFor: 'converting' stamp: 'HAW 27/Aug/2026, 5:23:15 pm (UTC)'!
asMethodReference

	^self ! !


!ExtractMethod methodsFor: 'applying' stamp: 'HAW 7/Sep/2026, 2:14:42 pm (UTC)'!
changes

	| changes |

	changes := Set with: newMethod asMethodReference.
	collectionOfReplacements do: [ :aReplacement |
		changes add: aReplacement methodToExtractFrom asMethodReference ].

	^changes
! !


!PushDownInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:19:47 pm (UTC)'!
changes

	^(OrderedCollection with: classToRefactor)
		addAll: classToRefactor subclasses;
		yourself
		! !

!PushDownInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:17:12 pm (UTC)'!
removeInstanceVariable

	classToRefactor removeInstVarName: instanceVariableToMove! !


!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:44:56 pm (UTC)'!
addInstanceVariableToRootClass

	rootClass addInstVarName: instanceVariableToMove.
	changes add: rootClass ! !

!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:45:08 pm (UTC)'!
removeInstanceVariableFromAllSubclasses

	rootClass allSubclasses
		select: [ :subclass | subclass definesInstanceVariableNamed: instanceVariableToMove ]
		thenDo: [ :subclass |
			subclass removeInstVarName: instanceVariableToMove.
			changes add: subclass ]
	! !

!PushUpInstanceVariable methodsFor: 'initialization' stamp: 'HAW 7/Sep/2026, 2:45:52 pm (UTC)'!
initializeNamed: anInstanceVariableToMove from: aClassToRefactor

	super initializeNamed: anInstanceVariableToMove from: aClassToRefactor.
	
	rootClass := aClassToRefactor superclass.
	changes := OrderedCollection new. 
	
! !


!TransferMethod methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
noteChangeIn: aClass at: aSelector

	changes add: (MethodReference class: aClass selector: aSelector)
! !


!AddInstanceVariable methodsFor: 'applying' stamp: 'HAW 26/Aug/2026, 10:51:13 pm (UTC)'!
apply

	classToRefactor addInstVarName: newVariable.

	^classToRefactor
! !


!ExtractMethod methodsFor: 'private - applying' stamp: 'HAW 7/Sep/2026, 2:09:37 pm (UTC)'!
createNewMethod

	newMethod := extractMethodNewMethod apply

! !

!ExtractMethod methodsFor: 'applying' stamp: 'HAW 7/Sep/2026, 2:09:31 pm (UTC)'!
apply

	^self 
		unifyNewMethodAndReplacements;
		createNewMethod;
		applyReplacements;
		changes! !


!RemoveInstanceVariable methodsFor: 'applying' stamp: 'HAW 26/Aug/2026, 10:51:13 pm (UTC)'!
apply

	classToRefactor removeInstVarName: variableToRemove.

	^classToRefactor
! !


!SafelyRemoveClass methodsFor: 'applying' stamp: 'HAW 7/Sep/2026, 2:50:26 pm (UTC)'!
apply

	| changes |
	
	"I collect the name of the classes that will be removed before doing it - Hernan"
	changes := classToRemove withAllSubclasses collect: [ :aClass | aClass name asString ].
	
	self removeWithAllSubclasses: classToRemove.
	
	^changes
! !

!SafelyRemoveClass methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:50:42 pm (UTC)'!
removeWithAllSubclasses: aClassToRemove

	"I have to do 'subclasses do:' and not 'subclassesDo:' because removing a class modifies parent's subclasses collection.
	#subclasses returns a copy of superclass' subclasses collection -Hernan"
	
	aClassToRemove subclasses do: [ :aSubclassToRemove | self removeWithAllSubclasses: aSubclassToRemove ].
	aClassToRemove removeFromSystem.
	! !


!PushDownInstanceVariable methodsFor: 'applying' stamp: 'HAW 7/Sep/2026, 2:17:05 pm (UTC)'!
apply

	^self 
		removeInstanceVariable;
		pushDownInstanceVariableToAllSubclasses;
		changes. 
	! !

!PushDownInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 26/Dec/2019, 11:23:14 pm (UTC)'!
pushDownInstanceVariableToAllSubclasses

	classToRefactor subclassesDo: [ :subClass |
		subClass addInstVarName: instanceVariableToMove ]! !


!PushUpInstanceVariable methodsFor: 'applying' stamp: 'HAW 7/Sep/2026, 2:45:43 pm (UTC)'!
apply

	self 
		removeInstanceVariableFromAllSubclasses;
		addInstanceVariableToRootClass.

	^changes! !


!TransferMethod methodsFor: 'initialization' stamp: 'HAW 7/Sep/2026, 2:26:09 pm (UTC)'!
initializeFor: aMethodToTransfer

	method := aMethodToTransfer.
	changes := Set new
! !

!TransferMethod methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
implementSubclassResponsibilityIn: aClass

	^self
		noteChangeIn: aClass
		at: (aClass
			compile: self subclassResponsibilitySourceCode
			classified: self methodCategory)
! !

!TransferMethod methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
removeMethod: aMethod

	aMethod methodClass removeSelector: aMethod selector.
	self noteChangeIn: aMethod methodClass at: aMethod selector
! !

!TransferMethod methodsFor: 'applying' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
apply

	self
		moveMethod;
		removeMethod.

	^changes
! !


!MoveToInstanceOrClassMethod methodsFor: 'applying - private' stamp: 'HAW 7/Sep/2026, 2:52:01 pm (UTC)'!
moveMethod

	| targetClass |

	targetClass := method methodClass isMeta
		ifTrue: [method methodClass soleInstance]
		ifFalse: [method methodClass class].

	self
		noteChangeIn: targetClass
		at: (targetClass 
			compile: method sourceCode 
			classified: self methodCategory)
! !


!PushDownMethodToOneSubclass methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
moveMethod

	self
		noteChangeIn: subclassToPushDownTo
		at: (subclassToPushDownTo
			compile: method sourceCode
			classified: self methodCategory)
! !


!PushDownMethodToSubclasses methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
moveMethod

	| methodCategory sourceCode |

	methodCategory := self methodCategory.
	sourceCode := method sourceCode.

	method methodClass subclassesDo: [ :subclass |
		self
			noteChangeIn: subclass
			at: (subclass compile: sourceCode classified: methodCategory) ]
! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
compileMethodInSuperclass: aMethod

	| superclassToPushUpTo |

	superclassToPushUpTo := aMethod methodClass superclass.
	self
		noteChangeIn: superclassToPushUpTo
		at: (superclassToPushUpTo
			compile: aMethod sourceCode
			classified: (self methodCategory: aMethod))
! !

!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 26/Aug/2026, 4:16:56 pm (UTC)'!
removeMethod

	subclassesToRemoveMethodFrom keysAndValuesDo: [ :methodWithSelectorToRemove :classesToRemoveMethodFrom |
		classesToRemoveMethodFrom do: [ :aClassToRemoveMethodFrom | 
			aClassToRemoveMethodFrom removeSelector: methodWithSelectorToRemove selector.
			self noteChangeIn: aClassToRemoveMethodFrom at: methodWithSelectorToRemove selector ]]
! !

!methodRemoval: PushUpInstanceVariable #addInstanceVariableToSuperclass stamp: 'HAW 7/Sep/2026, 2:44:56 pm (UTC)'!
PushUpInstanceVariable removeSelector: #addInstanceVariableToSuperclass!
!methodRemoval: PushUpInstanceVariable #removeSubclassesInstanceVariable stamp: 'HAW 7/Sep/2026, 2:38:00 pm (UTC)'!
PushUpInstanceVariable removeSelector: #removeSubclassesInstanceVariable!
!methodRemoval: SafelyRemoveClass #removeWithAllSubclasses:addingNameTo: stamp: 'HAW 7/Sep/2026, 2:50:44 pm (UTC)'!
SafelyRemoveClass removeSelector: #removeWithAllSubclasses:addingNameTo:!
!methodRemoval: ExtractMethod #changesWith: stamp: 'HAW 7/Sep/2026, 2:09:19 pm (UTC)'!
ExtractMethod removeSelector: #changesWith:!
!methodRemoval: ExtractMethod #changesWith stamp: 'HAW 7/Sep/2026, 2:09:31 pm (UTC)'!
ExtractMethod removeSelector: #changesWith!
!classDefinition: #ExtractMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethod
	instanceVariableNames: 'extractMethodNewMethod collectionOfReplacements newMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethod reorganize!
('initialization' initializeNewDefinition:replacements:)
('private - applying' allReplacementsReturnsADifferentSingleVariable applyMethodReplacements: applyReplacements createNewMethod unifyNewMethodAndReplacements)
('applying' apply changes)
('accessing' replacements)
!


!PushDownInstanceVariable reorganize!
('applying' apply)
('applying - private' changes pushDownInstanceVariableToAllSubclasses removeInstanceVariable)
!

!classDefinition: #PushUpInstanceVariable category: #'Tools-Refactoring'!
TransferInstanceVariable subclass: #PushUpInstanceVariable
	instanceVariableNames: 'changes rootClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpInstanceVariable reorganize!
('applying' apply)
('applying - private' addInstanceVariableToRootClass removeInstanceVariableFromAllSubclasses)
('initialization' initializeNamed:from:)
!

!classDefinition: #TransferMethod category: #'Tools-Refactoring'!
Refactoring subclass: #TransferMethod
	instanceVariableNames: 'method changes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TransferMethod reorganize!
('initialization' initializeFor:)
('applying - private' implementSubclassResponsibilityIn: methodCategory methodCategory: moveMethod noteChangeIn:at: removeMethod removeMethod: subclassResponsibilitySourceCode)
('applying' apply)
('accessing' selector)
!

