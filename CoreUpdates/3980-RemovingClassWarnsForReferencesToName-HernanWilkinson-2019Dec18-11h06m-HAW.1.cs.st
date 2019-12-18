'From Cuis 5.0 [latest update: #3979] on 18 December 2019 at 4:07:57 pm'!
!classDefinition: #ReferencesRefactoringWarning category: #'Tools-Refactoring'!
RefactoringWarning subclass: #ReferencesRefactoringWarning
	instanceVariableNames: 'references primaryReferencee allreferenced'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ClassReferencesCollector category: #'Tools-Refactoring'!
Object subclass: #ClassReferencesCollector
	instanceVariableNames: 'classToLookForReferences referencesToClass referencedAsClass referencesToName referencedAsName withAllSubclassesNames'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 12/18/2019 12:01:17'!
hasVariableBindingTo: aClass 
	
	self literalsDo: [ :aLiteral |
		(aLiteral isVariableBinding and: [ aLiteral value = aClass ]) ifTrue: [ ^true ]].
	
	^false! !


!CanNotRefactorDueToReferencesError methodsFor: 'initialization' stamp: 'HAW 12/18/2019 12:23:57'!
initializeWith: aMessageText references: aCollectionOfReferences to: aReferencee

	self messageText: aMessageText.
	references := aCollectionOfReferences.
	referencee := aReferencee ! !


!CanNotRefactorDueToReferencesError class methodsFor: 'instance creation' stamp: 'HAW 12/18/2019 12:23:36'!
with: aMessageText references: references to: aReferencee

	^self new initializeWith: aMessageText references: references to: aReferencee! !


!ReferencesRefactoringWarning methodsFor: 'initialization' stamp: 'HAW 12/18/2019 12:26:09'!
initializeWith: aMessageText references: aReferences of: aPrimaryReferencee toAll: anAllReferenced

	self messageText: aMessageText.
	references := aReferences.
	primaryReferencee := aPrimaryReferencee.
	allreferenced := anAllReferenced ! !

!ReferencesRefactoringWarning methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:14:58'!
referencee
	
	^primaryReferencee ! !

!ReferencesRefactoringWarning methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:14:48'!
references
	
	^references! !

!ReferencesRefactoringWarning methodsFor: 'references' stamp: 'HAW 12/18/2019 16:04:55'!
anyReference
	
	^references anyOne ! !

!ReferencesRefactoringWarning methodsFor: 'references' stamp: 'HAW 12/18/2019 16:04:39'!
numberOfReferences
	
	^references size! !


!ReferencesRefactoringWarning class methodsFor: 'as yet unclassified' stamp: 'HAW 12/18/2019 12:24:17'!
signal: aMessageText references: references of: primaryReferencee toAll: allReferenced 

	(self with: aMessageText references: references of: primaryReferencee toAll: allReferenced) signal! !

!ReferencesRefactoringWarning class methodsFor: 'instance creation' stamp: 'HAW 12/18/2019 12:24:55'!
with: aMessageText references: references of: primaryReferencee toAll: allReferenced

	^self new initializeWith: aMessageText references: references of: primaryReferencee toAll: allReferenced! !


!MethodReference methodsFor: 'testing' stamp: 'HAW 12/18/2019 11:59:43'!
hasVariableBindingTo: aClass 
	
	^self compiledMethod hasVariableBindingTo: aClass 
! !


!ClassReferencesCollector methodsFor: 'initialization' stamp: 'HAW 12/18/2019 15:50:25'!
initializeOf: aClassToLookForReferences

	classToLookForReferences := aClassToLookForReferences ! !

!ClassReferencesCollector methodsFor: 'evaluating - private' stamp: 'HAW 12/18/2019 15:55:55'!
add: aClass asReferencedToClassWith: referencesToVariableBinding
	
	referencedAsClass add: aClass.
	referencesToClass addAll: referencesToVariableBinding ! !

!ClassReferencesCollector methodsFor: 'evaluating - private' stamp: 'HAW 12/18/2019 15:55:44'!
add: aClass asReferencedToNameWith: referencesToClassName

	referencedAsName add: aClass.
	referencesToName addAll: referencesToClassName 
	! !

!ClassReferencesCollector methodsFor: 'evaluating - private' stamp: 'HAW 12/18/2019 15:55:55'!
collectReferencesOf: aClass 
	
	| allReferences referencesOutsideHierarchy referencesToVariableBinding referencesToClassName |
	
	allReferences := aClass allCallsOn.
	referencesOutsideHierarchy := allReferences reject: [ :aReference | withAllSubclassesNames includes: aReference classSymbol ].
	referencesToVariableBinding := referencesOutsideHierarchy select: [ :aReference | aReference hasVariableBindingTo: aClass ].
	referencesToClassName := referencesOutsideHierarchy difference: referencesToVariableBinding.
	
	referencesToVariableBinding notEmpty ifTrue: [ self add: aClass asReferencedToClassWith: referencesToVariableBinding ].	
	referencesToClassName notEmpty ifTrue: [ self add: aClass asReferencedToNameWith: referencesToClassName ].
	! !

!ClassReferencesCollector methodsFor: 'evaluating - private' stamp: 'HAW 12/18/2019 15:57:22'!
initializeCollectionFor: withAllSubclasses
	
	withAllSubclassesNames := withAllSubclasses collect: [:aClass | aClass name ].
	referencesToClass := OrderedCollection new.
	referencedAsClass := OrderedCollection new.
	referencesToName := OrderedCollection new.
	referencedAsName := OrderedCollection new! !

!ClassReferencesCollector methodsFor: 'evaluating' stamp: 'HAW 12/18/2019 15:57:12'!
value

	| withAllSubclasses |
	
	withAllSubclasses := classToLookForReferences withAllSubclasses.
	self initializeCollectionFor: withAllSubclasses.
		
	withAllSubclasses do: [ :aClass | self collectReferencesOf: aClass ].
		
	! !

!ClassReferencesCollector methodsFor: 'testing' stamp: 'HAW 12/18/2019 15:58:27'!
hasReferencesToClass
	
	^referencesToClass notEmpty! !

!ClassReferencesCollector methodsFor: 'testing' stamp: 'HAW 12/18/2019 15:58:43'!
hasReferencesToName
	
	^referencesToName notEmpty ! !

!ClassReferencesCollector methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:59:32'!
referencedAsClass
	
	^referencedAsClass! !

!ClassReferencesCollector methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:59:00'!
referencedAsName
	
	^referencedAsName! !

!ClassReferencesCollector methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:59:23'!
referencesToClass
	
	^referencesToClass! !

!ClassReferencesCollector methodsFor: 'accessing' stamp: 'HAW 12/18/2019 15:58:52'!
referencesToName
	
	^referencesToName! !


!ClassReferencesCollector class methodsFor: 'evaluating' stamp: 'HAW 12/18/2019 15:49:34'!
valueOf: aClassToLookForReferences

	^(self of: aClassToLookForReferences) value! !

!ClassReferencesCollector class methodsFor: 'instance creation' stamp: 'HAW 12/18/2019 15:49:47'!
of: aClassToLookForReferences

	^self new initializeOf: aClassToLookForReferences! !


!Refactoring class methodsFor: 'warnings' stamp: 'HAW 12/18/2019 12:21:19'!
referencesWarningClass
	
	^ReferencesRefactoringWarning! !

!Refactoring class methodsFor: 'warnings' stamp: 'HAW 12/18/2019 15:37:30'!
warnAboutReferences: references with: aMessageText of: primaryReferencee toAll: allReferenced
 
	^self referencesWarningClass
		signal: aMessageText 
		references: references 
		of: primaryReferencee 
		toAll: allReferenced ! !


!SafelyRemoveClass class methodsFor: 'warnings' stamp: 'HAW 12/18/2019 15:37:44'!
warnAboutRemoveOf: aClassToSafelyRemove dueToNameReferences: references toAll: allReferenced 
	
	self 
		warnAboutReferences: references
		with: (self warningMessageForReferencesToNames: allReferenced)
		of: aClassToSafelyRemove 
		toAll: allReferenced 
	! !

!SafelyRemoveClass class methodsFor: 'warnings' stamp: 'HAW 12/18/2019 12:08:06'!
warningMessageForReferencesToNames: referenced

	^'There are references to the name of ', referenced asCommaStringAnd ! !


!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 12/18/2019 15:18:30'!
handleReferencesWarning: aReferencesWarning
	
	| options answer question |
	
	options := 
'Browse references and Cancel
Browse references and Continue
Continue'.

	question := PopUpMenu labels: options icons: #(cancelIcon mailForwardIcon acceptIcon).
	answer := question startUpWithCaption: aReferencesWarning messageText.
	
	answer <= 2 ifTrue: [ self browseReferencesOn: aReferencesWarning ].
	answer = 1 ifTrue: [ self endRequest ].
	aReferencesWarning resume.! !


!CanNotRefactorDueToReferencesError class methodsFor: 'signaling' stamp: 'HAW 12/18/2019 12:23:14'!
signal: aMessageText references: references to: aReferencee

	(self with: aMessageText references: references to: aReferencee) signal! !


!SafelyRemoveClass class methodsFor: 'instance creation' stamp: 'HAW 12/18/2019 15:18:38'!
of: aClassToSafelyRemove

	| theNonMetaclassToRemove |
	
	theNonMetaclassToRemove := aClassToSafelyRemove theNonMetaClass.	
	self assertNoReferencesTo: theNonMetaclassToRemove.	
	self warnIfHasSubclasses: theNonMetaclassToRemove.
	
	^self new initializeOf: theNonMetaclassToRemove ! !

!SafelyRemoveClass class methodsFor: 'pre-conditions' stamp: 'HAW 12/18/2019 15:49:06'!
assertNoReferencesTo: aClassToSafelyRemove
	
	| referencesCollector |
	
	referencesCollector := ClassReferencesCollector valueOf: aClassToSafelyRemove.
	
	referencesCollector hasReferencesToClass ifTrue: [ 
		^self 
			signalCanNotRemove: aClassToSafelyRemove 
			dueToReferences: referencesCollector referencesToClass 
			toAll: referencesCollector referencedAsClass ].
	
	referencesCollector hasReferencesToName ifTrue: [
		^self 
			warnAboutRemoveOf: aClassToSafelyRemove 
			dueToNameReferences: referencesCollector referencesToName 
			toAll: referencesCollector referencedAsName ].
! !


!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 12/18/2019 12:28:15'!
valueHandlingRefactoringExceptions: aBlock

	^[[[aBlock
		on: Refactoring referencesWarningClass
		do: [ :aReferencesRefactoringWarning | self handleReferencesWarning: aReferencesRefactoringWarning ]]
		on: Refactoring refactoringWarningClass 
		do: [ :aRefactoringWarning | self handleRefactoringWarning: aRefactoringWarning ]]
		on: Refactoring canNotRefactorDueToReferencesErrorClass
		do: [ :aCanNotRefactorDueToReferencesError | self handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError ]]
		on: Refactoring refactoringErrorClass 
		do: [ :aRefactoringError | self handleRefactoringError: aRefactoringError ]
	! !

!methodRemoval: Refactoring class #warnAboutReferences:references:of:toAll: stamp: 'HAW 12/18/2019 15:37:18'!
Refactoring class removeSelector: #warnAboutReferences:references:of:toAll:!
!methodRemoval: ClassReferencesCollector #add:asReferenceeToClassWith: stamp: 'HAW 12/18/2019 15:55:55'!
ClassReferencesCollector removeSelector: #add:asReferenceeToClassWith:!
!methodRemoval: CanNotRefactorDueToReferencesError #initialize:references:to: stamp: 'HAW 12/18/2019 12:23:59'!
CanNotRefactorDueToReferencesError removeSelector: #initialize:references:to:!

!CanNotRefactorDueToReferencesError class reorganize!
('signaling' signal:references:to:)
('instance creation' with:references:to:)
!


!ClassReferencesCollector reorganize!
('initialization' initializeOf:)
('evaluating - private' add:asReferencedToClassWith: add:asReferencedToNameWith: collectReferencesOf: initializeCollectionFor:)
('evaluating' value)
('testing' hasReferencesToClass hasReferencesToName)
('accessing' referencedAsClass referencedAsName referencesToClass referencesToName)
!


!ClassReferencesCollector class reorganize!
('evaluating' valueOf:)
('instance creation' of:)
!


!SafelyRemoveClass class reorganize!
('instance creation' of:)
('pre-conditions' assertNoReferencesTo: warnIfHasSubclasses:)
('exceptions' errorMessageForCanNotRemove:dueToReferencesToAll: signalCanNotRemove:dueToReferences:toAll:)
('warnings' warn:hasSubclasses: warnAboutRemoveOf:dueToNameReferences:toAll: warningMessageFor:hasSubclasses: warningMessageForReferencesToNames:)
!

