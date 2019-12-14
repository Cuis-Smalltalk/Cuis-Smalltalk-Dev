'From Cuis 5.0 [latest update: #3958] on 9 December 2019 at 1:56:57 am'!
!classDefinition: #MoveToInstanceOrClassMethod category: #'Tools-Refactoring'!
Refactoring subclass: #MoveToInstanceOrClassMethod
	instanceVariableNames: 'methodToMove originalClass newClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: 'MoveToInstanceOrClassMethod class' category: #'Tools-Refactoring'!
MoveToInstanceOrClassMethod class
	instanceVariableNames: ''!
!classDefinition: #MoveToInstanceOrClassMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #MoveToInstanceOrClassMethodApplier
	instanceVariableNames: 'browser methodToMove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!BrowserWindow methodsFor: 'refactorings' stamp: 'LMY 12/9/2019 00:34:31'!
moveToInstanceOrClassMethod

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(MoveToInstanceOrClassMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !


!MoveToInstanceOrClassMethod methodsFor: 'initialization' stamp: 'LMY 12/9/2019 00:01:14'!
initializeFor: aMethodToMove 

	methodToMove := aMethodToMove.
	originalClass := aMethodToMove methodClass.
	newClass := aMethodToMove methodClass isMeta
		ifTrue: [aMethodToMove methodClass soleInstance]
		ifFalse: [aMethodToMove methodClass class]! !

!MoveToInstanceOrClassMethod methodsFor: 'applying' stamp: 'LMY 12/9/2019 00:02:43'!
apply

	| methodCategory |

	methodCategory := originalClass organization categoryOfElement: methodToMove selector.
	newClass
		compile: methodToMove sourceCode
		classified: methodCategory.

	originalClass removeSelector: methodToMove selector.
 ! !


!MoveToInstanceOrClassMethod class methodsFor: 'instance creation' stamp: 'LMY 12/8/2019 18:44:15'!
for: aMethodToMove

	self assertIsNotAccessingInstanceVariable: aMethodToMove.
	self assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove.
	
	^self new initializeFor: aMethodToMove ! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'LMY 12/8/2019 18:42:50'!
localVariableConflictsWithInstanceVariableErrorMessage
	
	^ 'Can not move a method that uses a local variable with same name as an instance variable'! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'LMY 12/8/2019 17:58:30'!
referencingInstanceVariablesErrorMessage
	
	^ 'Can not move a method that accesses an instance variable'! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'LMY 12/8/2019 18:42:11'!
signalLocalVariableConflictsWithInstanceVariable
	
	self refactoringError: self localVariableConflictsWithInstanceVariableErrorMessage! !

!MoveToInstanceOrClassMethod class methodsFor: 'exceptions' stamp: 'LMY 12/8/2019 17:58:37'!
signalMethodCannotAccessInstanceVariable
	
	self refactoringError: self referencingInstanceVariablesErrorMessage! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'LMY 12/8/2019 17:56:44'!
assertIsNotAccessingInstanceVariable: aMethodToMove
	
	aMethodToMove methodClass instVarNames 
		do: [ :instVarName | 
			(aMethodToMove accessesInstanceVariable: instVarName) ifTrue: [self signalMethodCannotAccessInstanceVariable]].! !

!MoveToInstanceOrClassMethod class methodsFor: 'pre-conditions' stamp: 'LMY 12/9/2019 00:08:13'!
assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove
	
	| newClass |
	
	aMethodToMove methodClass isMeta
		ifTrue: [newClass := aMethodToMove methodClass soleInstance]
		ifFalse: [newClass := aMethodToMove methodClass class].
		
	newClass instVarNames 
		do: [ :instVarName | 
			(aMethodToMove methodNode hasLocalNamed: instVarName)
				ifTrue: [self signalLocalVariableConflictsWithInstanceVariable]
			]! !


!MoveToInstanceOrClassMethodApplier methodsFor: 'initialization' stamp: 'LMY 12/9/2019 00:26:42'!
initializeOn: aBrowser for: aMethodToMove
	
	browser := aBrowser.
	methodToMove := aMethodToMove.! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - creation' stamp: 'LMY 12/9/2019 00:30:05'!
createRefactoring
		
	^self refactoringClass for: methodToMove.! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - creation' stamp: 'LMY 12/9/2019 00:29:41'!
refactoringClass

	^MoveToInstanceOrClassMethod! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - changes' stamp: 'LMY 12/9/2019 00:31:04'!
informChangesToBrowser

	| classMethod |
	
	classMethod := methodToMove methodClass asString.
	
	browser classListIndex: (browser classListIndexOf: classMethod).! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - changes' stamp: 'LMY 12/9/2019 00:30:19'!
showChanges

	self informChangesToBrowser.! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'LMY 12/9/2019 01:26:59'!
confirmationMessageText
	
	^'This message has senders. Are you sure you want to move it?'! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'LMY 12/9/2019 01:46:26'!
methodHasSenders

	^(methodToMove methodClass whichSelectorsReferTo: methodToMove selector) isEmpty not.! !

!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'LMY 12/9/2019 01:25:16'!
requestRefactoringParameters

	self methodHasSenders ifTrue: [
		(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ]
	]! !


!MoveToInstanceOrClassMethodApplier class methodsFor: 'instance creation' stamp: 'LMY 12/9/2019 00:46:41'!
on: aBrowser for: aMethodToMove
	
	^self new initializeOn: aBrowser for: aMethodToMove ! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'LMY 12/9/2019 01:56:12'!
messsageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down'.
				#selector 		-> 		#pushDownSelector.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		60.
				#label 			-> 		'move to instance/class method'.
				#selector 		-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
	}`.

	! !


!MoveToInstanceOrClassMethod reorganize!
('initialization' initializeFor:)
('applying' apply)
!


!MoveToInstanceOrClassMethod class reorganize!
('instance creation' for:)
('exceptions' localVariableConflictsWithInstanceVariableErrorMessage referencingInstanceVariablesErrorMessage signalLocalVariableConflictsWithInstanceVariable signalMethodCannotAccessInstanceVariable)
('pre-conditions' assertIsNotAccessingInstanceVariable: assertLocalVariableDoesNotConflictWithInstanceVariable:)
!


!MoveToInstanceOrClassMethodApplier reorganize!
('initialization' initializeOn:for:)
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - changes' informChangesToBrowser showChanges)
('refactoring - parameters request' confirmationMessageText methodHasSenders requestRefactoringParameters)
!


!MoveToInstanceOrClassMethodApplier class reorganize!
('instance creation' on:for:)
!

