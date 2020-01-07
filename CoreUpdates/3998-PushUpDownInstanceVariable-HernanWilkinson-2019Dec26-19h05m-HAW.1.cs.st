'From Cuis 5.0 [latest update: #3997] on 26 December 2019 at 10:03:20 pm'!
!classDefinition: #MoveInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #MoveInstanceVariable
	instanceVariableNames: 'classToRefactor instanceVariableToMove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushDownInstanceVariable category: #'Tools-Refactoring'!
MoveInstanceVariable subclass: #PushDownInstanceVariable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpInstanceVariable category: #'Tools-Refactoring'!
MoveInstanceVariable subclass: #PushUpInstanceVariable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #MoveInstanceVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #MoveInstanceVariableApplier
	instanceVariableNames: 'browser classToRefactor instanceVariableName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushDownInstanceVariableApplier category: #'Tools-Refactoring'!
MoveInstanceVariableApplier subclass: #PushDownInstanceVariableApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpInstanceVariableApplier category: #'Tools-Refactoring'!
MoveInstanceVariableApplier subclass: #PushUpInstanceVariableApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CodeProvider methodsFor: 'contents' stamp: 'MSC 12/19/2019 07:20:34'!
instanceVariablePushedUp

	self acceptedContentsChanged
! !


!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:18'!
apply: aBlock inClassDefinitionOf: aClassDefinitionNode in: aSelectedClass 
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ aBlock value: aSelectedClass ]
		ifFalse: [ morph flash ]
	
! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:20'!
contextualPushDownInClassDefinition 

	self inClassDefinitionContextuallyApply: [ :aSelectedClass | 
		(PushDownInstanceVariableApplier 
			on: self codeProvider 
			for: self wordUnderCursor 
			at: aSelectedClass ) value ]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:25'!
contextualPushUpInClassDefinition 

	self inClassDefinitionContextuallyApply: [ :aSelectedClass | 
		(PushUpInstanceVariableApplier 
			on: self codeProvider 
			for: self wordUnderCursor 
			at: aSelectedClass) value ]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:28'!
ifEditingClassDefinitionDoOrWarn: aBlock

	self isEditingClassDefinition 
		ifTrue: aBlock
		ifFalse: [ self informRefactoringCanOnlyBeAppliedInClassDefinition ]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:32'!
inClassDefinitionContextuallyApply: aBlock

	self ifEditingClassDefinitionDoOrWarn: [
		self ifSourceCodeRefactoringCanBeAppliedDo: [
			self
				withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
					self apply: aBlock inClassDefinitionOf: classDefinitionNode in: selectedClass ]
				ifErrorsParsing: [ :anError | morph flash ]]]! !

!SmalltalkEditor methodsFor: 'contextual push up/down inst. var' stamp: 'HAW 12/26/2019 19:37:35'!
informRefactoringCanOnlyBeAppliedInClassDefinition

	self inform: 'This refactoring can only be applied from the class definition'! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'MSC 12/21/2019 10:54:17'!
pushDownInstanceVariable

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushDownInstanceVariableApplier on: model at: aClass ) value].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'MSC 12/19/2019 07:09:01'!
pushUpInstanceVariable

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushUpInstanceVariableApplier on: model at: aClass ) value].! !


!ChangeListElement methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:06:18'!
fileIn

	self subclassResponsibility ! !

!ChangeListElement methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:07:08'!
fileOutOn: aFileStream

	self subclassResponsibility ! !


!FeatureChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:05:33'!
fileIn
	
	"It does nothing - Hernan"! !

!FeatureChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:07:28'!
fileOutOn: aFileStream

	"Nothing to file out - Hernan"! !


!MoveInstanceVariable methodsFor: 'initialization' stamp: 'HAW 12/26/2019 20:23:54'!
initializeNamed: anInstanceVariableToMove from: aClassToRefactor

	instanceVariableToMove := anInstanceVariableToMove.
	classToRefactor := aClassToRefactor.! !


!MoveInstanceVariable class methodsFor: 'instance creation' stamp: 'HAW 12/26/2019 20:36:55'!
named: anInstanceVariable from: aClassToRefactor
	
	self assertCanMove: anInstanceVariable from: aClassToRefactor.
	
	^self new initializeNamed: anInstanceVariable from: aClassToRefactor! !

!MoveInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 20:36:58'!
assert: aClassToRefactor hasInstanceVariable: anInstanceVariable

	(aClassToRefactor definesInstanceVariableNamed: anInstanceVariable) ifFalse: [self refactoringError: self instanceVariableDoesNotExistOnClassToRefactor].
	
	! !

!MoveInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 20:37:02'!
assertCanMove: anInstanceVariable from: aClassToRefactor

	self subclassResponsibility ! !

!MoveInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 12/26/2019 20:37:05'!
instanceVariableDoesNotExistOnClassToRefactor
	
	^ 'Instance variable does not exist on class to refactor'! !


!PushDownInstanceVariable methodsFor: 'applying' stamp: 'HAW 12/26/2019 20:23:14'!
apply

	classToRefactor removeInstVarName: instanceVariableToMove.
	self pushDownInstanceVariableToAllSubclasses! !

!PushDownInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 12/26/2019 20:23:14'!
pushDownInstanceVariableToAllSubclasses

	classToRefactor subclassesDo: [ :subClass |
		subClass addInstVarName: instanceVariableToMove ]! !


!PushDownInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 20:37:10'!
assert: aClassToRefactor isNotAccessingInstanceVariable: anInstanceVariable

	| selectorsReferencingInstVar |
	
	selectorsReferencingInstVar := aClassToRefactor whichSelectorsAccess: anInstanceVariable.
	selectorsReferencingInstVar ifNotEmpty: [
		self 
			canNotRefactorDueToReferencesError: (
				self errorMessageForInstanceVariable: anInstanceVariable isAccessedInMethodsOf: aClassToRefactor)
			references: (selectorsReferencingInstVar collect: [ :selector | 
				MethodReference class: aClassToRefactor selector: selector ]) asArray
			to: anInstanceVariable ]! !

!PushDownInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 20:37:13'!
assertCanMove: anInstanceVariable from: aClassToRefactor

	self assert: aClassToRefactor hasInstanceVariable: anInstanceVariable.
	self assert: aClassToRefactor isNotAccessingInstanceVariable: anInstanceVariable.
	
	! !

!PushDownInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 12/26/2019 20:37:16'!
errorMessageForInstanceVariable: anInstanceVariable isAccessedInMethodsOf: aClassToRefactor 
	
	^ 'Cannot push down ', anInstanceVariable, ' because it is accessed in methods of ', aClassToRefactor name! !


!PushUpInstanceVariable methodsFor: 'applying' stamp: 'HAW 12/26/2019 20:36:17'!
apply

	self removeSubclassesInstanceVariable.
	classToRefactor superclass addInstVarName: instanceVariableToMove.! !

!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 12/26/2019 20:36:02'!
removeSubclassesInstanceVariable

	classToRefactor superclass subclassesDo: [ :subclass |
		(subclass definesInstanceVariableNamed: instanceVariableToMove) ifTrue: [ subclass removeInstVarName: instanceVariableToMove].
	].! !


!PushUpInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 20:37:25'!
assertCanMove: anInstanceVariable from: aClassToRefactor

	self assert: aClassToRefactor hasInstanceVariable: anInstanceVariable.
	self assertNoOtherMethodIn: aClassToRefactor superclass definesTemporaryNamed: anInstanceVariable.! !

!PushUpInstanceVariable class methodsFor: 'pre-conditions' stamp: 'HAW 12/26/2019 21:59:38'!
assertNoOtherMethodIn: aSuperclass definesTemporaryNamed: anInstanceVariableName

 	| methodsDefiningTemporaryInSuperclass |

	methodsDefiningTemporaryInSuperclass := OrderedCollection new.
	methodsDefiningTemporaryInSuperclass addAll: (
		aSuperclass methodsWithArgumentOrTemporaryNamed: anInstanceVariableName).

	methodsDefiningTemporaryInSuperclass ifNotEmpty: [
		self 
			canNotRefactorDueToReferencesError: (
				self errorMessageForInstanceVariable: anInstanceVariableName isDefinedInMethodsOf: aSuperclass)
			references: (
				methodsDefiningTemporaryInSuperclass collect: [ :aMethod | MethodReference method: aMethod ])
			to: anInstanceVariableName ]! !

!PushUpInstanceVariable class methodsFor: 'error descriptions' stamp: 'HAW 12/26/2019 20:35:21'!
errorMessageForInstanceVariable: anInstanceVariableName isDefinedInMethodsOf: aSuperclass 
	
	^ anInstanceVariableName, ' exist as temporary in methods of ', aSuperclass name! !


!MoveInstanceVariableApplier methodsFor: 'initialization' stamp: 'HAW 12/26/2019 20:44:46'!
initializeOn: aBrowserWindow for: anInstanceVariableName at: aClassToRefactor 
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	instanceVariableName := anInstanceVariableName! !

!MoveInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/26/2019 20:45:00'!
chooseInstanceVariable
	
	instanceVariableName ifNotNil: [ ^self ].
		
	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anInstanceVariable | ^instanceVariableName := anInstanceVariable ].
		
	self endRequest ! !

!MoveInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/26/2019 20:45:09'!
requestRefactoringParameters

	self chooseInstanceVariable! !

!MoveInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 12/26/2019 20:45:33'!
informChangesToBrowser

	browser acceptedContentsChanged.! !

!MoveInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'HAW 12/26/2019 20:45:38'!
showChanges

	self informChangesToBrowser! !


!MoveInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 12/26/2019 20:46:23'!
on: aBrowser at: aClassToRefactor
	
	^self on: aBrowser for: nil at: aClassToRefactor! !

!MoveInstanceVariableApplier class methodsFor: 'instance creation' stamp: 'HAW 12/26/2019 20:46:28'!
on: aBrowser for: anInstanceVariableName at:  aClassToRefactor 
	
	^self new initializeOn: aBrowser for: anInstanceVariableName at: aClassToRefactor! !


!PushDownInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MSC 12/21/2019 11:03:46'!
selectVariableLabel
	
	^'Select instance variable to push down'! !

!PushDownInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 12/26/2019 20:17:41'!
createRefactoring
		
	^PushDownInstanceVariable named: instanceVariableName from: classToRefactor.
	! !


!PushUpInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MSC 12/19/2019 07:11:47'!
selectVariableLabel
	
	^'Select instance variable to push up'! !

!PushUpInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'HAW 12/26/2019 20:41:01'!
createRefactoring
		
	^PushUpInstanceVariable named: instanceVariableName from: classToRefactor.
	! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/26/2019 19:16:11'!
contextualRenameInClassDefinition 

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]! !


!ChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:06:40'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	self isDoIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]! !

!ChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 12/26/2019 19:06:52'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue: [
			aFileStream newLine; nextPut: $!!.
			aString _  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil: [
				aString _ aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $!! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine! !


!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 12/26/2019 22:03:03'!
browseReferencesOn: aCanNotRefactorDueToReferencesError
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoSelect: aCanNotRefactorDueToReferencesError referencee asString
! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 12/26/2019 20:51:34'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract Method... (K)'.
				#selector 		-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		50.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		60.
				#label 			-> 		'Push Up Instance Variable'.
				#selector 		-> 		#contextualPushUpInClassDefinition.
				#icon 			-> 		#goTopIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		70.
				#label 			-> 		'Push Down Instance Variable'.
				#selector 		-> 		#contextualPushDownInClassDefinition.
				#icon 			-> 		#goBottomIcon
		} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'browser menues' stamp: 'MSC 12/21/2019 10:53:57'!
classRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class... (R)'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'insert superclass...'.
				#object 			-> 		#model.
				#selector 		-> 		#insertSuperclass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'safely remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#safelyRemoveClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'add inst var...'.
				#selector 		-> 		#addInstVar.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'rename inst var...'.
				#selector 		-> 		#renameInstVar.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove inst var...'.
				#selector 		-> 		#removeInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 			20.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove all unreferenced inst var...'.
				#selector 		-> 		#removeAllUnreferencedInstVar.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'push up inst var...'.
				#selector 		-> 		#pushUpInstanceVariable.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		60.
				#label 			-> 		'push down inst var...'.
				#selector 		-> 		#pushDownInstanceVariable.
				#icon 			-> 		#goBottomIcon
			} asDictionary.				
			{
				#itemGroup 	-> 			30.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename global...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameGlobal.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
	}`.
	! !

!methodRemoval: PushUpInstanceVariableApplier class #on:at: stamp: 'HAW 12/26/2019 20:46:18'!
PushUpInstanceVariableApplier class removeSelector: #on:at:!
!methodRemoval: PushUpInstanceVariableApplier class #on:for:at: stamp: 'HAW 12/26/2019 20:46:18'!
PushUpInstanceVariableApplier class removeSelector: #on:for:at:!
!methodRemoval: PushUpInstanceVariableApplier #showChanges stamp: 'HAW 12/26/2019 20:46:10'!
PushUpInstanceVariableApplier removeSelector: #showChanges!
!methodRemoval: PushUpInstanceVariableApplier #chooseInstanceVariable stamp: 'HAW 12/26/2019 20:45:52'!
PushUpInstanceVariableApplier removeSelector: #chooseInstanceVariable!
!methodRemoval: PushUpInstanceVariableApplier #initializeOn:for:at: stamp: 'HAW 12/26/2019 20:45:47'!
PushUpInstanceVariableApplier removeSelector: #initializeOn:for:at:!
!methodRemoval: PushUpInstanceVariableApplier #requestRefactoringParameters stamp: 'HAW 12/26/2019 20:45:54'!
PushUpInstanceVariableApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: PushUpInstanceVariableApplier #informChangesToBrowser stamp: 'HAW 12/26/2019 20:46:10'!
PushUpInstanceVariableApplier removeSelector: #informChangesToBrowser!
!methodRemoval: PushDownInstanceVariableApplier class #on:at: stamp: 'HAW 12/26/2019 20:46:23'!
PushDownInstanceVariableApplier class removeSelector: #on:at:!
!methodRemoval: PushDownInstanceVariableApplier class #on:for:at: stamp: 'HAW 12/26/2019 20:46:28'!
PushDownInstanceVariableApplier class removeSelector: #on:for:at:!
!methodRemoval: PushDownInstanceVariableApplier #showChanges stamp: 'HAW 12/26/2019 20:45:38'!
PushDownInstanceVariableApplier removeSelector: #showChanges!
!methodRemoval: PushDownInstanceVariableApplier #chooseInstanceVariable stamp: 'HAW 12/26/2019 20:45:00'!
PushDownInstanceVariableApplier removeSelector: #chooseInstanceVariable!
!methodRemoval: PushDownInstanceVariableApplier #initializeOn:for:at: stamp: 'HAW 12/26/2019 20:44:46'!
PushDownInstanceVariableApplier removeSelector: #initializeOn:for:at:!
!methodRemoval: PushDownInstanceVariableApplier #requestRefactoringParameters stamp: 'HAW 12/26/2019 20:45:09'!
PushDownInstanceVariableApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: PushDownInstanceVariableApplier #informChangesToBrowser stamp: 'HAW 12/26/2019 20:45:33'!
PushDownInstanceVariableApplier removeSelector: #informChangesToBrowser!
!methodRemoval: PushUpInstanceVariable class #assert:pushUp: stamp: 'HAW 12/26/2019 20:26:57'!
PushUpInstanceVariable class removeSelector: #assert:pushUp:!
!methodRemoval: PushUpInstanceVariable class #errorMessageForInstanceVariableNotExistOnClassToRefactor stamp: 'HAW 12/26/2019 20:13:23'!
PushUpInstanceVariable class removeSelector: #errorMessageForInstanceVariableNotExistOnClassToRefactor!
!methodRemoval: PushUpInstanceVariable class #assert:hasInstanceVariable: stamp: 'HAW 12/26/2019 20:26:36'!
PushUpInstanceVariable class removeSelector: #assert:hasInstanceVariable:!
!methodRemoval: PushUpInstanceVariable class #named:from: stamp: 'HAW 12/26/2019 20:26:22'!
PushUpInstanceVariable class removeSelector: #named:from:!
!methodRemoval: PushUpInstanceVariable class #instanceVariableDoesNotExistOnClassToRefactor stamp: 'HAW 12/26/2019 20:32:46'!
PushUpInstanceVariable class removeSelector: #instanceVariableDoesNotExistOnClassToRefactor!
!methodRemoval: PushUpInstanceVariable class #errorMessageForInstanceVariableAlreadyExistsOnSuperClassToRefactorAsTemporary: stamp: 'HAW 12/26/2019 20:33:07'!
PushUpInstanceVariable class removeSelector: #errorMessageForInstanceVariableAlreadyExistsOnSuperClassToRefactorAsTemporary:!
!methodRemoval: PushUpInstanceVariable class #errorMessageForInstanceVariable: stamp: 'HAW 12/26/2019 20:34:07'!
PushUpInstanceVariable class removeSelector: #errorMessageForInstanceVariable:!
!methodRemoval: PushUpInstanceVariable class #named:to: stamp: 'HAW 12/26/2019 20:18:52'!
PushUpInstanceVariable class removeSelector: #named:to:!
!methodRemoval: PushUpInstanceVariable class #errorMessageForInstanceVariableAlreadyExistsOnSuperClassToRefactorAsTemporary stamp: 'HAW 12/26/2019 20:31:47'!
PushUpInstanceVariable class removeSelector: #errorMessageForInstanceVariableAlreadyExistsOnSuperClassToRefactorAsTemporary!
!methodRemoval: PushUpInstanceVariable #initializeNamed:from: stamp: 'HAW 12/26/2019 20:24:43'!
PushUpInstanceVariable removeSelector: #initializeNamed:from:!
!methodRemoval: PushUpInstanceVariable #removeSubclassesInstanceVariables:from: stamp: 'HAW 12/26/2019 20:36:06'!
PushUpInstanceVariable removeSelector: #removeSubclassesInstanceVariables:from:!
!methodRemoval: PushUpInstanceVariable #initializeNamed:to: stamp: 'HAW 12/26/2019 20:24:36'!
PushUpInstanceVariable removeSelector: #initializeNamed:to:!
!methodRemoval: PushDownInstanceVariable class #errorMessageForInstanceVariableNotExistOnClassToRefactor stamp: 'HAW 12/26/2019 20:13:23'!
PushDownInstanceVariable class removeSelector: #errorMessageForInstanceVariableNotExistOnClassToRefactor!
!methodRemoval: PushDownInstanceVariable class #assert:hasInstanceVariable: stamp: 'HAW 12/26/2019 20:25:58'!
PushDownInstanceVariable class removeSelector: #assert:hasInstanceVariable:!
!methodRemoval: PushDownInstanceVariable class #assert:IsNotAccessingInstanceVariable: stamp: 'HAW 12/26/2019 19:42:31'!
PushDownInstanceVariable class removeSelector: #assert:IsNotAccessingInstanceVariable:!
!methodRemoval: PushDownInstanceVariable class #assert:pushDown: stamp: 'HAW 12/26/2019 19:41:43'!
PushDownInstanceVariable class removeSelector: #assert:pushDown:!
!methodRemoval: PushDownInstanceVariable class #errorMessageForInstanceVariableIsAccessesOnClassToRefactor stamp: 'HAW 12/26/2019 20:08:30'!
PushDownInstanceVariable class removeSelector: #errorMessageForInstanceVariableIsAccessesOnClassToRefactor!
!methodRemoval: PushDownInstanceVariable class #named:from: stamp: 'HAW 12/26/2019 20:25:22'!
PushDownInstanceVariable class removeSelector: #named:from:!
!methodRemoval: PushDownInstanceVariable class #instanceVariableDoesNotExistOnClassToRefactor stamp: 'HAW 12/26/2019 20:26:12'!
PushDownInstanceVariable class removeSelector: #instanceVariableDoesNotExistOnClassToRefactor!
!methodRemoval: PushDownInstanceVariable class #errorMessageForInstanceVariable: stamp: 'HAW 12/26/2019 20:10:15'!
PushDownInstanceVariable class removeSelector: #errorMessageForInstanceVariable:!
!methodRemoval: PushDownInstanceVariable class #errorMessageForInstanceVariableIsAccessesOnClassToRefactor: stamp: 'HAW 12/26/2019 20:09:32'!
PushDownInstanceVariable class removeSelector: #errorMessageForInstanceVariableIsAccessesOnClassToRefactor:!
!methodRemoval: PushDownInstanceVariable class #named:to: stamp: 'HAW 12/26/2019 19:41:11'!
PushDownInstanceVariable class removeSelector: #named:to:!
!methodRemoval: PushDownInstanceVariable class #assertCanPushDown:from: stamp: 'HAW 12/26/2019 20:25:13'!
PushDownInstanceVariable class removeSelector: #assertCanPushDown:from:!
!methodRemoval: PushDownInstanceVariable #initializeNamed:from: stamp: 'HAW 12/26/2019 20:23:54'!
PushDownInstanceVariable removeSelector: #initializeNamed:from:!
!methodRemoval: PushDownInstanceVariable #pushDownInstanceVariableToAllSubclasses:to: stamp: 'HAW 12/26/2019 20:15:17'!
PushDownInstanceVariable removeSelector: #pushDownInstanceVariableToAllSubclasses:to:!
!methodRemoval: PushDownInstanceVariable #initializeNamed:to: stamp: 'HAW 12/26/2019 20:14:15'!
PushDownInstanceVariable removeSelector: #initializeNamed:to:!
!methodRemoval: SmalltalkEditor #contextualPushDown stamp: 'HAW 12/26/2019 19:36:34'!
SmalltalkEditor removeSelector: #contextualPushDown!
!methodRemoval: SmalltalkEditor #pushUpInstanceVariableOn:for:at: stamp: 'HAW 12/26/2019 19:32:38'!
SmalltalkEditor removeSelector: #pushUpInstanceVariableOn:for:at:!
!methodRemoval: SmalltalkEditor #contextualPushUp stamp: 'HAW 12/26/2019 19:36:36'!
SmalltalkEditor removeSelector: #contextualPushUp!
!methodRemoval: SmalltalkEditor #contextualPushUpInClassDefinitionOf:in: stamp: 'HAW 12/26/2019 19:30:58'!
SmalltalkEditor removeSelector: #contextualPushUpInClassDefinitionOf:in:!
!methodRemoval: SmalltalkEditor #pushDownInstanceVariableOn:for:at: stamp: 'HAW 12/26/2019 19:33:38'!
SmalltalkEditor removeSelector: #pushDownInstanceVariableOn:for:at:!
!methodRemoval: SmalltalkEditor #contextualPushDownInClassDefinitionOf:in: stamp: 'HAW 12/26/2019 19:32:58'!
SmalltalkEditor removeSelector: #contextualPushDownInClassDefinitionOf:in:!
!methodRemoval: CodeProvider #instanceVariablePushedDown stamp: 'HAW 12/26/2019 20:45:27'!
CodeProvider removeSelector: #instanceVariablePushedDown!

!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
('extract method' extractMethod extractMethod:)
('*LiveTyping' actualImplementorsOfIt actualImplementorsOfIt: actualLocalImplementorsOfIt actualLocalImplementorsOfIt: actualSendersOfIt actualSendersOfIt: balloonTypeInfoAt: balloonTypeInfoInMethodAt: balloonTypeInfoOf:in:definedAt: balloonTypeInfoOfInstanceVariableAt: showSelectionTypeInfo showSelectionTypeInfo: showSelectionTypeInfoInMethod showTypeInfoOf:in:definedAt: showTypeInfoOfInstanceVariable typeCheckMethod typeCheckMethod:)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
!


!ChangeListElement reorganize!
('testing' isClassDeletion isDoIt isMethodDeletion)
('accessing' text)
('perform' performOn:)
('fileIn/fileOut' fileIn fileOutOn:)
!


!ChangeRecord reorganize!
('access' category changeClass changeClassName changeType compilerClass dateAndTime fileName isMetaClassChange methodSelector prior stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: file:position:type:class:category:meta:stamp:prior: markAsTest:)
('testing' isDoIt isTestClassChange)
('printing' printOn:)
('fileIn/fileOut' fileIn fileOutOn:)
!


!FeatureChangeRecord reorganize!
('accessing' changeClass changeClassName changeType feature methodSelector string type:feature:)
('fileIn/fileOut' fileIn fileOutOn:)
!


!MoveInstanceVariable class reorganize!
('instance creation' named:from:)
('pre-conditions' assert:hasInstanceVariable: assertCanMove:from:)
('error descriptions' instanceVariableDoesNotExistOnClassToRefactor)
!


!PushDownInstanceVariable reorganize!
('applying' apply)
('applying - private' pushDownInstanceVariableToAllSubclasses)
!


!PushDownInstanceVariable class reorganize!
('pre-conditions' assert:isNotAccessingInstanceVariable: assertCanMove:from:)
('error descriptions' errorMessageForInstanceVariable:isAccessedInMethodsOf:)
!


!PushUpInstanceVariable reorganize!
('applying' apply)
('applying - private' removeSubclassesInstanceVariable)
!


!PushUpInstanceVariable class reorganize!
('pre-conditions' assertCanMove:from: assertNoOtherMethodIn:definesTemporaryNamed:)
('error descriptions' errorMessageForInstanceVariable:isDefinedInMethodsOf:)
!


!PushDownInstanceVariableApplier reorganize!
('refactoring - parameters request' selectVariableLabel)
('refactoring - creation' createRefactoring)
!


!PushDownInstanceVariableApplier class reorganize!
('as yet unclassified')
!


!PushUpInstanceVariableApplier reorganize!
('refactoring - parameters request' selectVariableLabel)
('refactoring - creation' createRefactoring)
!


!PushUpInstanceVariableApplier class reorganize!
('as yet unclassified')
!

