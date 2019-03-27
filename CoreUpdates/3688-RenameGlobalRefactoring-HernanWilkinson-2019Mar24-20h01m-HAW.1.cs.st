'From Cuis 5.0 of 7 November 2016 [latest update: #3685] on 26 March 2019 at 7:58:13 pm'!
!classDefinition: #RenameGlobal category: #'Tools-Refactoring'!
Refactoring subclass: #RenameGlobal
	instanceVariableNames: 'system oldName newName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameGlobalApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameGlobalApplier
	instanceVariableNames: 'browser newName oldName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #NewGlobalPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #NewGlobalPrecondition
	instanceVariableNames: 'system newName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/26/2019 19:11:22'!
renameGlobal
	
	(RenameGlobalApplier on: self for: '') value! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/26/2019 18:42:51'!
renameGlobalOn: aBrowser for: anOldName

	(RenameGlobalApplier on: aBrowser for: anOldName) value! !


!RenameGlobal methodsFor: 'applying' stamp: 'HAW 3/26/2019 18:34:07'!
apply
	
	| renamedReferences |
	
	system at: newName put: (system at: oldName).
	renamedReferences := self renameReferences.
	system removeKey: oldName.
	
	^renamedReferences 
	! !

!RenameGlobal methodsFor: 'initialization' stamp: 'HAW 3/26/2019 17:55:21'!
initializeFrom: anOldName to: aNewName in: aSystem  

	oldName := anOldName.
	newName := aNewName.
	system := aSystem.
	
	! !

!RenameGlobal methodsFor: 'accessing' stamp: 'HAW 3/26/2019 17:56:01'!
newName
	
	^newName ! !

!RenameGlobal methodsFor: 'accessing' stamp: 'HAW 3/26/2019 18:03:41'!
referencesToOldName
	
	^system allCallsOn: oldName! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 17:55:21'!
newSourceCodeOf: aCompiledMethod 
	
	| newSource |
	
	newSource := aCompiledMethod sourceCode copyReplacing: (self rangesToReplaceOf: aCompiledMethod) with: newName.

	^newSource! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 18:12:45'!
rangesForLiteralOf: methodNode
	
	| literalRanges |
	
	literalRanges := methodNode positionsForLiteralNode: oldName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].
	
	^literalRanges ! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 18:12:31'!
rangesForLiteralVariableOf: methodNode
	
	^methodNode positionsForLiteralVariableNode: oldName ifAbsent: [ #() ]
	! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 17:14:05'!
rangesToReplaceOf: aCompiledMethod 
	
	| methodNode ranges |
	
	methodNode := aCompiledMethod methodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).
	
	^ranges ! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 19:36:06'!
references: aMethodReference classVarNamed: aName

	^aMethodReference actualClass theNonMetaClass definesClassVariableNamedInHierarchy: aName ! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 18:25:27'!
referencesOldName: aMethodReference

	^self references: aMethodReference classVarNamed: oldName ! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 18:34:48'!
rejectReferencesToClassVariablesFrom: references
	
	^references reject: [ :aMethodReference | self referencesOldName: aMethodReference ].! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 17:14:05'!
renameReference: aReferencingMethod 
	
	| newSource |
	
	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource ! !

!RenameGlobal methodsFor: 'applying - private' stamp: 'HAW 3/26/2019 18:51:20'!
renameReferences
	
	| references |
	
	references := self referencesToOldName.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	
	^references! !


!RenameGlobal class methodsFor: 'pre-conditions' stamp: 'HAW 3/26/2019 19:17:48'!
assert: anOldName isDefinedIn: aSystem
	
	(aSystem bindingOf: anOldName) ifNil: [ self signalGlobalNotDefined: anOldName ]! !

!RenameGlobal class methodsFor: 'pre-conditions' stamp: 'HAW 3/26/2019 17:20:18'!
assert: anOldName isNotEqualTo: aNewName

	anOldName = aNewName ifTrue: [ self signalNewNameEqualsOldName]! !

!RenameGlobal class methodsFor: 'pre-conditions' stamp: 'HAW 3/26/2019 17:18:11'!
assertIsNotClass: anOldName

	(Smalltalk classNamed: anOldName) ifNotNil: [ self signalGlobalToRenameCanNotBeClass]! !

!RenameGlobal class methodsFor: 'pre-conditions' stamp: 'HAW 3/26/2019 19:50:14'!
assertIsNotEmpty: anOldName

	anOldName isEmpty ifTrue: [ self signalOldNameIsEmpty ]! !

!RenameGlobal class methodsFor: 'pre-conditions' stamp: 'HAW 3/26/2019 17:39:50'!
newGlobalPreconditionClass

	^NewGlobalPrecondition ! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 19:18:54'!
errorMessageForGlobalNotDefined: anOldName

	^ anOldName asString, ' is not defined as global variable'! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:18:56'!
globalCanNotBeClassErrorMessage
	
	^'Global to rename can not be a class'! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 18:56:16'!
newNameEqualsOldNameErrorMessage
	
	^'New name is equal to the old one'! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 19:51:21'!
oldNameCanNotBeEmptyErrorMessage
	
	^'Global variable name to rename can not be empty'! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 19:18:19'!
signalGlobalNotDefined: anOldName

	self refactoringError: (self errorMessageForGlobalNotDefined: anOldName)! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:18:37'!
signalGlobalToRenameCanNotBeClass
	
	self refactoringError: self globalCanNotBeClassErrorMessage! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:14:05'!
signalNewNameEqualsOldName

	self refactoringError: self newNameEqualsOldNameErrorMessage.! !

!RenameGlobal class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 19:50:54'!
signalOldNameIsEmpty
	
	self refactoringError: self oldNameCanNotBeEmptyErrorMessage! !

!RenameGlobal class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 17:15:55'!
from: anOldName to: aNewName

	^self from: anOldName to: aNewName in: Smalltalk
	! !

!RenameGlobal class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 19:48:41'!
from: anOldName to: aNewName in: aSystem

	self assertIsNotEmpty: anOldName.
	self assert: anOldName isDefinedIn: aSystem.
	self assertIsNotClass: anOldName.
	self assert: anOldName isNotEqualTo: aNewName.
	self newGlobalPreconditionClass valueFor: aNewName in: aSystem.
	
	^self new initializeFrom: anOldName to: aNewName in: aSystem ! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/26/2019 17:36:48'!
sendersFrom: methodReferences

	^ methodReferences collect: [:aMethodReference | aMethodReference compiledMethod ]! !


!RenameSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 19:56:53'!
createAndValueHandlingExceptionsOn: aModel of: aMessageNode in: aSelectedClass at: aSelectedSelector
	
	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass 
! !


!RenameGlobalApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/26/2019 18:53:42'!
askNewName

	newName := self request: 'Enter new name:' initialAnswer: oldName asString.
	newName := newName withBlanksTrimmed asSymbol.
	! !

!RenameGlobalApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/26/2019 19:13:49'!
askOldName

	oldName := self request: 'Enter global name to rename:' initialAnswer: oldName.
	oldName := oldName withBlanksTrimmed asSymbol.
	! !

!RenameGlobalApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/26/2019 19:13:00'!
requestRefactoringParameters

	oldName isEmpty ifTrue: [ self askOldName ].
	self askNewName! !

!RenameGlobalApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/26/2019 18:36:05'!
createRefactoring

	^RenameGlobal from: oldName to: newName in: Smalltalk ! !

!RenameGlobalApplier methodsFor: 'refactoring - changes' stamp: 'HAW 3/26/2019 18:35:42'!
openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references' autoSelect: newName ]
! !

!RenameGlobalApplier methodsFor: 'refactoring - changes' stamp: 'HAW 3/26/2019 18:36:56'!
showChanges

	self openChangedMethods
! !

!RenameGlobalApplier methodsFor: 'initialization' stamp: 'HAW 3/26/2019 18:37:12'!
initializeOn: aBrowser for: anOldName

	browser := aBrowser.
	oldName := anOldName.
	! !


!RenameGlobalApplier class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 19:12:35'!
on: aBrowser 

	^self on: aBrowser for: ''! !

!RenameGlobalApplier class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 18:37:50'!
on: aBrowser for: anOldName

	^self new initializeOn: aBrowser for: anOldName! !


!NewGlobalPrecondition methodsFor: 'evaluating' stamp: 'HAW 3/26/2019 17:29:43'!
value

	self 
		assertNewNameIsNotEmpty;
		assertNewNameIsSymbol;
		assertNewNameHasNoSeparators;
		assertNewNameDoesNotExistInSystem.

! !

!NewGlobalPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/26/2019 17:26:43'!
assertNewNameDoesNotExistInSystem

	system at: newName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists ]
			ifFalse: [ self signalGlobalAlreadyExists ]].! !

!NewGlobalPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/26/2019 17:27:22'!
assertNewNameHasNoSeparators

	(newName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewNameCanNotHaveSeparators ]! !

!NewGlobalPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/26/2019 17:28:35'!
assertNewNameIsNotEmpty

	newName withBlanksTrimmed isEmpty ifTrue: [ self signalNewNameCanNotBeEmpty]! !

!NewGlobalPrecondition methodsFor: 'evaluating - private' stamp: 'HAW 3/26/2019 17:29:43'!
assertNewNameIsSymbol

	newName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]! !

!NewGlobalPrecondition methodsFor: 'initialization' stamp: 'HAW 3/26/2019 17:37:17'!
initializeFor: aNewName in: aSystem  

	newName := aNewName.
	system := aSystem.
! !

!NewGlobalPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:26:07'!
signalClassAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newName).! !

!NewGlobalPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:26:07'!
signalGlobalAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newName)! !

!NewGlobalPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:28:54'!
signalNewNameCanNotBeEmpty
	
	self refactoringError: self class newNameCanNotBeEmptyErrorMessage! !

!NewGlobalPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:27:57'!
signalNewNameCanNotHaveSeparators
	
	self refactoringError: self class newNameCanNotHaveSeparatorsErrorMessage ! !

!NewGlobalPrecondition methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 17:22:52'!
signalNewNameMustBeSymbol

	self refactoringError: self class newNameMustBeSymbolErrorMessage.! !


!NewGlobalPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/26/2019 17:26:34'!
errorMessageForAlreadyExistClassNamed: aNewName

	^'Class named ', aNewName, ' already exist'! !

!NewGlobalPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/26/2019 17:26:21'!
errorMessageForAlreadyExistGlobalNamed: aNewName

	^'There is already a global variable named ', aNewName ! !

!NewGlobalPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/26/2019 17:29:00'!
newNameCanNotBeEmptyErrorMessage
	
	^'New name can not be empty'! !

!NewGlobalPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/26/2019 17:28:07'!
newNameCanNotHaveSeparatorsErrorMessage
	
	^'New name can not have separators'! !

!NewGlobalPrecondition class methodsFor: 'error messages' stamp: 'HAW 3/26/2019 17:30:05'!
newNameMustBeSymbolErrorMessage
	
	^'New name must be a symbol'! !

!NewGlobalPrecondition class methodsFor: 'instance creation' stamp: 'HAW 3/26/2019 17:38:25'!
for: aNewName in: aSystem  

	^self new initializeFor: aNewName in: aSystem ! !

!NewGlobalPrecondition class methodsFor: 'evaluation' stamp: 'HAW 3/26/2019 17:40:04'!
valueFor: aNewName in: aSystem  

	^(self for: aNewName in: aSystem) value! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/26/2019 18:57:11'!
rename: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor name ].

	self ifRenameCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [ ^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [ ^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | variableName |
			variableName := aNodeUnderCursor key key.
			(Smalltalk classNamed: variableName) ifNotNil: [ :classToRename | ^self renameClassOn: self codeProvider for: classToRename ].
			(Smalltalk bindingOf: variableName) ifNotNil: [ ^self renameGlobalOn: self codeProvider for: variableName ] ].

		^morph flash ]! !


!InsertSuperclass class methodsFor: 'instance creation' stamp: 'HAW 3/3/2019 09:11:31'!
to: aClass named: aSuperclassName in: aSystem undeclared: anUndeclared

	self newClassPreconditionClass valueFor: aSuperclassName in: aSystem undeclared: anUndeclared.
	
	^self new initializeTo: aClass theNonMetaClass named: aSuperclassName ! !


!RenameClass class methodsFor: 'exceptions' stamp: 'HAW 3/26/2019 18:56:37'!
newNameEqualsOldNameErrorMessage
	
	^'New class name is equal to the old one'! !

!RenameClass class methodsFor: 'instance creation' stamp: 'HAW 8/13/2018 18:45:15'!
from: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	self assertIsNotMeta: aClass.
	self assert: aClass isNotNamed: aNewClassName.
	self newClassPreconditionClass valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary.
	
	^self new initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 
! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 3/26/2019 19:09:35'!
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
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'remove all unreferenced inst var...'.
				#selector 		-> 		#removeAllUnreferencedInstVar.
				#icon 			-> 		#deleteIcon
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


!NewClassPrecondition class methodsFor: 'evaluation' stamp: 'HAW 8/13/2018 17:37:20'!
valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	^(self for: aNewClassName in: aSystem undeclared: anUndeclaredDictionary) value! !

!methodRemoval: NewGlobalPrecondition class #errorMessageForNewClassIsUndeclared:!
NewGlobalPrecondition class removeSelector: #errorMessageForNewClassIsUndeclared:!
!methodRemoval: NewGlobalPrecondition class #for:in:undeclared:!
NewGlobalPrecondition class removeSelector: #for:in:undeclared:!
!methodRemoval: NewGlobalPrecondition class #newClassCanNotHaveSeparatorsErrorMessage!
NewGlobalPrecondition class removeSelector: #newClassCanNotHaveSeparatorsErrorMessage!
!methodRemoval: NewGlobalPrecondition class #newClassNameCanNotBeEmptyErrorMessage!
NewGlobalPrecondition class removeSelector: #newClassNameCanNotBeEmptyErrorMessage!
!methodRemoval: NewGlobalPrecondition class #newClassNameCanNotHaveSeparatorsErrorMessage!
NewGlobalPrecondition class removeSelector: #newClassNameCanNotHaveSeparatorsErrorMessage!
!methodRemoval: NewGlobalPrecondition class #newNameMustStartWithUppercaseLetterErrorMessage!
NewGlobalPrecondition class removeSelector: #newNameMustStartWithUppercaseLetterErrorMessage!
!methodRemoval: NewGlobalPrecondition class #valueFor:in:undeclared:!
NewGlobalPrecondition class removeSelector: #valueFor:in:undeclared:!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameDoesNotExistInSystem!
NewGlobalPrecondition removeSelector: #assertNewClassNameDoesNotExistInSystem!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameHasNoSeparators!
NewGlobalPrecondition removeSelector: #assertNewClassNameHasNoSeparators!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameIsNotEmpty!
NewGlobalPrecondition removeSelector: #assertNewClassNameIsNotEmpty!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameIsNotUndeclaredInUndeclared!
NewGlobalPrecondition removeSelector: #assertNewClassNameIsNotUndeclaredInUndeclared!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameStartsWithUppercaseLetter!
NewGlobalPrecondition removeSelector: #assertNewClassNameStartsWithUppercaseLetter!
!methodRemoval: NewGlobalPrecondition #assertNewClassNameSymbol!
NewGlobalPrecondition removeSelector: #assertNewClassNameSymbol!
!methodRemoval: NewGlobalPrecondition #assertNewNameSymbol!
NewGlobalPrecondition removeSelector: #assertNewNameSymbol!
!methodRemoval: NewGlobalPrecondition #initializeFor:in:undeclared:!
NewGlobalPrecondition removeSelector: #initializeFor:in:undeclared:!
!methodRemoval: NewGlobalPrecondition #signalNewClassIsUndeclared!
NewGlobalPrecondition removeSelector: #signalNewClassIsUndeclared!
!methodRemoval: NewGlobalPrecondition #signalNewClassNameCanNotBeEmpty!
NewGlobalPrecondition removeSelector: #signalNewClassNameCanNotBeEmpty!
!methodRemoval: NewGlobalPrecondition #signalNewClassNameCanNotHaveSeparators!
NewGlobalPrecondition removeSelector: #signalNewClassNameCanNotHaveSeparators!
!methodRemoval: NewGlobalPrecondition #signalNewNameMustStartWithUppercaseLetter!
NewGlobalPrecondition removeSelector: #signalNewNameMustStartWithUppercaseLetter!
!methodRemoval: RenameGlobalApplier #askNewClassName!
RenameGlobalApplier removeSelector: #askNewClassName!
!methodRemoval: RenameGlobalApplier #informChangesToBrowser!
RenameGlobalApplier removeSelector: #informChangesToBrowser!
!methodRemoval: RenameSelectorApplier class #createAndValueHandlingExceptionsOn:of:in:!
RenameSelectorApplier class removeSelector: #createAndValueHandlingExceptionsOn:of:in:!
!methodRemoval: RenameSelectorApplier #sendersFrom:!
RenameSelectorApplier removeSelector: #sendersFrom:!
!methodRemoval: RenameGlobal class #assert:isNotNamed:!
RenameGlobal class removeSelector: #assert:isNotNamed:!
!methodRemoval: RenameGlobal class #assertIsNotMeta:!
RenameGlobal class removeSelector: #assertIsNotMeta:!
!methodRemoval: RenameGlobal class #classToRenameCanNotBeMetaclassErrorMessage!
RenameGlobal class removeSelector: #classToRenameCanNotBeMetaclassErrorMessage!
!methodRemoval: RenameGlobal class #from:to:in:undeclared:!
RenameGlobal class removeSelector: #from:to:in:undeclared:!
!methodRemoval: RenameGlobal class #newClassPreconditionClass!
RenameGlobal class removeSelector: #newClassPreconditionClass!
!methodRemoval: RenameGlobal class #signalClassToRenameCanNotBeMetaclass!
RenameGlobal class removeSelector: #signalClassToRenameCanNotBeMetaclass!
!methodRemoval: RenameGlobal #initializeFrom:to:in:undeclared:!
RenameGlobal removeSelector: #initializeFrom:to:in:undeclared:!
!methodRemoval: RenameGlobal #newClassName!
RenameGlobal removeSelector: #newClassName!
!methodRemoval: RenameGlobal #referencesNewClassName:!
RenameGlobal removeSelector: #referencesNewClassName:!
!methodRemoval: RenameGlobal #referencesOldClassName:!
RenameGlobal removeSelector: #referencesOldClassName:!
!methodRemoval: RenameGlobal #referencesToOldClass!
RenameGlobal removeSelector: #referencesToOldClass!
!methodRemoval: RenameGlobal #referencesToOldClassName!
RenameGlobal removeSelector: #referencesToOldClassName!

!InsertSuperclass class reorganize!
('instance creation' newClassPreconditionClass to:named: to:named:in:undeclared:)
!


!RenameClass class reorganize!
('pre-conditions' assert:isNotNamed: assertIsNotMeta: newClassPreconditionClass)
('exceptions' classToRenameCanNotBeMetaclassErrorMessage newNameEqualsOldNameErrorMessage signalClassToRenameCanNotBeMetaclass signalNewNameEqualsOldName)
('instance creation' from:to: from:to:in: from:to:in:undeclared:)
!


!RenameGlobal reorganize!
('applying' apply)
('initialization' initializeFrom:to:in:)
('accessing' newName referencesToOldName)
('applying - private' newSourceCodeOf: rangesForLiteralOf: rangesForLiteralVariableOf: rangesToReplaceOf: references:classVarNamed: referencesOldName: rejectReferencesToClassVariablesFrom: renameReference: renameReferences)
!


!RenameGlobal class reorganize!
('pre-conditions' assert:isDefinedIn: assert:isNotEqualTo: assertIsNotClass: assertIsNotEmpty: newGlobalPreconditionClass)
('exceptions' errorMessageForGlobalNotDefined: globalCanNotBeClassErrorMessage newNameEqualsOldNameErrorMessage oldNameCanNotBeEmptyErrorMessage signalGlobalNotDefined: signalGlobalToRenameCanNotBeClass signalNewNameEqualsOldName signalOldNameIsEmpty)
('instance creation' from:to: from:to:in:)
!


!ChangeSelectorApplier reorganize!
('refactoring - parameters request' askForImplementosAndSenders askScope calculateImplementorsAndSenders implementorsAndSendersForClass implementorsAndSendersForHierarchy implementorsAndSendersInCategory implementorsAndSendersInCategoryAndHierarchy implementorsAndSendersInSystem initializeImplementorsAndSenders scopeOptionLabels sendersFrom: startWizard)
('accessing' implementors implementors: oldSelector senders senders: wizardStepWindow:)
('refactoring - changes' closeBrowser doNotShowChanges informChangesToBrowser messageSetWindowClass showChanges showChangesInMessageSetWindow)
('refactoring - creation' createAndApplyRefactoring createAndApplyRefactoringWhenNoSendersAndOneImplementor: refactoringClass)
('initialization' initializeOn:for:in:)
('evaluation' ifHasNoSendersAndOneImplementor:ifNot: value wizardEnded)
!


!RenameSelectorApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
!


!RenameGlobalApplier reorganize!
('refactoring - parameters request' askNewName askOldName requestRefactoringParameters)
('refactoring - creation' createRefactoring)
('refactoring - changes' openChangedMethods showChanges)
('initialization' initializeOn:for:)
!


!NewClassPrecondition class reorganize!
('error messages' errorMessageForAlreadyExistClassNamed: errorMessageForAlreadyExistGlobalNamed: errorMessageForNewClassIsUndeclared: newClassNameCanNotBeEmptyErrorMessage newClassNameCanNotHaveSeparatorsErrorMessage newNameMustBeSymbolErrorMessage newNameMustStartWithUppercaseLetterErrorMessage)
('instance creation' for:in:undeclared:)
('evaluation' valueFor:in:undeclared:)
!


!NewGlobalPrecondition reorganize!
('evaluating' value)
('evaluating - private' assertNewNameDoesNotExistInSystem assertNewNameHasNoSeparators assertNewNameIsNotEmpty assertNewNameIsSymbol)
('initialization' initializeFor:in:)
('exceptions' signalClassAlreadyExists signalGlobalAlreadyExists signalNewNameCanNotBeEmpty signalNewNameCanNotHaveSeparators signalNewNameMustBeSymbol)
!


!NewGlobalPrecondition class reorganize!
('error messages' errorMessageForAlreadyExistClassNamed: errorMessageForAlreadyExistGlobalNamed: newNameCanNotBeEmptyErrorMessage newNameCanNotHaveSeparatorsErrorMessage newNameMustBeSymbolErrorMessage)
('instance creation' for:in:)
('evaluation' valueFor:in:)
!

