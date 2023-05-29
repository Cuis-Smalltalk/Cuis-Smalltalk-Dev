'From Cuis 6.0 [latest update: #5818] on 29 May 2023 at 4:22:42 pm'!
Smalltalk renameClassNamed: #PushDownMethod as: #PushDownMethodToSubclasses!
!classDefinition: #PushDownMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushDownMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
PushDownMethod subclass: #PushDownMethodToOneSubclass
	instanceVariableNames: 'subclassToPushDown subclassToPushDownTo implementSubclassResponsibility '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushDownMethodToSubclasses category: #'Tools-Refactoring'!
PushDownMethod subclass: #PushDownMethodToSubclasses
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
MoveMethodApplier subclass: #PushDownMethodToOneSubclassApplier
	instanceVariableNames: 'subclassToPushDownTo '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
Smalltalk renameClassNamed: #PushDownMethodApplier as: #PushDownMethodToSubclassesApplier!

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 5/29/2023 16:12:52'!
pushDownSelectorToOneSubclass

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodToOneSubclassApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 5/29/2023 16:12:20'!
pushDownSelectorToSubclasses

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodToSubclassesApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].! !


!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 14:49:28'!
implementSubclassResponsibility

	^ method methodClass compile: self subclassResponsibilitySourceCode! !

!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 14:49:02'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selector;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !


!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 11:23:48'!
addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames

	| shadowedInstVarNames |

	shadowedInstVarNames := subclass instVarNames select: [:instVarName |
		(aMethodToPushDown hasArgumentOrTemporaryNamed: instVarName) ].

	 shadowedInstVarNames ifNotEmpty: [
		subclassesWithShadowedInstVarNames at: subclass put: shadowedInstVarNames ]! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 12:39:02'!
assert: aMethodToPushDown hasNoShadowedInstVarsIn: aSubclasses 

	| subclassesWithShadowedInstVarNames |

	subclassesWithShadowedInstVarNames := Dictionary new.

	aSubclasses do: [:subclass |
		self addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames ].

	subclassesWithShadowedInstVarNames ifNotEmpty: [
		self signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames]! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 11:24:13'!
is: subclass equivalentlyImplementedIn: aMethodToPushDown

	^ subclass 
		compiledMethodAt: aMethodToPushDown selector
		ifPresent: [ :methodInSubclass | (methodInSubclass equivalentTo: aMethodToPushDown) not ]
		ifAbsent: [ false ]! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 11:18:50'!
warnIf: aMethodToPushDown existsOn: aSubclasses 

	| subclassesImplementingMessage |

	subclassesImplementingMessage := aSubclasses select: [:subclass |
		self is: subclass equivalentlyImplementedIn: aMethodToPushDown ].
		
	subclassesImplementingMessage ifNotEmpty: [
		self warnMessageAlreadyImplementedIn: subclassesImplementingMessage ]! !

!PushDownMethod class methodsFor: 'warnings' stamp: 'HAW 5/29/2023 11:19:21'!
warnMessageAlreadyImplementedIn: subclassesImplementingMessage

	self refactoringWarning:
		(self warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage)! !

!PushDownMethod class methodsFor: 'warnings' stamp: 'HAW 5/29/2023 11:19:13'!
warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Method already exist in the following subclasses:';
			newLine.

		subclassesImplementingMessage asCommaSeparated: [ :aClass | stream print: aClass ] on: stream.

		stream
			newLine;
			nextPutAll: 'If you continue they will be overwritten' ].! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 5/29/2023 11:25:48'!
errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not push down method because the following subclasses';
			newLine;
			nextPutAll: 'would have shadowed instance variables:'.

		subclassesWithShadowedInstVarNames
			keysAndValuesDo: [ :class :instVarNames |
				stream
					newLine;
					print: class;
					nextPutAll: ': '.
				instVarNames asCommaSeparated: [:instVarName | stream nextPutAll: instVarName ] on: stream ]].


! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 5/29/2023 11:25:39'!
signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames

	self refactoringError: (
		self errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames)! !


!PushDownMethodToOneSubclass methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 14:50:05'!
removeMethod
	
	super removeMethod.
	implementSubclassResponsibility ifTrue: [ self implementSubclassResponsibility ]! !

!PushDownMethodToOneSubclass methodsFor: 'initialization' stamp: 'HAW 5/29/2023 16:18:50'!
initializeFor: aMethodToPushDown to: aSubclassToPushDown implementingSubclassResponsibility: shouldImplementSubclassResponsibility 

	method := aMethodToPushDown.
	subclassToPushDownTo := aSubclassToPushDown.
	implementSubclassResponsibility := shouldImplementSubclassResponsibility ! !


!PushDownMethodToOneSubclass class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 11:36:48'!
assert: aSubclassToPushDownTo isSubclassOf: aClassToRefactor

	(aClassToRefactor subclasses includes: aSubclassToPushDownTo) ifFalse: [
		self signal: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor ]! !

!PushDownMethodToOneSubclass class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 14:42:05'!
warnIf: aMethodToPushDown isNotDefinedInOtherSubclassesBut: aSubclassToPushDownTo 
	
	| siblings siblingsNotImplementing |
	
	siblings := aMethodToPushDown methodClass subclasses asOrderedCollection remove: aSubclassToPushDownTo; yourself.
	siblingsNotImplementing := siblings reject: [ :aSibling | aSibling includesSelector: aMethodToPushDown selector ].
	
	siblingsNotImplementing ifNotEmpty: [ self signalNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing ].
	^siblingsNotImplementing isEmpty! !

!PushDownMethodToOneSubclass class methodsFor: 'exceptions' stamp: 'HAW 5/29/2023 11:38:58'!
errorMessage: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor 
	
	^'Can not push down method because ', aSubclassToPushDownTo name, ' is not sublcass of ', aClassToRefactor name! !

!PushDownMethodToOneSubclass class methodsFor: 'exceptions' stamp: 'HAW 5/29/2023 11:38:11'!
signal: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor 
	
	self refactoringError: (self errorMessage: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor)! !

!PushDownMethodToOneSubclass class methodsFor: 'exceptions' stamp: 'HAW 5/29/2023 11:48:58'!
signalNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing 
	
	self refactoringWarning: (self warningMessageNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing)! !

!PushDownMethodToOneSubclass class methodsFor: 'warnings' stamp: 'HAW 5/29/2023 11:50:42'!
warningMessageNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing 
	
	^String streamContents: [ :stream |
		stream
			nextPutAll: 'This is not a refactoring because the message is not implemented in:';
			newLine.

		siblingsNotImplementing asCommaSeparated: [ :aClass | stream print: aClass ] on: stream.

		stream
			newLine;
			nextPutAll: 'Existing code may not work if applied' ].! !


!PushDownMethodToOneSubclassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/29/2023 16:15:18'!
requestRefactoringParameters

	subclassToPushDownTo := (SelectionMenu selections: methodToMove methodClass subclasses)
		startUpWithCaption: 'Select subclass to push down method to'.
	subclassToPushDownTo ifNil: [ self endRequest ]! !


!PushDownMethodToOneSubclass methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 16:21:37'!
moveMethod

	subclassToPushDownTo 
		compile: method sourceCode 
		classified: self methodCategory.
	
! !


!PushDownMethodToOneSubclass class methodsFor: 'instance creation' stamp: 'HAW 5/29/2023 14:47:10'!
for: aMethodToPushDown to: aSubclassToPushDownTo 

	| allSiblingsImplementMessage shouldImplementSubclassResponsibility |
	
	self assert: aSubclassToPushDownTo isSubclassOf: aMethodToPushDown methodClass.
	self assert: aMethodToPushDown hasNoShadowedInstVarsIn: { aSubclassToPushDownTo }.
	self warnIf: aMethodToPushDown existsOn: { aSubclassToPushDownTo }.
	allSiblingsImplementMessage := self warnIf: aMethodToPushDown isNotDefinedInOtherSubclassesBut: aSubclassToPushDownTo.
	shouldImplementSubclassResponsibility := allSiblingsImplementMessage and: [ aMethodToPushDown methodClass subclasses size > 1 ].

	^self new initializeFor: aMethodToPushDown to: aSubclassToPushDownTo implementingSubclassResponsibility: shouldImplementSubclassResponsibility
	! !


!PushDownMethodToSubclasses methodsFor: 'applying - private' stamp: 'HAW 5/29/2023 14:49:22'!
removeMethod

	super removeMethod.
	self implementSubclassResponsibility
							! !


!PushDownMethodToSubclasses class methodsFor: 'instance creation' stamp: 'HAW 5/29/2023 11:15:48'!
for: aMethodToPushDown

	self assertIsValidToPushDownMethod: aMethodToPushDown.
	self warnIf: aMethodToPushDown existsOn: aMethodToPushDown methodClass subclasses.

	^self new initializeFor: aMethodToPushDown ! !

!PushDownMethodToSubclasses class methodsFor: 'pre-conditions' stamp: 'HAW 5/29/2023 11:22:56'!
assertIsValidToPushDownMethod: aMethodToPushDown

	self assertHasSubclassesToPushDownMethodTo: aMethodToPushDown.
	self assert: aMethodToPushDown hasNoShadowedInstVarsIn: aMethodToPushDown methodClass subclasses
		! !


!PushDownMethodToOneSubclassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 5/29/2023 16:02:24'!
createRefactoring
	
	^self refactoringClass for: methodToMove to: subclassToPushDownTo
	! !

!PushDownMethodToOneSubclassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 5/29/2023 11:10:45'!
refactoringClass

	^PushDownMethodToOneSubclass! !


!PushDownMethodToSubclassesApplier methodsFor: 'refactoring - creation' stamp: 'HAW 5/29/2023 11:08:44'!
refactoringClass

	^PushDownMethodToSubclasses! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 5/29/2023 16:12:52'!
messageListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		5.
				#itemOrder 		-> 		10.
				#label 			-> 		RefactoringMenues refactoringsLabel.
				#selector 		-> 		#yourself.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'inline method... (3)'.
				#selector 		-> 		#inlineMethod.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		20.
				#label 			-> 		'move to instance/class method'.
				#selector 		-> 		#moveToInstanceOrClassMethod.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		30.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		40.
				#label 			-> 		'push down to subclasses'.
				#selector 		-> 		#pushDownSelectorToSubclasses.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
			{
				#submenuOf -> RefactoringMenues refactoringsLabel.
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down to one subclass'.
				#selector 		-> 		#pushDownSelectorToOneSubclass.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
		}`.
	! !

!methodRemoval: PushDownMethodToSubclasses class #assert:hasNoShadowedInstVarsIn: stamp: 'HAW 5/29/2023 11:23:27'!
PushDownMethodToSubclasses class removeSelector: #assert:hasNoShadowedInstVarsIn:!
!methodRemoval: PushDownMethodToSubclasses class #warnIfExistMethodToPushDownOnSubClass:existsOn: stamp: 'HAW 5/29/2023 11:15:48'!
PushDownMethodToSubclasses class removeSelector: #warnIfExistMethodToPushDownOnSubClass:existsOn:!
!methodRemoval: PushDownMethodToSubclasses class #assertHasNoShadowedInstVars:hasNoShadowedInstVarsIn: stamp: 'HAW 5/29/2023 11:22:56'!
PushDownMethodToSubclasses class removeSelector: #assertHasNoShadowedInstVars:hasNoShadowedInstVarsIn:!
!methodRemoval: PushDownMethodToSubclasses class #signalMethodCannotShadowInstVarOfSubclasses: stamp: 'HAW 5/29/2023 11:25:39'!
PushDownMethodToSubclasses class removeSelector: #signalMethodCannotShadowInstVarOfSubclasses:!
!methodRemoval: PushDownMethodToSubclasses class #warnIf:existsOn: stamp: 'HAW 5/29/2023 11:18:50'!
PushDownMethodToSubclasses class removeSelector: #warnIf:existsOn:!
!methodRemoval: PushDownMethodToSubclasses class #is:equivalentlyImplementedIn: stamp: 'HAW 5/29/2023 11:24:13'!
PushDownMethodToSubclasses class removeSelector: #is:equivalentlyImplementedIn:!
!methodRemoval: PushDownMethodToSubclasses class #errorMessageCanNotPushDownWithShadowedInstVarsOf: stamp: 'HAW 5/29/2023 11:25:48'!
PushDownMethodToSubclasses class removeSelector: #errorMessageCanNotPushDownWithShadowedInstVarsOf:!
!methodRemoval: PushDownMethodToSubclasses class #warnMessageAlreadyImplementedIn: stamp: 'HAW 5/29/2023 11:19:21'!
PushDownMethodToSubclasses class removeSelector: #warnMessageAlreadyImplementedIn:!
!methodRemoval: PushDownMethodToSubclasses class #addShadowedInstVarsOf:in:to: stamp: 'HAW 5/29/2023 11:23:48'!
PushDownMethodToSubclasses class removeSelector: #addShadowedInstVarsOf:in:to:!
!methodRemoval: PushDownMethodToSubclasses class #assertHasNoShadowedInstVars: stamp: 'HAW 5/29/2023 11:22:43'!
PushDownMethodToSubclasses class removeSelector: #assertHasNoShadowedInstVars:!
!methodRemoval: PushDownMethodToSubclasses class #warnIfExistMethodToPushDownOnSubClass: stamp: 'HAW 5/29/2023 11:15:31'!
PushDownMethodToSubclasses class removeSelector: #warnIfExistMethodToPushDownOnSubClass:!
!methodRemoval: PushDownMethodToSubclasses class #warningMesssageForMessageAlreadyImplementedIn: stamp: 'HAW 5/29/2023 11:19:13'!
PushDownMethodToSubclasses class removeSelector: #warningMesssageForMessageAlreadyImplementedIn:!
!methodRemoval: PushDownMethodToSubclasses #implementSubclassResponsibility stamp: 'HAW 5/29/2023 14:49:28'!
PushDownMethodToSubclasses removeSelector: #implementSubclassResponsibility!
!methodRemoval: PushDownMethodToSubclasses #subclassResponsibilitySourceCode stamp: 'HAW 5/29/2023 14:49:02'!
PushDownMethodToSubclasses removeSelector: #subclassResponsibilitySourceCode!
!methodRemoval: PushDownMethodToOneSubclass class #signalMethodCannotShadowInstVarOfSubclasses: stamp: 'HAW 5/29/2023 11:25:39'!
PushDownMethodToOneSubclass class removeSelector: #signalMethodCannotShadowInstVarOfSubclasses:!
!methodRemoval: PushDownMethodToOneSubclass class #errorMessageCanNotPushDownWithShadowedInstVarsOf: stamp: 'HAW 5/29/2023 11:25:48'!
PushDownMethodToOneSubclass class removeSelector: #errorMessageCanNotPushDownWithShadowedInstVarsOf:!
!methodRemoval: PushDownMethodToOneSubclass class #warnMessageAlreadyImplementedIn: stamp: 'HAW 5/29/2023 11:19:21'!
PushDownMethodToOneSubclass class removeSelector: #warnMessageAlreadyImplementedIn:!
!methodRemoval: PushDownMethodToOneSubclass class #addShadowedInstVarsOf:in:to: stamp: 'HAW 5/29/2023 11:23:48'!
PushDownMethodToOneSubclass class removeSelector: #addShadowedInstVarsOf:in:to:!
!methodRemoval: PushDownMethodToOneSubclass class #assertIsValidToPushDownMethod: stamp: 'HAW 5/29/2023 11:35:09'!
PushDownMethodToOneSubclass class removeSelector: #assertIsValidToPushDownMethod:!
!methodRemoval: PushDownMethodToOneSubclass class #warningMesssageForMessageAlreadyImplementedIn: stamp: 'HAW 5/29/2023 11:19:13'!
PushDownMethodToOneSubclass class removeSelector: #warningMesssageForMessageAlreadyImplementedIn:!
!methodRemoval: PushDownMethodToOneSubclass #initializeFor:to: stamp: 'HAW 5/29/2023 14:45:54'!
PushDownMethodToOneSubclass removeSelector: #initializeFor:to:!
!methodRemoval: BrowserWindow #pushDownMethodToSubclassSelector stamp: 'HAW 5/29/2023 16:12:52'!
BrowserWindow removeSelector: #pushDownMethodToSubclassSelector!
!methodRemoval: BrowserWindow #pushDownSelector stamp: 'HAW 5/29/2023 16:12:20'!
BrowserWindow removeSelector: #pushDownSelector!

!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' messageListKey:from: systemCatListKey:from: systemCatSingletonKey:from:)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter changeKeywordOrder inlineMethod moveToInstanceOrClassMethod pushDownInstanceVariable pushDownSelectorToOneSubclass pushDownSelectorToSubclasses pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('commands' findClass)
!

!classDefinition: #PushDownMethodToOneSubclass category: #'Tools-Refactoring'!
PushDownMethod subclass: #PushDownMethodToOneSubclass
	instanceVariableNames: 'subclassToPushDownTo implementSubclassResponsibility'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushDownMethodToOneSubclass reorganize!
('applying - private' moveMethod removeMethod)
('initialization' initializeFor:to:implementingSubclassResponsibility:)
!


!PushDownMethodToOneSubclass class reorganize!
('instance creation' for:to:)
('pre-conditions' assert:isSubclassOf: warnIf:isNotDefinedInOtherSubclassesBut: warnIfExistMethodToPushDownOnSubClass:)
('exceptions' errorMessage:isNotSubclassOf: signal:isNotSubclassOf: signalNotARefactoringBecauseNotImplementedIn:)
('warnings' warningMessageNotARefactoringBecauseNotImplementedIn:)
!


!PushDownMethodToSubclasses class reorganize!
('instance creation' for:)
('pre-conditions' assertHasSubclassesToPushDownMethodTo: assertIsValidToPushDownMethod:)
('exceptions' errorMessageNoSubclassToPushDownMethodTo signalNoSubclassToPushDownMethodTo)
!

!classDefinition: #PushDownMethodToOneSubclassApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushDownMethodToOneSubclassApplier
	instanceVariableNames: 'subclassToPushDownTo'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushDownMethodToOneSubclassApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' requestRefactoringParameters)
!


!PushDownMethodToSubclassesApplier reorganize!
('refactoring - creation' refactoringClass)
!


!RefactoringMenues class reorganize!
('editor menus' smalltalkEditorMenuOptions)
('browser menues' classListMenuOptions messageListMenuOptions refactoringsLabel)
('initialization' initialize)
('shortcuts' smalltalkEditorCmdShortcutsSpec)
!

