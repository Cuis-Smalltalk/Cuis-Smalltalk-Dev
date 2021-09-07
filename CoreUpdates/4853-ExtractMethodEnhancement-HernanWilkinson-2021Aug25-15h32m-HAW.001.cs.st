'From Cuis 5.0 [latest update: #4809] on 5 September 2021 at 10:49:47 pm'!

Smalltalk removeClassNamed: #ExtractMethodApplier!
Smalltalk removeClassNamed: #ExtractMethod!

!classDefinition: #ExtractMethodMessageSet category: #'Tools-Refactoring'!
MessageSet subclass: #ExtractMethodMessageSet
	instanceVariableNames: 'finder selectedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!classDefinition: #ExtractMethodReplacementsWindow category: #'Tools-Refactoring'!
MessageSetWindow subclass: #ExtractMethodReplacementsWindow
	instanceVariableNames: 'applier finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'intervalToExtract sourceMethod replacements newMessage sourceCodeToExtract sizeToExtract'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!classDefinition: #ExtractMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethod
	instanceVariableNames: 'extractMethodNewMethod collectionOfReplacements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethod commentStamp: '<historical>' prior: 0!
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.!

!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'intervalToExtract categoryOfNewSelector newMessage extractedSourceCode existingMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodNewMethod commentStamp: '<historical>' prior: 0!
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.!

!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom callingExpression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodReplacement commentStamp: '<historical>' prior: 0!
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.!

!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider sourceCodeToExtract newMethodRefactoring finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodMessageSet methodsFor: 'initialization' stamp: 'HAW 9/5/2021 07:15:36'!
initialize

	selectedIndex := 0.
	super initialize ! !

!ExtractMethodMessageSet methodsFor: 'initialization' stamp: 'HAW 9/5/2021 00:12:23'!
initializeFinder: aFinder 
	
	finder := aFinder.! !

!ExtractMethodMessageSet methodsFor: 'message list' stamp: 'HAW 9/5/2021 07:15:16'!
messageListIndex

	^selectedIndex ! !

!ExtractMethodMessageSet methodsFor: 'message list' stamp: 'HAW 9/5/2021 07:14:51'!
messageListIndex: anIndex

	selectedIndex := anIndex.
	^super messageListIndex: anIndex ! !

!ExtractMethodMessageSet methodsFor: 'message list' stamp: 'HAW 9/5/2021 21:52:26'!
removeMessageFromBrowserKeepingLabel

	| newIndex |

	selectedMessage ifNil: [ ^nil ].
	messageList removeIndex: selectedIndex.
	finder removeReplacementAt: selectedIndex.
	self changed: #messageList.
	
	newIndex := selectedIndex > messageList size 
		ifTrue: [ selectedIndex - 1 ]
		ifFalse: [ selectedIndex ].
	self messageListIndex: newIndex.! !

!ExtractMethodMessageSet methodsFor: 'source code ranges' stamp: 'HAW 9/5/2021 21:46:48'!
messageSendsRangesOf: aSelector
	
	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^Array with: replacement intervalToExtract 
	! !


!ExtractMethodMessageSet class methodsFor: 'instance creation' stamp: 'HAW 9/5/2021 00:11:22'!
finder: aFinder 
	
	^(self messageList: aFinder methodsToReplace) initializeFinder: aFinder! !


!MethodNode methodsFor: 'source ranges' stamp: 'HAW 8/26/2021 15:57:14'!
definitionStartPosition
	
	"It does not includes temp definition because the extract can include temps - Hernan"
	^self selectorLastPosition + 1! !


!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 9/5/2021 20:41:28'!
closeAfter: aBlock

	aBlock value.
	self whenUIinSafeState: [ self delete ].
	! !

!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 9/5/2021 20:41:16'!
extractAllInClass
	
	self closeAfter: [ applier valueWithMethodsInClass ].
	! !

!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 9/5/2021 20:41:43'!
extractInMethodOnly

	self closeAfter: [ applier valueWithSourceMethod ].
	! !

!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 9/5/2021 20:41:52'!
extractSelectionOnly
	
	self closeAfter: [ applier valueWithOriginalSelection ].
	! !

!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 9/5/2021 20:42:00'!
refactor
	
	self closeAfter: [ applier valueWithAllReplacements ].
	! !

!ExtractMethodReplacementsWindow methodsFor: 'actions' stamp: 'HAW 8/25/2021 22:07:31'!
remove
	
	model removeMessageFromBrowserKeepingLabel! !

!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'HAW 8/25/2021 22:07:31'!
addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10! !

!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'HAW 9/5/2021 20:02:21'!
addButtonsTo: row color: buttonColor

	self 
		addButton: self createRemoveButton to: row color: buttonColor;
		addButton: self createRefactorButton to: row color: buttonColor;
		addButton: self createExtractSelectionOnlyButton to: row color: buttonColor;
		addButton: self createExtractInMethodOnlyButton to: row color: buttonColor;
		addButton: self createExtractAllInClassButton to: row color: buttonColor;
		addButton: self createCancelButton to: row color: buttonColor.
! !

!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'HAW 8/25/2021 22:07:31'!
buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!ExtractMethodReplacementsWindow methodsFor: 'GUI building' stamp: 'HAW 8/25/2021 22:07:31'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 8/25/2021 22:07:31'!
createCancelButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #delete
		label: 'Cancel'.
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 19:55:47'!
createExtractAllInClassButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractAllInClass
		label: 'In Class'! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 19:55:57'!
createExtractInMethodOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractInMethodOnly
		label: 'In Method'! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 19:43:25'!
createExtractSelectionOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractSelectionOnly
		label: 'Selection Only'! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 19:56:07'!
createRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 20:07:00'!
createRemoveButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #remove
		label: 'Remove'.
! !

!ExtractMethodReplacementsWindow methodsFor: 'initialization' stamp: 'HAW 9/4/2021 23:30:56'!
initializeFrom: anExtractMethodApplier with: aFinder

	applier := anExtractMethodApplier.
	finder := aFinder ! !


!ExtractMethodReplacementsWindow class methodsFor: 'instance creation' stamp: 'HAW 9/5/2021 20:31:15'!
openFrom: anExtractMethodApplier with: aFinder

	| window messageSet |

	messageSet := ExtractMethodMessageSet finder: aFinder.
	"I have to set a autoSelectString even if I do not use it because if not the 
	autoSelect event is not triggered - Hernan"
	messageSet autoSelectString: aFinder sourceCodeToExtract.
	
	window := self open: messageSet label: 'Select replacements'. 
	window initializeFrom: anExtractMethodApplier with: aFinder.
	
	^window 

! !


!ExtractMethodReplacementsFinder methodsFor: 'initialization' stamp: 'HAW 9/2/2021 17:31:19'!
initializeOfCodeIn: anIntervalToExtract at: aMethod to: aNewMessage
	
	intervalToExtract := anIntervalToExtract.
	sourceMethod := aMethod.
	newMessage := aNewMessage ! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 9/5/2021 22:05:15'!
addReplacementAt: foundIntervalToExtract in: aMethod
	
	"If ther is an error creating the refactoring, then the found text is not extractable and 
	therefore should not be replaced - Hernan"
	[ replacements add: (self createReplacementAt: foundIntervalToExtract in: aMethod) ] 
		on: RefactoringError 
		do: [ :anError | ].! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 9/4/2021 21:03:06'!
createReplacementAt: foundIntervalToExtract in: aMethod
	
	^ExtractMethodReplacement 
		fromInterval: foundIntervalToExtract asSourceCodeInterval 
		of: aMethod  
		to: newMessage! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 9/4/2021 21:01:06'!
findReplacementsAt: aClass

	aClass methodsDo: [ :aMethod | self findReplacementsIn: aMethod asMethodReference ]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 9/4/2021 21:04:49'!
findReplacementsIn: aMethod

	| sourceCode foundIntervalToExtract sourceCodeToExtractStart |
	
	sourceCode := aMethod sourceCode.
	sourceCodeToExtractStart := 1.
	
	[ sourceCodeToExtractStart := sourceCode indexOfSubCollection: sourceCodeToExtract startingAt: sourceCodeToExtractStart.
	sourceCodeToExtractStart ~= 0 ] whileTrue: [ 
		foundIntervalToExtract := sourceCodeToExtractStart to: sourceCodeToExtractStart + sizeToExtract.
		self addReplacementAt: foundIntervalToExtract in: aMethod.
		sourceCodeToExtractStart := foundIntervalToExtract last + 1 ] 
		
	 ! !

!ExtractMethodReplacementsFinder methodsFor: 'testing' stamp: 'HAW 9/4/2021 23:25:35'!
hasOneReplacement
	
	^replacements size = 1! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 20:18:47'!
inClassReplacements
	
	^replacements select: [ :aReplacement | aReplacement isAt: sourceMethod methodClass ]! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 00:12:03'!
methodsToReplace
	
	^replacements collect: [ :aReplacement | aReplacement methodToExtractFrom ]! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 20:14:49'!
originalSelectionReplacement
	
	^ExtractMethodReplacement fromInterval: intervalToExtract of: sourceMethod to: newMessage ! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 21:51:40'!
removeReplacementAt: anIndex

	^replacements removeAt: anIndex ! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/2/2021 17:41:27'!
replacements
	
	^replacements ! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 07:24:58'!
replacementsAt: anIndex ifAbsent: ifAbsentBlock
 
	^replacements at: anIndex ifAbsent: ifAbsentBlock ! !

!ExtractMethodReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 9/5/2021 20:17:31'!
sourceMethodReplacements
	
	^replacements select: [:aReplacement | aReplacement isOf: sourceMethod ]! !

!ExtractMethodReplacementsFinder methodsFor: 'evaluating' stamp: 'HAW 9/4/2021 21:00:59'!
value

	sourceCodeToExtract := sourceMethod sourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	sizeToExtract := intervalToExtract size - 1.
	replacements := OrderedCollection new.
	
	sourceMethod methodClass withAllSubclassesDo: [ :aClass | self findReplacementsAt: aClass]
	 ! !

!ExtractMethodReplacementsFinder methodsFor: 'source code' stamp: 'HAW 9/5/2021 00:14:54'!
sourceCodeToExtract
	
	^sourceCodeToExtract! !


!ExtractMethodReplacementsFinder class methodsFor: 'instance creation' stamp: 'HAW 9/2/2021 17:31:33'!
ofCodeIn: anIntervalToExtract at: aMethod to: aNewMessage
	
	^self new initializeOfCodeIn: anIntervalToExtract at: aMethod to: aNewMessage ! !


!ExtractMethod methodsFor: 'initialization' stamp: 'HAW 9/5/2021 22:46:43'!
initializeNewDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements 
	
	extractMethodNewMethod := anExtractMethodNewMethod.
	collectionOfReplacements := aCollectionOfReplacements.! !

!ExtractMethod methodsFor: 'private - applying' stamp: 'HAW 9/5/2021 22:46:48'!
applyMethodReplacements: aMethodReplacements 
	
	| adjustment sortedReplacements |
	
	adjustment := 0.
	"This is not really necesary because the groupBy: keeps the order, but I do it just in case that is changed - Hernan"
	sortedReplacements := aMethodReplacements sorted: [ :leftReplacement :rightReplacement | leftReplacement isBefore: rightReplacement ].
	sortedReplacements do: [ :aReplacement | 
		aReplacement applyAdjusting: adjustment.
		adjustment := adjustment + aReplacement adjustmentForNextReplacement ]! !

!ExtractMethod methodsFor: 'private - applying' stamp: 'HAW 9/5/2021 22:46:51'!
applyReplacements
	
	| replacementsByMethod |
	
	replacementsByMethod := collectionOfReplacements groupBy: [ :aReplacement | aReplacement methodToExtractFrom ].
	replacementsByMethod valuesDo: [ :aMethodReplacements | self applyMethodReplacements: aMethodReplacements ].
	! !

!ExtractMethod methodsFor: 'private - applying' stamp: 'HAW 9/5/2021 22:46:54'!
createNewMethod
	
	extractMethodNewMethod apply.
! !

!ExtractMethod methodsFor: 'applying' stamp: 'HAW 9/5/2021 22:46:38'!
apply
	
	self 
		createNewMethod;
		applyReplacements ! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'HAW 9/5/2021 22:46:15'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	^self 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage 
			categorizedAs: aCategory ) 
		replacements: (Array with: (ExtractMethodReplacement 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage))
	
! !

!ExtractMethod class methodsFor: 'instance creation' stamp: 'HAW 9/5/2021 22:46:32'!
newDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements 
	
	^self new initializeNewDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements ! !


!ExtractMethodNewMethod methodsFor: 'applying' stamp: 'HAW 9/4/2021 15:55:33'!
apply

	self sourceClass
		compile: self newMethodSourceCode
		classified: categoryOfNewSelector! !

!ExtractMethodNewMethod methodsFor: 'initialization' stamp: 'HAW 9/2/2021 18:09:20'!
initializeExtractedSourceCode

	extractedSourceCode := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last! !

!ExtractMethodNewMethod methodsFor: 'initialization' stamp: 'HAW 9/2/2021 18:09:20'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage in: aCategory 

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage.
	categoryOfNewSelector := aCategory.
	self initializeExtractedSourceCode.! !

!ExtractMethodNewMethod methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 18:09:20'!
newMessageString

	^ newMessage fullName! !

!ExtractMethodNewMethod methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 18:09:20'!
newMethodSourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation;
			nextPutAll: self returnCharacterIfNeeded;
			nextPutAll: extractedSourceCode ]! !

!ExtractMethodNewMethod methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 18:09:20'!
returnCharacterIfNeeded

	| extractedMethodNode |
	
	extractedMethodNode := Parser parse: extractedSourceCode class: self sourceClass noPattern: true.
	
	^ (extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ])
		ifTrue: [ '' ] ifFalse: [ '^ ' ]! !

!ExtractMethodNewMethod methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 18:09:20'!
sourceClass

	^ existingMethod methodClass! !

!ExtractMethodNewMethod methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 18:09:20'!
startingMethodIdentation

	^ String lfString , String lfString , String tab! !


!ExtractMethodNewMethod class methodsFor: 'error messages' stamp: 'HAW 9/2/2021 18:09:20'!
noSelectionErrorMessage

	^ 'Please select some code for extraction'! !

!ExtractMethodNewMethod class methodsFor: 'error messages' stamp: 'HAW 9/2/2021 18:09:20'!
outOfBoundsSelectionErrorMessage

	^ 'The requested source code selection interval is out of bounds'! !

!ExtractMethodNewMethod class methodsFor: 'error messages' stamp: 'HAW 9/2/2021 18:09:20'!
wrongNumberOfArgumentsGivenErrorMessage

	^ 'The number of arguments in the given selector is not correct'! !

!ExtractMethodNewMethod class methodsFor: 'exceptions' stamp: 'HAW 9/2/2021 18:09:20'!
signalExtractMethodWithWrongNumberOfArgumentsError

	self refactoringError: self wrongNumberOfArgumentsGivenErrorMessage! !

!ExtractMethodNewMethod class methodsFor: 'exceptions' stamp: 'HAW 9/2/2021 18:09:20'!
signalNoSelectedCodeError

	self refactoringError: self noSelectionErrorMessage! !

!ExtractMethodNewMethod class methodsFor: 'exceptions' stamp: 'HAW 9/2/2021 18:09:20'!
signalOutOfBoundsIntervalError

	self refactoringError: self outOfBoundsSelectionErrorMessage! !

!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'HAW 9/2/2021 18:09:20'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	| trimmedIntervalToExtract |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !

!ExtractMethodNewMethod class methodsFor: 'pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract

	self
		assertIntervalToExtractIsNotEmpty: anIntervalToExtract;
		assert: anIntervalToExtract isWithinBoundsOf: aMethodToExtractCodeFrom sourceCode;
		assert: aMethodToExtractCodeFrom containsValidCodeToBeExtractedAt: anIntervalToExtract! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assert: aSelector canBeDefinedIn: aClass

	NewSelectorPrecondition valueFor: aSelector on: aClass! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assert: aMethodToRefactor containsValidCodeToBeExtractedAt: anIntervalToExtract

	SourceCodeOfMethodToBeExtractedPrecondition valueFor: anIntervalToExtract of: aMethodToRefactor! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor

	| parseNodesToParameterize |
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract.
	newMessage arguments size = parseNodesToParameterize size
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assert: anIntervalToExtract isWithinBoundsOf: sourceCode

	(self is: anIntervalToExtract withinBoundsOf: sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ]! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
assertIntervalToExtractIsNotEmpty: anIntervalToExtract

	(self isNotEmpty: anIntervalToExtract)
		ifFalse: [ self signalNoSelectedCodeError ]! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
is: anIntervalToExtract withinBoundsOf: aSourceCode

	^ anIntervalToExtract first >= 1 and: [ anIntervalToExtract last <= aSourceCode size ]! !

!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 9/2/2021 18:09:20'!
isNotEmpty: anInterval 

	^ anInterval first <= anInterval last! !


!ExtractMethodReplacement methodsFor: 'applying' stamp: 'HAW 9/2/2021 17:38:51'!
apply

	self sourceClass
		compile: self updatedSourceCodeOfExistingMethod
		classified: methodToExtractFrom category! !

!ExtractMethodReplacement methodsFor: 'applying' stamp: 'HAW 9/4/2021 20:59:16'!
applyAdjusting: anAdjustment

	intervalToExtract := (intervalToExtract + anAdjustment) asSourceCodeInterval.
	self apply ! !

!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'HAW 9/4/2021 16:53:14'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage

	intervalToExtract := anIntervalToExtract.
	methodToExtractFrom := aMethodToExtractCodeFrom.
	newMessage := 	aNewMessage.
	self initializeCallingExpression ! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 9/4/2021 16:31:47'!
initializeCallingExpression

	callingExpression := 'self ', self newMessageString.
	self shouldBeEnclosedWithParens ifTrue: [ callingExpression := '(' , callingExpression , ')' ]
		! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 17:06:21'!
newMessageString

	^ newMessage fullName! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 17:38:51'!
shouldBeEnclosedWithParens

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode _ methodToExtractFrom methodNode.
	initialNodeAncestors  _ methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors _ methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon _ initialNodeAncestors intersection: finalNodeAncestors.

	initialNode _ (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode _ (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions _ initialNode isMessageNode and: [ finalNode isMessageNode ].
	
	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 9/2/2021 17:38:51'!
sourceClass

	^ methodToExtractFrom methodClass! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 9/4/2021 16:37:55'!
updatedSourceCodeOfExistingMethod

	^ methodToExtractFrom sourceCode
		copyReplaceFrom: intervalToExtract first  
		to: intervalToExtract last 
		with: callingExpression! !

!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'HAW 9/2/2021 17:36:44'!
intervalToExtract
	
	^intervalToExtract! !

!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'HAW 9/2/2021 17:38:51'!
methodToExtractFrom
	
	^methodToExtractFrom ! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'HAW 9/4/2021 20:28:45'!
isAt: aClass 
	
	^methodToExtractFrom methodClass = aClass ! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'HAW 9/4/2021 17:02:06'!
isBefore: anExtractMethodReplacement 
	
	^anExtractMethodReplacement startsAfter: intervalToExtract first! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'HAW 9/2/2021 18:06:56'!
isOf: aMethod

	^methodToExtractFrom = aMethod ! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'HAW 9/4/2021 17:02:34'!
startsAfter: aPosition

	^intervalToExtract first > aPosition ! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'HAW 9/4/2021 16:50:17'!
adjustmentForNextReplacement
	
	^callingExpression size - intervalToExtract size! !


!ExtractMethodReplacement class methodsFor: 'instance creation' stamp: 'HAW 9/2/2021 17:26:03'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage 

	| trimmedIntervalToExtract |
	
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		! !

!ExtractMethodReplacement class methodsFor: 'pre-conditions' stamp: 'HAW 9/5/2021 22:07:25'!
assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract

	ExtractMethodNewMethod assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract
! !


!RefactoringApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/26/2021 16:06:07'!
createAndSetRefactoringHandlingRefactoringExceptions: aCreatorBlock

	self valueHandlingRefactoringExceptions: [ refactoring := aCreatorBlock value]
	! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'HAW 9/5/2021 22:47:39'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.
	newMessageArguments := Dictionary new! !

!ExtractMethodApplier methodsFor: 'refactoring - changes' stamp: 'HAW 9/5/2021 22:47:43'!
showChanges

	codeProvider currentMethodRefactored! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:47:49'!
createRefactoring
	
	^ self shouldNotImplement! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:47:53'!
createRefactoringForMethodsInClass
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder inClassReplacements ! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:47:57'!
createRefactoringForOriginalSelection
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements:  { finder originalSelectionReplacement }! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:48:02'!
createRefactoringForSourceMethod
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder sourceMethodReplacements ! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:48:06'!
createRefactoringWithAllReplacements
	
	^ self refactoringClass newDefinition: newMethodRefactoring replacements: finder replacements ! !

!ExtractMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/5/2021 22:48:10'!
refactoringClass

	^ ExtractMethod! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/5/2021 22:48:14'!
createExtractMethodNewMethodFor: newMessage

	^ newMethodRefactoring := ExtractMethodNewMethod 
		fromInterval: intervalToExtract
		of: methodToExtractCodeFrom
		to: newMessage
		categorizedAs: methodToExtractCodeFrom category! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/5/2021 22:48:17'!
findReplacementsWith: newMessage

	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: methodToExtractCodeFrom to: newMessage.
	finder value! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/5/2021 22:48:20'!
requestNewMessage

	| parseNodesToParameterize initialAnswer userAnswer  |
	
	parseNodesToParameterize := self parseNodesToParameterize.
	initialAnswer := self buildInitialSelectorAnswer: parseNodesToParameterize.
	userAnswer := self request: 'New method name:' initialAnswer: initialAnswer.

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ].
	
	^self buildNewMessage.
	! !

!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/5/2021 22:48:24'!
requestRefactoringParameters

	| newMessage |
	
	newMessage := self requestNewMessage.
	self createExtractMethodNewMethodFor: newMessage.
	self findReplacementsWith: newMessage.
	
	finder hasOneReplacement 
		ifTrue: [ self valueWithAllReplacements ]
		ifFalse: [ ExtractMethodReplacementsWindow openFrom: self with: finder ]
		! !

!ExtractMethodApplier methodsFor: 'private - new message' stamp: 'HAW 9/5/2021 22:48:29'!
buildNewMessage

	^ Message
		selector: newSelector
		arguments: self newMessageArgumentNames! !

!ExtractMethodApplier methodsFor: 'private - new message' stamp: 'HAW 9/5/2021 22:48:32'!
newMessageArgumentNames

	^ newMessageArguments values collect: [ :parseNode | parseNode name ]! !

!ExtractMethodApplier methodsFor: 'private - new message' stamp: 'HAW 9/5/2021 22:48:36'!
saveUnarySelector: userAnswer

	^ newSelector := userAnswer asSymbol! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:40'!
value

	requestExitBlock := [ ^self ].
	
	self requestRefactoringParametersHandlingRefactoringExceptions
! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:45'!
valueCreatingWith: aRefactoringCreationBlock

	self 
		createAndSetRefactoringHandlingRefactoringExceptions: aRefactoringCreationBlock;
		applyRefactoring;
		showChanges
	
	! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:48'!
valueWithAllReplacements

	self valueCreatingWith: [ self createRefactoringWithAllReplacements ]
	! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:51'!
valueWithMethodsInClass
	
	self valueCreatingWith: [ self createRefactoringForMethodsInClass ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:54'!
valueWithOriginalSelection
	
	self valueCreatingWith: [ self createRefactoringForOriginalSelection ]! !

!ExtractMethodApplier methodsFor: 'value' stamp: 'HAW 9/5/2021 22:48:58'!
valueWithSourceMethod
	
	self valueCreatingWith: [ self createRefactoringForSourceMethod ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:03'!
buildInitialSelectorAnswer: parseNodesToParameterize
	"builds a selector with the shape of #m1 if unary, or #m1: something m2: else if it has args"

	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: 'm1' ]
		ifNotEmpty: [ parseNodesToParameterize
			inject: ''
			into: [ :partialSelector :parseNode |
				| currentKeyword |
				currentKeyword _ 'm' , (parseNodesToParameterize indexOf: parseNode) asString , ': '.
				partialSelector
				, (self formatAsKeyword: currentKeyword)
				, (self formatAsMethodArgument: parseNode name)
				, String newLineString ] ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:06'!
formatAsKeyword: aKeyword

	^ Text
		string: aKeyword
		attributes: (SHTextStylerST80 attributesFor: #patternKeyword)! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:10'!
formatAsMethodArgument: aMethodArgumentName

	^ Text
		string: aMethodArgumentName
		attributes: (SHTextStylerST80 attributesFor: #methodArg)! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:13'!
parseNodesToParameterize

	^ ExtractMethodParametersDetector
		valueFor: methodToExtractCodeFrom methodNode
		at: intervalToExtract! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:16'!
saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize

	self saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer.
	newSelector := ('' join: (self selectorTokensOf: userAnswer)) asSymbol.! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:20'!
saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer

	| newSelectorKeywords |
	newSelectorKeywords _ self selectorTokensOf: userAnswer.
	self validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords.
	parseNodesToParameterize withIndexDo: [ :parseNode :index |
		newMessageArguments at: (newSelectorKeywords at: index) put: parseNode ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:23'!
selectorTokensOf: userAnswer
	"this selects the pieces of strings before each $:"

	^ (userAnswer findTokens: ':') allButLast
		collect: [ :tok | (tok findTokens: Character separators) last , ':' ]! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:26'!
sourceCodeToExtract
	
	^sourceCodeToExtract! !

!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 9/5/2021 22:49:29'!
validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords

	newSelectorKeywords size = parseNodesToParameterize size
		ifFalse: [ ExtractMethodNewMethod signalExtractMethodWithWrongNumberOfArgumentsError ]! !


!ExtractMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 9/5/2021 22:47:32'!
on: aCodeProvider for: anIntervalToExtract of: aMethodToRefactor

	| trimmedIntervalToExtract sourceCode |
	
	sourceCode := aMethodToRefactor sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode..

	self assertCanApplyRefactoringOn: aMethodToRefactor at: trimmedIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: trimmedIntervalToExtract of: aMethodToRefactor! !

!ExtractMethodApplier class methodsFor: 'pre-conditions' stamp: 'HAW 9/5/2021 22:47:27'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	ExtractMethodNewMethod 
		assertCanApplyRefactoringOn: aMethodToRefactor
		at: anIntervalToExtract! !


!SmalltalkEditor methodsFor: 'extract method' stamp: 'HAW 9/5/2021 20:36:00'!
extractMethod

	self performCodeExtractionRefactoringWith: ExtractMethodApplier! !


!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 9/5/2021 20:07:07'!
createRemoveButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #remove
		label: 'Remove'.
! !


!AddInstanceVariable methodsFor: 'initialization' stamp: 'HAW 8/28/2021 17:44:36'!
initializeNamed: aNewVariable to: aClassToRefactor 
	
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor ! !


!RefactoringApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/26/2021 16:04:27'!
createRefactoringHandlingRefactoringExceptions

	self createAndSetRefactoringHandlingRefactoringExceptions: [ self createRefactoring ]
	! !


!SourceCodeIntervalPrecondition methodsFor: 'pre-conditions' stamp: 'HAW 8/26/2021 15:57:31'!
assertSourceCodeIsNotPartOfMethodSignature

	self intervalToExtractIncludesPartOfMethodSignature
		ifTrue: [ self signalExtractingPartOfMethodSignatureError ]! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'HAW 8/26/2021 15:56:33'!
methodDefinitionStartPosition

	^methodNode definitionStartPosition! !

!MethodReference methodsFor: 'decompiling' stamp: 'HAW 9/5/2021 23:06:10'!
methodNode
	
	^self compiledMethod methodNode! !

!CompiledMethod methodsFor: 'converting' stamp: 'HAW 9/5/2021 23:06:10'!
asMethodReference
	
	^MethodReference method: self! !

!methodRemoval: SourceCodeIntervalPrecondition #firstParseNodeOfMethodDefinition stamp: 'HAW 8/26/2021 15:56:28'!
SourceCodeIntervalPrecondition removeSelector: #firstParseNodeOfMethodDefinition!
!methodRemoval: ChangeSelectorWizardStepWindow #isMessageSelected stamp: 'HAW 9/5/2021 20:07:17'!
ChangeSelectorWizardStepWindow removeSelector: #isMessageSelected!

!ExtractMethodMessageSet reorganize!
('initialization' initialize initializeFinder:)
('message list' messageListIndex messageListIndex: removeMessageFromBrowserKeepingLabel)
('source code ranges' messageSendsRangesOf:)
!


!SmalltalkEditor reorganize!
('accessing-selection' selection)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt doItProfiling: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu: openSmalltalkEditorRefactoringMenu refactoringMenu)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
!


!ExtractMethodReplacementsWindow reorganize!
('actions' closeAfter: extractAllInClass extractInMethodOnly extractSelectionOnly refactor remove)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('button creation' createCancelButton createExtractAllInClassButton createExtractInMethodOnlyButton createExtractSelectionOnlyButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:with:)
!


!ExtractMethodReplacementsFinder reorganize!
('initialization' initializeOfCodeIn:at:to:)
('private-replacement finding' addReplacementAt:in: createReplacementAt:in: findReplacementsAt: findReplacementsIn:)
('testing' hasOneReplacement)
('replacements' inClassReplacements methodsToReplace originalSelectionReplacement removeReplacementAt: replacements replacementsAt:ifAbsent: sourceMethodReplacements)
('evaluating' value)
('source code' sourceCodeToExtract)
!


!ExtractMethod reorganize!
('initialization' initializeNewDefinition:replacements:)
('private - applying' applyMethodReplacements: applyReplacements createNewMethod)
('applying' apply)
!


!ExtractMethod class reorganize!
('instance creation' fromInterval:of:to:categorizedAs: newDefinition:replacements:)
!


!ExtractMethodReplacement reorganize!
('applying' apply applyAdjusting:)
('initialization' initializeFrom:of:to:)
('private - source code' initializeCallingExpression newMessageString shouldBeEnclosedWithParens sourceClass updatedSourceCodeOfExistingMethod)
('accessing' intervalToExtract methodToExtractFrom)
('testing' isAt: isBefore: isOf: startsAfter:)
('adjustment' adjustmentForNextReplacement)
!


!ExtractMethodReplacement class reorganize!
('instance creation' fromInterval:of:to:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
!


!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForMethodsInClass createRefactoringForOriginalSelection createRefactoringForSourceMethod createRefactoringWithAllReplacements refactoringClass)
('refactoring - parameters request' createExtractMethodNewMethodFor: findReplacementsWith: requestNewMessage requestRefactoringParameters)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('value' value valueCreatingWith: valueWithAllReplacements valueWithMethodsInClass valueWithOriginalSelection valueWithSourceMethod)
('private - parameterizing' buildInitialSelectorAnswer: formatAsKeyword: formatAsMethodArgument: parseNodesToParameterize saveBinaryOrKeywordSelector:withArguments: saveMessageArgumentsForEach:using: selectorTokensOf: sourceCodeToExtract validateRequiredParameters:haveACorrespondingKeywordIn:)
!

