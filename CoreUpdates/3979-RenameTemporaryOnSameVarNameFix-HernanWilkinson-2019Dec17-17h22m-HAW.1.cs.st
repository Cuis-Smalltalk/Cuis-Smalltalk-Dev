'From Cuis 5.0 [latest update: #3978] on 17 December 2019 at 7:20:58 pm'!
!classDefinition: #RenameTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameTemporaryApplier
	instanceVariableNames: 'classToRefactor oldVariable newVariable smalltalkEditor methodNode oldVariableNode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!classDefinition: #RenameTemporaryApplier category: #'Tools-Refactoring'!
Refactoring subclass: #RenameTemporary
	instanceVariableNames: 'oldVariableNode newVariable methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 12/17/2019 19:12:33'!
hasArgumentOrTemporaryNamed: aVariable

	| methodNode |

	methodNode := self methodNode.

	^methodNode tempNames includes: aVariable ! !


!BlockNode methodsFor: 'testing' stamp: 'HAW 12/17/2019 19:14:02'!
isArgumentOrTemporary: aParseNode

	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	! !


!MethodNode methodsFor: 'testing' stamp: 'HAW 12/17/2019 19:12:33'!
hasArgumentOrTemporaryNamed: aVariable

	^self tempNames includes: aVariable! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 12/17/2019 19:13:21'!
isArgumentOrTemporary: aParseNode

	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/17/2019 18:33:53'!
renameTemporary: aTemporaryNode at: aMethodNode

	self codeProvider isEditingMethod ifTrue: [ | applier |
		[ applier := RenameTemporaryApplier on: self for: aTemporaryNode at: aMethodNode ]
			on: SyntaxErrorNotification
			do: [:anError | ^self inform: (RenameTemporaryApplier errorMessageForCanNotParseMethod: anError) ].
		applier value ].
! !


!RenameTemporary methodsFor: 'initialization' stamp: 'HAW 12/17/2019 19:16:57'!
initializeFromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

	oldVariableNode := anOldVariableNode.
	newVariable := aNewVariable.
	methodNode := aMethodNode ! !


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/17/2019 19:14:24'!
assert: anOldVariableNode isPartOf: aMethodNode

	"I can not use tempNode becuase it uses scopeTable that does not have
	repeated nodes for variables with same name - Hernan"

	(aMethodNode isArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ].

	aMethodNode nodesDo: [ :aNode |
		aNode isBlockNode ifTrue: [
			(aNode isArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ]]].

	self signalOldVariableNodeNotPartOfMethodNode ! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/17/2019 18:52:51'!
assertIsArgOrTempNode: anOldVariableNode

	anOldVariableNode isTempOrArg ifFalse: [ self signalOldVariableNodeMustBeArgOrTempNodeErrorDescription ]! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 12/17/2019 18:53:23'!
oldVariableNodeMustBeArgOrTempNodeErrorDescription

	^'Old variable node must be argument or temporary node'! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 12/17/2019 18:49:53'!
oldVariableNodeNotPartOfMethodNodeErrorDescription

	^'Node of variable to rename is not part of method''s method node'! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 12/17/2019 18:53:03'!
signalOldVariableNodeMustBeArgOrTempNodeErrorDescription

	self refactoringError: self oldVariableNodeMustBeArgOrTempNodeErrorDescription! !

!RenameTemporary class methodsFor: 'exceptions' stamp: 'HAW 12/17/2019 18:49:23'!
signalOldVariableNodeNotPartOfMethodNode

	self refactoringError: self oldVariableNodeNotPartOfMethodNodeErrorDescription! !

!RenameTemporary class methodsFor: 'instance creation' stamp: 'HAW 12/17/2019 19:16:57'!
fromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

	|  trimmedNewVariable |

	trimmedNewVariable := aNewVariable withBlanksTrimmed.

	self assertIsArgOrTempNode: anOldVariableNode.
	self assert: anOldVariableNode isPartOf: aMethodNode.
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: aMethodNode.

	self assert: trimmedNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aMethodNode methodClass.

	^self new initializeFromOldVariableNode: anOldVariableNode to: trimmedNewVariable in: aMethodNode
! !


!RenameTemporaryApplier methodsFor: 'initialization' stamp: 'HAW 12/17/2019 18:41:46'!
initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := aMethodNode.
	oldVariableNode := aTemporaryNode.
	! !


!RenameTemporaryApplier class methodsFor: 'instance creation' stamp: 'HAW 12/17/2019 18:34:06'!
on: aSmalltalkEditor for: aTemporaryNode at: aMethodNode

	^self new initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 12/17/2019 19:12:33'!
methodsWithArgumentOrTemporaryNamed: instVarName

	^self methodsSelect: [:aMethod | aMethod hasArgumentOrTemporaryNamed: instVarName ]! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/17/2019 18:31:46'!
contextualRenameOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifChangeSelectorCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]


! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/17/2019 18:33:32'!
rename: aNodeUnderCursor in: aSelectedClass at: aMethodNode

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor at: aMethodNode ].

	self ifChangeSelectorCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [
			^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aMethodNode selector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [
			^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | variableName |
			variableName := aNodeUnderCursor key key.
			(Smalltalk classNamed: variableName) ifNotNil: [ :classToRename |
				^self renameClassOn: self codeProvider for: classToRename ].
			(Smalltalk bindingOf: variableName) ifNotNil: [
				^self renameGlobalOn: self codeProvider for: variableName ] ].

		^morph flash ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/17/2019 18:38:47'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	"I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan"
	selectedClass := [ self codeProvider selectedClassOrMetaClass ] on: Error do: [ :anError | anError return: UndefinedObject ].
	[ methodNode := selectedClass methodNodeFor: model actualContents asString ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !


!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 12/17/2019 19:12:33'!
addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames

	| shadowedInstVarNames |

	shadowedInstVarNames := subclass instVarNames select: [:instVarName |
		(aMethodToPushDown hasArgumentOrTemporaryNamed: instVarName) ].

	 shadowedInstVarNames ifNotEmpty: [
		subclassesWithShadowedInstVarNames at: subclass put: shadowedInstVarNames ]! !


!RenameTemporary methodsFor: 'applying' stamp: 'HAW 12/17/2019 18:23:43'!
apply

	| newSource ranges |

	ranges := methodNode rangeForNode: oldVariableNode ifAbsent: [ #() ].
	newSource := methodNode sourceText copyReplacing: ranges with: newVariable.

	^ newSource! !


!RenameTemporary class methodsFor: 'instance creation - private' stamp: 'HAW 12/17/2019 19:16:37'!
from: anOldVariable to: aNewVariable in: aMethodNode

	| oldVariableNode |

	"I keept this message for testing only, the applier now uses the one that receives the
	old variable node, that fixes the problem when renaming a temp that is in more than
	one block - Hernan"
	oldVariableNode := aMethodNode tempNodes
		detect: [ :aTempNode | aTempNode name = anOldVariable ]
		ifNone: [ self signalTemporaryVariable: anOldVariable notDefinedIn: aMethodNode ].

	^self fromOldVariableNode: oldVariableNode to: aNewVariable in: aMethodNode ! !


!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/17/2019 18:41:40'!
askNewVariableName

	newVariable := (self request: 'Enter new name:' initialAnswer: oldVariableNode name) withBlanksTrimmed ! !

!RenameTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 12/17/2019 18:40:46'!
requestRefactoringParameters

	self askNewVariableName! !

!RenameTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'HAW 12/17/2019 19:15:28'!
createRefactoring

	^RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode
	! !

!methodRemoval: RenameTemporaryApplier class #on:for: stamp: 'HAW 12/17/2019 18:34:10'!
RenameTemporaryApplier class removeSelector: #on:for:!
!methodRemoval: RenameTemporaryApplier #selectTemporaryVariableFrom: stamp: 'HAW 12/17/2019 18:41:06'!
RenameTemporaryApplier removeSelector: #selectTemporaryVariableFrom:!
!methodRemoval: RenameTemporaryApplier #chooseTemporaryVariable stamp: 'HAW 12/17/2019 18:40:53'!
RenameTemporaryApplier removeSelector: #chooseTemporaryVariable!
!methodRemoval: RenameTemporaryApplier #noTemporaryToRename stamp: 'HAW 12/17/2019 18:41:01'!
RenameTemporaryApplier removeSelector: #noTemporaryToRename!
!methodRemoval: RenameTemporaryApplier #initializeOn:for: stamp: 'HAW 12/17/2019 18:34:32'!
RenameTemporaryApplier removeSelector: #initializeOn:for:!
!methodRemoval: RenameTemporaryApplier #is:temporaryVariableFrom: stamp: 'HAW 12/17/2019 18:40:59'!
RenameTemporaryApplier removeSelector: #is:temporaryVariableFrom:!
!methodRemoval: RenameTemporaryApplier #chooseTemporaryVariableFrom: stamp: 'HAW 12/17/2019 18:40:55'!
RenameTemporaryApplier removeSelector: #chooseTemporaryVariableFrom:!
!methodRemoval: RenameTemporary class #fromNodeOfOld:to:in: stamp: 'HAW 12/17/2019 19:15:28'!
RenameTemporary class removeSelector: #fromNodeOfOld:to:in:!
!methodRemoval: RenameTemporary #initializeFrom:to:in: stamp: 'HAW 12/17/2019 18:21:53'!
RenameTemporary removeSelector: #initializeFrom:to:in:!
!methodRemoval: RenameTemporary #initializeFromNodeOfOld:to:in: stamp: 'HAW 12/17/2019 19:16:57'!
RenameTemporary removeSelector: #initializeFromNodeOfOld:to:in:!
!methodRemoval: SmalltalkEditor #renameTemporary: stamp: 'HAW 12/17/2019 18:33:32'!
SmalltalkEditor removeSelector: #renameTemporary:!
!methodRemoval: MethodNode #hasArgumentOrTemporary: stamp: 'HAW 12/17/2019 19:12:33'!
MethodNode removeSelector: #hasArgumentOrTemporary:!
!methodRemoval: CompiledMethod #hasArgumentOrTemporary: stamp: 'HAW 12/17/2019 19:12:33'!
CompiledMethod removeSelector: #hasArgumentOrTemporary:!

!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allUnreferencedInstanceVariables allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
!


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
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifChangeSelectorCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
!


!PushDownMethod class reorganize!
('instance creation' for:)
('pre-conditions' addShadowedInstVarsOf:in:to: assertIsValidToPushDownMethod: warnIfExistMethodToPushDownOnSubClass:)
('exceptions' errorMessageCanNotPushDownWithShadowedInstVarsOf: signalMethodCannotShadowInstVarOfSubclasses:)
('warnings' warnMessageAlreadyImplementedIn: warningMesssageForMessageAlreadyImplementedIn:)
!


!RenameTemporary reorganize!
('applying' apply methodNodeAfterApply)
('initialization' initializeFromOldVariableNode:to:in:)
!


!RenameTemporary class reorganize!
('pre-conditions' assert:isDefinedIn: assert:isNotDefinedAsInstanceVariableInHierarchyOf: assert:isNotDefinedIn: assert:isPartOf: assertIsArgOrTempNode: assertIsNotEmpty: assertIsValidVariableName:)
('exceptions' errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: errorMessageForTemporaryVariable:notDefinedIn: newVariableCanNotBeEmptyErrorMessage oldVariableNodeMustBeArgOrTempNodeErrorDescription oldVariableNodeNotPartOfMethodNodeErrorDescription signalInvalidTemporaryVariable: signalNewTemporaryVariable:isAlreadyDefinedIn: signalNewVariableCanNotBeEmpty signalOldVariableNodeMustBeArgOrTempNodeErrorDescription signalOldVariableNodeNotPartOfMethodNode signalTemporaryVariable:notDefinedIn:)
('instance creation' fromOldVariableNode:to:in:)
('warnings' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: signal:canNotHideInstanceVariableDefinedIn:)
('instance creation - private' from:to:in:)
!

!classDefinition: #RenameTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameTemporaryApplier
	instanceVariableNames: 'classToRefactor newVariable smalltalkEditor methodNode oldVariableNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!classDefinition: #RenameTemporaryApplier category: #'Tools-Refactoring'!
Refactoring subclass: #RenameTemporary
	instanceVariableNames: 'oldVariableNode newVariable methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameTemporaryApplier reorganize!
('refactoring - parameters request' askNewVariableName requestRefactoringParameters)
('refactoring - creation' createRefactoring)
('initialization' initializeOn:for:at:)
('refactoring - changes' showChanges)
!
