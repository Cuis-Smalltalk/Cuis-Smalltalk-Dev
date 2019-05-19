'From Cuis 5.0 of 7 November 2016 [latest update: #3744] on 17 May 2019 at 1:38:30 pm'!
Smalltalk renameClassNamed: #LocalToInstanceVariable as: #TemporaryToInstanceVariable!
!classDefinition: #TemporaryToInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #TemporaryToInstanceVariable
	instanceVariableNames: 'variable method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #TemporaryToInstanceVariableApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #TemporaryToInstanceVariableApplier
	instanceVariableNames: 'smalltalkEditor classToRefactor methodNode variableName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'EB 5/17/2019 13:00:30'!
temporaryToInstanceVariable
	self withNodeUnderCursorDo: [ :nodeUnderCursor |
		nodeUnderCursor isTemp ifTrue: [
			TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name :: value 
		] ifFalse: [ morph flash ].
	] ifAbsent: [ morph flash ].! !

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'EB 5/17/2019 13:21:21'!
temporaryToInstanceVariable: aKeyboardEvent
	self temporaryToInstanceVariable.
	^true.! !

!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'EB 5/17/2019 12:53:43'!
withNodeUnderCursorDo: aDoBlock ifAbsent: anAbsentBlock
	self 
		withMethodNodeAndClassDo: [ :currentMethodNode :currentClass |
			currentMethodNode withParseNodeIncluding: self startIndex
				do: aDoBlock
				ifAbsent: anAbsentBlock.
		] ifErrorsParsing: [ :arg1 | anAbsentBlock value ].! !


!TemporaryToInstanceVariable methodsFor: 'initialization' stamp: 'EB 5/15/2019 22:52:40'!
initializeNamed: aTemporaryVariableName fromMethod: aMethodNode 
	variable _ aTemporaryVariableName.
	method _ aMethodNode.! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/16/2019 00:09:59'!
addInstanceVariable
	AddInstanceVariable named: variable to: method methodClass :: apply.! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/17/2019 13:08:04'!
apply
	| newSourceCode |
	newSourceCode _ self removeTemporary.
	self addInstanceVariable.
	^newSourceCode.! !

!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/17/2019 13:07:44'!
removeTemporary
	| temporaryVariablePositions newSourceCode variableDeclarationPosition |
	temporaryVariablePositions _ method positionsForTemporaryVariable: variable ifAbsent: [].
	variableDeclarationPosition _ {temporaryVariablePositions first}.
	newSourceCode _ method sourceText copyReplacing: variableDeclarationPosition with: ''.
	method methodClass compile: newSourceCode.
	^newSourceCode.
	! !


!TemporaryToInstanceVariable class methodsFor: 'instance creation' stamp: 'EB 5/17/2019 01:24:00'!
named: aTemporaryVariableName fromMethod: aMethodNode
	| methodClass |
	methodClass _ aMethodNode methodClass.
	
	self assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName.
	self assertNoOtherMethodIn: methodClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName. 
	self assertSubclassesOf: methodClass haventGotInstanceVariableNamed: aTemporaryVariableName.
	
	^self new initializeNamed: aTemporaryVariableName fromMethod: aMethodNode.! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'EB 5/16/2019 00:17:51'!
inexistentTemporaryErrorDescription
	^'The temporary variable does not exist.'! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'EB 5/16/2019 02:43:50'!
temporaryExistsAsInstVarInSubclassesErrorDescription
	^'The temporary variable exists as an instance variable in a subclass.'! !

!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'EB 5/16/2019 00:42:42'!
temporaryExistsInOtherMethodsErrorDescription
	^'Temporary variable exists in other methods; remove those first.'! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 5/17/2019 13:28:16'!
assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName
	aMethodNode hasLocalNamed: aTemporaryVariableName :: ifFalse: [
		self refactoringError: self inexistentTemporaryErrorDescription.
	].! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 5/17/2019 13:28:45'!
assertNoOtherMethodIn: aClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName
 	| methodsDefiningTemporaryInHierarchy |
	methodsDefiningTemporaryInHierarchy := OrderedCollection new.
	aClass withAllSubclassesDo: [ :subclass | 
		methodsDefiningTemporaryInHierarchy addAll: (subclass methodsWithArgumentOrTemporaryNamed: aTemporaryVariableName)
	].

	methodsDefiningTemporaryInHierarchy size > 1 ifTrue: [
		self refactoringError: self temporaryExistsInOtherMethodsErrorDescription.
	].! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 5/17/2019 13:29:19'!
assertSubclassesOf: aClass haventGotInstanceVariableNamed: anInstanceVariableName
	aClass allSubclassesDo: [ :subclass | 
		subclass instVarNames includes: anInstanceVariableName :: ifTrue: [
			self refactoringError: self temporaryExistsAsInstVarInSubclassesErrorDescription.
		].
	].! !


!TemporaryToInstanceVariableApplier methodsFor: 'initialization' stamp: 'EB 5/17/2019 00:59:52'!
initializeOn: aSmalltalkEditor for: aTemporaryVariableName
	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := classToRefactor methodNodeFor: smalltalkEditor actualContents string.
	variableName := aTemporaryVariableName 
	! !

!TemporaryToInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'EB 5/17/2019 00:58:32'!
createRefactoring
	^TemporaryToInstanceVariable named: variableName fromMethod: methodNode.! !

!TemporaryToInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'EB 5/17/2019 01:01:08'!
requestRefactoringParameters
	! !

!TemporaryToInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'EB 5/17/2019 13:08:22'!
showChanges
	smalltalkEditor actualContents: changes.
	! !


!TemporaryToInstanceVariableApplier class methodsFor: 'as yet unclassified' stamp: 'EB 5/17/2019 00:59:39'!
on: aSmalltalkEditor for: aTemporaryVariableName
	
	^self new initializeOn: aSmalltalkEditor for: aTemporaryVariableName! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'EB 5/17/2019 13:32:48'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 	-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Temporary to Instance Variable (P)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'EB 5/17/2019 13:21:27'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor')
		#(		$P	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable'))
! !

!methodRemoval: SmalltalkEditor #withNodeUnderCursorInside:do:ifAbsent:!
SmalltalkEditor removeSelector: #withNodeUnderCursorInside:do:ifAbsent:!

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
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifRenameCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
!


!TemporaryToInstanceVariable reorganize!
('initialization' initializeNamed:fromMethod:)
('applying' addInstanceVariable apply removeTemporary)
!


!TemporaryToInstanceVariable class reorganize!
('instance creation' named:fromMethod:)
('error descriptions' inexistentTemporaryErrorDescription temporaryExistsAsInstVarInSubclassesErrorDescription temporaryExistsInOtherMethodsErrorDescription)
('assertions' assert:hasTemporaryNamed: assertNoOtherMethodIn:orSubclassesDefinesTemporaryNamed: assertSubclassesOf:haventGotInstanceVariableNamed:)
!


!TemporaryToInstanceVariableApplier reorganize!
('initialization' initializeOn:for:)
('refactoring - creation' createRefactoring)
('refactoring - parameters request' requestRefactoringParameters)
('refactoring - changes' showChanges)
!

