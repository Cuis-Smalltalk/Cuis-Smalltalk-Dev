'From Cuis 5.0 [latest update: #3990] on 23 December 2019 at 11:14:21 am'!

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/23/2019 10:59:16'!
ifSourceCodeRefactoringCanBeAppliedDo: aBlock

	^(self hasUnacceptedEdits or: [morph hasEditingConflicts ])
		ifTrue: [ self inform: 'This refactoring can not be applied when there are unsaved changes' ]
		ifFalse: aBlock! !


!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 09:58:07'!
hasNodeIn: aCollectionOfNodes named: aName

	^aCollectionOfNodes anySatisfy: [ :tempNode | tempNode isNamed: aName ]! !


!BlockNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 09:59:19'!
hasArgumentOrTemporaryNamed: aName

	^ self hasLocallyArgumentOrTemporaryNamed: aName! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 12/23/2019 10:40:42'!
completeSourceRanges
	"Returns the 'expanded' version of the source ranges, for instance in message sends it also includes the receiver, and if there are parentheses they are included in the source range as well. Right now used for refactorings."

	^ encoder completeSourceRangesBasedOn: self sourceText! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 12/23/2019 10:40:45'!
completeSourceRangesDo: aBinaryBlock
	"block has to receive parse node and collection of source ranges"

	^ self completeSourceRanges keysAndValuesDo: aBinaryBlock! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 12/23/2019 10:41:00'!
parseNodesPathAt: aPosition ifAbsent: aBlockClosure

	^ encoder
		parseNodesPathAt: aPosition
		using: self completeSourceRanges
		ifAbsent: aBlockClosure! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 11:10:37'!
allParseNodesWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ]
				:: ifTrue: [ ^ false ]]].
	
	^ true! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/23/2019 10:59:16'!
contextualRenameInClassDefinition 

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/23/2019 10:59:16'!
contextualRenameOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]


! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/23/2019 10:59:16'!
rename: aNodeUnderCursor in: aSelectedClass at: aMethodNode

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor at: aMethodNode ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
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

!SmalltalkEditor methodsFor: 'contextual add/remove parameter' stamp: 'HAW 12/23/2019 10:59:16'!
contextualChangeSelectorUsing: aChangeSelectorApplier
	
	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self contextualChangeSelectorInMethodUsing: aChangeSelectorApplier ]]! !

!SmalltalkEditor methodsFor: 'extract method' stamp: 'HAW 12/23/2019 10:59:16'!
extractMethod

	"hack to prevent the extract method to be evaluated on editors w/o methods like the workspace"
	self codeProvider respondsTo: #currentCompiledMethod
		:: and: [ self codeProvider currentCompiledMethod notNil ]
		:: ifFalse: [ ^ nil ].
 
	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractMethodApplier createAndValueHandlingExceptions: [
			ExtractMethodApplier for: self selectionInterval of: self codeProvider currentCompiledMethod ] ]! !


!ExtractMethodApplier methodsFor: 'private - parameterizing' stamp: 'HAW 12/23/2019 10:02:36'!
nodesThatAddVariablesToScope

	^ (Set with: methodToExtractCodeFrom methodNode)
		addAll: self blockNodesEnclosingIntervalToExtract;
		yourself! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'HAW 12/23/2019 10:20:54'!
startAndEndNodesShareAParentNode  

	| parseNodesInCommon |
	parseNodesInCommon _ self parseNodesInCommon.
	
	^ parseNodesInCommon notEmpty and: [
		(self trimmed: parseNodesInCommon first value) = intervalToExtract]


! !

!methodRemoval: SmalltalkEditor #ifChangeSelectorCanBeAppliedDo: stamp: 'HAW 12/23/2019 10:59:16'!
SmalltalkEditor removeSelector: #ifChangeSelectorCanBeAppliedDo:!

!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector ifPrimitivePrintOn: methodClass printCommentOn: printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printSelectorAndArgumentsOn: printTemporariesOn: printWithClosureAnalysisOn: selectorAndArgumentsAsString sourceText tempNames)
('source mapping' addPositionTo:of:inside: completeSourceRanges completeSourceRangesDo: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: parseNodesPathAt:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' allParseNodesWithin:satisfy: hasArgumentOrTemporaryNamed: hasLocalNamed: isMultipleRanges:)
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
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
('extract method' extractMethod extractMethod:)
!

