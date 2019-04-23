'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 17 April 2019 at 9:09:08 pm'!
!classDefinition: #ClassDefinitionNodeAnalyzer category: #'Compiler-Support'!
Object subclass: #ClassDefinitionNodeAnalyzer
	instanceVariableNames: 'classDefinitionNode classCreationMessageNode superClassNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!ClassDefinitionNodeAnalyzer methodsFor: 'initialization' stamp: 'HAW 4/17/2019 18:19:19'!
initializeFor: aClassDefinitionMethodNode 

	classDefinitionNode := aClassDefinitionMethodNode.
	classCreationMessageNode := classDefinitionNode block statements first expr.
	superClassNode := classCreationMessageNode receiver.
! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 4/17/2019 20:47:37'!
isAtCategory: anIndex

	^self is: anIndex atStringParameterNumber: self class categoryPosition 
	! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 4/17/2019 18:36:15'!
isAtClassName: anIndex

	^(classDefinitionNode sourceRangeFor: classCreationMessageNode arguments first) first includes: anIndex! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 4/17/2019 20:46:45'!
isAtInstanceVariables: anIndex

	^self is: anIndex atStringParameterNumber: self class instanceVariableNamesPosition! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'HAW 4/17/2019 18:34:11'!
isAtSuperclass: anIndex

	^(classDefinitionNode sourceRangeFor: superClassNode) first includes: anIndex! !

!ClassDefinitionNodeAnalyzer methodsFor: 'accessing' stamp: 'HAW 4/17/2019 18:41:46'!
superclass
	
	^superClassNode key value ! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing - private' stamp: 'HAW 4/17/2019 20:37:06'!
is: anIndex atStringParameterNumber: aParameterPosition

	| parameterRange |
	
	parameterRange := (classDefinitionNode sourceRangeFor: (classCreationMessageNode arguments at: aParameterPosition)) first.
	
	^anIndex between: parameterRange first + 1 and: parameterRange last - 1! !


!ClassDefinitionNodeAnalyzer class methodsFor: 'parameters positions' stamp: 'HAW 4/17/2019 20:47:27'!
categoryPosition
	
	^5! !

!ClassDefinitionNodeAnalyzer class methodsFor: 'parameters positions' stamp: 'HAW 4/17/2019 20:47:15'!
instanceVariableNamesPosition
	
	^2! !

!ClassDefinitionNodeAnalyzer class methodsFor: 'instance creation' stamp: 'HAW 4/17/2019 18:18:03'!
for: aClassDefinitionMethodNode 
	
	^self new initializeFor: aClassDefinitionMethodNode ! !


!Editor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 16:01:45'!
wordLeftDelimiters

	^''! !

!Editor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 15:57:53'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^1 to: 1].
	here _ self pointIndex min: string size max: 2.
	initialDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			finalDelimiter _ rightDelimiters at: match]
		ifFalse: [
			initialDelimiter _ string at: here.
			match _ rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					finalDelimiter _ leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction _ -1]].
	level _ 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar _ string at: (here _ here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start _ 1.
									"go right if hit string start"
									direction _ 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start _ here + 1.
									"go right if hit non-token"
									direction _ 1]
								ifFalse: [level _ 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level _ level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		leftDelimiters size + rightDelimiters size = 0 ifFalse: [
			"If we failed to find final delimiter, then just select word."
			^self wordRangeLeftDelimiters: '' rightDelimiters: '' ].
		here _ here + direction ].
	^ direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			here + 1 to: stop]! !

!Editor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 16:01:58'!
wordRightDelimiters

	^''! !

!Editor methodsFor: 'new selection' stamp: 'HAW 4/17/2019 18:01:07'!
wordUnderCursorRange
	
	^self wordRangeLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters ! !


!TextEditor methodsFor: 'model access' stamp: 'HAW 4/17/2019 19:26:58'!
wordUnderCursor
	
	| wordUnderCursorRange word indexOfSpace |
	
	wordUnderCursorRange := self wordUnderCursorRange.
	word := (model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asString.
	
	"I have to handle the edge case where the cursor is for example between a ' and the first letter of the word.
	In that case the range will include words with spaces - Hernan"
	indexOfSpace := word indexOf: $  ifAbsent: [ ^word ].
	
	^word first: indexOfSpace -1 
	
			! !


!SmalltalkEditor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 16:01:45'!
wordLeftDelimiters

	^ '([{<|''"`'! !

!SmalltalkEditor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 16:01:58'!
wordRightDelimiters

	^ ')]}>|''"`'! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/17/2019 20:09:44'!
contextualRenameInClassDefinition 

	self ifRenameCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/17/2019 21:06:49'!
contextualRenameInClassDefinitionOf: aClassDefinitionNode in: aSelectedClass 
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtSuperclass: cursorPosition) 
		ifTrue: [ ^self renameClassOn: self codeProvider for: analyzer superclass ].
	
	(analyzer isAtClassName: cursorPosition)
		ifTrue: [ ^self  renameClassOn: self codeProvider for: aSelectedClass ].
		
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ ^self renameInstanceVariableOn: self codeProvider for: self wordUnderCursor at: aSelectedClass ].
		
	(analyzer isAtCategory: cursorPosition)
		ifTrue: [ 
			"I'm sure codeProvider is a Browser - Hernan"
			^self codeProvider renameSystemCategory ].
		
	morph flash
	
! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/15/2019 14:48:21'!
contextualRenameInMethod

	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualRenameOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ]		! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/15/2019 14:54:34'!
withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: model actualContents noPattern: true ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 4/17/2019 20:13:57'!
sourceRangeFor: aParseNode

	^encoder sourceRangeFor: aParseNode ! !


!Editor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 16:01:58'!
selectWord
	"Select delimited text or word--the result of double-clicking."

	^self selectWordLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters ! !

!Editor methodsFor: 'new selection' stamp: 'HAW 4/15/2019 15:48:33'!
selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| wordRange |
	
	wordRange := self wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters.
	
	self selectFrom: wordRange first to: wordRange last! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/15/2019 14:48:08'!
contextualRename

	self isEditingClassDefinition 
		ifTrue: [ self contextualRenameInClassDefinition ]
		ifFalse: [ self contextualRenameInMethod ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/17/2019 20:11:29'!
isEditingClassDefinition

	"This is ugly, but I don't find a better way to do it without making a big change in the code provider hierarchy - Hernan"
	^(self codeProvider respondsTo: #editSelection) and: [ self codeProvider editSelection == #editClass]! !

!methodRemoval: SmalltalkEditor #selectWord!
SmalltalkEditor removeSelector: #selectWord!

!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition sourceRangeFor: withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' hasArgumentOrTemporary: hasLocalNamed: isMultipleRanges:)
!


!ClassDefinitionNodeAnalyzer reorganize!
('initialization' initializeFor:)
('testing' isAtCategory: isAtClassName: isAtInstanceVariables: isAtSuperclass:)
('accessing' superclass)
('testing - private' is:atStringParameterNumber:)
!


!ClassDefinitionNodeAnalyzer class reorganize!
('parameters positions' categoryPosition instanceVariableNamesPosition)
('instance creation' for:)
!


!Editor reorganize!
('accessing' currentAttributes help morph morph: userHasEdited)
('accessing-selection' selectionInterval selectionIntervalsDo:)
('editing keys' copySelection: cut: noop: paste:)
('menu messages' clipboardStringOrText clipboardTextPut: lineSelectAndEmptyCheck: paste pasteString wordSelectAndEmptyCheck:)
('new selection' deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectMark:point: selectWord selectWordLeftDelimiters:rightDelimiters: wordLeftDelimiters wordRangeLeftDelimiters:rightDelimiters: wordRightDelimiters wordUnderCursorRange)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: selectWord:)
('typing/selecting keys' backspace: cursorTopHome: enter: newLine: normalCharacter: returnKey: selectAll selectAll:)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText moveCursor:forward:event: nextWordEnd: nextWordEndFrom:goingForwards: nextWordStart: nextWordStartFrom:goingForwards: previousWordStart: sameColumn:newLine:forward: setIndices:forward:)
('menu commands' offerMenuFromEsc:)
('typing support' cmdShortcuts shortcuts)
!


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: setSearch: text totalTextHeight visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('attributes' changeEmphasisOrAlignment clearFont offerColorMenu offerDefaultFontMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling' scrollBy:)
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: storeSelectionInComposition unapplyAttributes: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
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
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifRenameCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
!

