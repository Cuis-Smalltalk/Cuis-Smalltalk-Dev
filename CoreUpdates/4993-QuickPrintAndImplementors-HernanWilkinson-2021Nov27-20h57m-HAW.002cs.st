'From Cuis 5.0 [latest update: #4972] on 1 December 2021 at 1:03:21 pm'!

!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 11/30/2021 19:02:42'!
sizeOfDoItSourceCodeHeaderWithContext: hasContext 

	^(String streamContents: [ :stream | self doItSourceCodeHeaderWithContext: hasContext into: stream ]) size! !


!ParseNode methodsFor: 'printing' stamp: 'HAW 11/28/2021 16:59:21'!
printSourceCode
	
	^String streamContents: [ :stream | self printOn: stream indent: 0 ] ! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 15:44:04'!
afterCompiling: aSourceCode do: aBlock

	| context provider receiver |
	
	provider := self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver := provider doItReceiver.
			context := provider doItContext]
		ifFalse: [
			receiver := context := nil].

	^self afterCompiling: aSourceCode do: aBlock for: receiver in: context ifFail: [ nil ].
	! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 15:49:45'!
afterCompiling: aSourceCode do: aBlock for: aReceiver in: anEvaluationContext ifFail: aFailBlock
	
	| methodNode method sourceCode compiler |

	sourceCode := self doItSourceCodeFor: aSourceCode in: anEvaluationContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler		
			compileDoIt: sourceCode
			in: aReceiver class
			context: anEvaluationContext
			notifying: self
			ifFail: [ ^ aFailBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method _ methodNode generate.
	method methodNode: methodNode.

	^aBlock value: compiler value: method value: aReceiver value: anEvaluationContext 
! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 19:01:22'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^String streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode ]! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 15:50:49'!
evaluate: aSourceCode andDo: aBlock ifFail: failBlock profiled: doProfile
	
	| result | 
	
	^ self 
		afterCompiling: aSourceCode 
		do: [ :compiler :method :receiver :context | method ifNotNil: [
			result := compiler evaluateMethod: method to: receiver logged: true profiled: doProfile.
			aBlock value: result ]]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 17:10:10'!
browseImplementorsAt: aPosition

	self 
		withSelectorAt: aPosition 
		do: [ :selector | Smalltalk browseAllImplementorsOf: selector ]
		ifBehavior: [ :behavior | BrowserWindow fullOnClass: behavior selector: nil ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 16:28:56'!
withSelectorAt: aPosition do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock

	| methodNode nodeAtRange nodeUnderCursor  |
	
	methodNode := self codeProvider
		methodNodeOf: model actualContents
		ifErrorsParsing: [ :anError | ^ failBlock value ].
	
	aPosition < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: aPosition ifAbsent: [ ^ failBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].
	(nodeUnderCursor isLiteralVariableNode and: [ nodeUnderCursor isReferencingBehavior ]) ifTrue: [ ^aBehaviorBlock value: nodeUnderCursor key value ].
	
	^ failBlock value ! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 16:45:22'!
withSelectorUnderCursorDo: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock

	self withSelectorAt: self startIndex do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock! !

!SmalltalkEditor methodsFor: 'quick action (shift+click)' stamp: 'HAW 11/30/2021 17:09:44'!
hoverHelpStringOfEvaluating: aNodeUnderCursor

	^ self
		evaluate: aNodeUnderCursor printSourceCode
		andDo: [ :result | result printString ]
		ifFail: [ 
			morph flash.
			'Nothing to show' ]
		profiled: false! !

!SmalltalkEditor methodsFor: 'quick action (shift+click)' stamp: 'HAW 11/30/2021 17:09:44'!
hoverHelpToShowEvaluating: aNodeUnderCursor

	^ [[ self hoverHelpStringOfEvaluating: aNodeUnderCursor]
		on: MessageNotUnderstood 
		do: [ :mnu | | receiverPrintString | 
			"just guessing if the real receiver is the one in the message node to show a nicer error message.
			This will not be correct in cases such as: v1 xx: (v2 xx: 1). In this case will show that v1 does not understand xx: 
			when it is v2 the one that does not understand it, but this is so rare that I leave it anyway - Hernan"
			receiverPrintString := mnu message selector = aNodeUnderCursor selectorSymbol 
				ifTrue: [ aNodeUnderCursor originalReceiver printSourceCode ]
				ifFalse: [ mnu receiver printString ].
			mnu return: receiverPrintString, ' does not understand ', mnu message selector]]
		on: Error 
		do: [ :anError | anError return: anError messageText ]! !

!SmalltalkEditor methodsFor: 'quick action (shift+click)' stamp: 'HAW 11/30/2021 16:36:59'!
implementorsOfNodeUnder: aMousePosition

	| mousePositionInText |
	
	mousePositionInText := morph positionInTextOf: aMousePosition.
	self browseImplementorsAt: mousePositionInText ! !

!SmalltalkEditor methodsFor: 'quick action (shift+click)' stamp: 'HAW 11/30/2021 15:53:13'!
quickPrintOfNodeUnder: aMousePosition

	| mousePositionInText |
	
	mousePositionInText := morph positionInTextOf: aMousePosition.

	self 
		withMethodNodeAndClassDo: [ :methodNode :class | 
			methodNode 
				withParseNodeIncluding: mousePositionInText 
				do: [ :aNodeUnderCursor | morph showHoverHelpWith: (self hoverHelpToShowEvaluating: aNodeUnderCursor)]
				ifAbsent: [  morph flash  ]]
		ifErrorsParsing: [ :anError | morph showHoverHelpWith: anError messageText ]! !


!Morph methodsFor: 'accessing - properties' stamp: 'HAW 11/30/2021 15:09:02'!
valueOfProperty: aSymbol ifPresentDo: aBlock ifAbsent: absentBlock
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property, if not evaluate absentBlock"

	| value |
	
	value := self valueOfProperty: aSymbol ifAbsent: [^ absentBlock value ].
	
	^aBlock value: value! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'HAW 11/28/2021 17:38:17'!
positionInTextOf: aMousePosition 
	
	^(textComposition characterBlockAtPoint: aMousePosition) stringIndex. 
	! !


!CodeWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 13:02:12'!
addShiftClickEventHandlerFor: codePane

	^ codePane scroller setProperty: #mouseButton1Up:localPosition: toValue: [ :event :mousePosition |
		event shiftPressed
			ifTrue: [ self shiftClickPressedOn: codePane at: mousePosition. true ]
			ifFalse: [ false ]]! !

!CodeWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 12:55:25'!
createCodePaneMorph

	^ (TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)'! !

!CodeWindow methodsFor: 'menu commands' stamp: 'HAW 12/1/2021 12:24:24'!
shiftClickPressedOn: aCodePane at: aMousePosition
	
	aCodePane editor implementorsOfNodeUnder: aMousePosition ! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 12:57:57'!
createCodePaneMorph
	
	codePane _ super createCodePaneMorph.
	^codePane! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 12:56:51'!
createCodePaneMorph

	^(TextModelMorph
		textProvider: model
		textGetter: #acceptedContents) emptyTextDisplayMessage: 'Selection detail (no change selected?)'! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 12:58:06'!
createCodePaneMorph
	
	codePane _ super createCodePaneMorph.
	^codePane! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'HAW 12/1/2021 12:25:08'!
shiftClickPressedOn: aCodePane at: aMousePosition
	
	aCodePane editor quickPrintOfNodeUnder: aMousePosition
	! !


!PseudoClass methodsFor: 'class' stamp: 'HAW 11/30/2021 14:38:45'!
superclass
	
	"I do not use superclassName because definition can be nil. This can cause a inconsistency becuase the definition
	could be different to what the image has ... 
	On the other hand, allSuperclasses uses realClass so it is inconsistent already - Hernan"
	
	^self realClass ifNotNil: [ :aClass | aClass superclass ]! !


!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 11/30/2021 17:20:58'!
doItSourceCodeHeaderWithContext: hasContext into: stream

	stream
		nextPutAll: (hasContext ifTrue: [ Scanner doItInSelector, ' ', Encoder doItInContextName ] ifFalse: [ Scanner doItSelector ]);
		newLine;
		newLine! !


!Parser methodsFor: 'error correction' stamp: 'HAW 11/30/2021 19:02:42'!
sourceDelta

	| userSelectionDelta |
	requestor notNil ifTrue: [
		userSelectionDelta _ requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ].
		encoder selector = Scanner doItSelector ifTrue: [ 
			^ (Scanner sizeOfDoItSourceCodeHeaderWithContext: false) - userSelectionDelta ].
		encoder selector = Scanner doItInSelector ifTrue: [ 
			^ (Scanner sizeOfDoItSourceCodeHeaderWithContext: true) - userSelectionDelta ]].
	
	^ 0! !


!LiteralVariableNode methodsFor: 'testing' stamp: 'HAW 11/30/2021 17:12:09'!
isReferencingBehavior
	
	^key value isBehavior! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 15:46:41'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selectionAsStream upToEnd
		do: [ :compiler :method :receiver :context | method ifNotNil: [ self debug: method receiver: receiver in: context ]]! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 11/30/2021 15:46:34'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"	
	
	self lineSelectAndEmptyCheck: [^ ''].
	^self evaluate: self selectionAsStream upToEnd andDo: aBlock ifFail: failBlock profiled: doProfile! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 16:33:26'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	Preferences alternativeBrowseIt ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry _ anEntry class].
			BrowserWindow fullOnClass: anEntry selector: nil]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 16:36:59'!
implementorsOfIt
	"Open an implementors browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	^self selectedSelector 
		ifNil: [ self browseImplementorsAt: self startIndex ]
		ifNotNil: [ :selector | Smalltalk browseAllImplementorsOf: selector ].
	! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/30/2021 17:02:01'!
sendersOfIt
	"Open a senders browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllCallsOn: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllCallsOn: selector ]
		ifBehavior: [:behavior | Smalltalk browseAllCallsOn: (Smalltalk bindingOf: behavior name)] 
		otherwise: [ self sendersOfItWhenErrorsParsing ]! !


!InnerTextMorph methodsFor: 'events' stamp: 'HAW 11/30/2021 14:54:59'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| mouseButton1DownHandled |

	self pauseBlinking.

	mouseButton1DownHandled := self 
		valueOfProperty: #mouseButton1Up:localPosition:
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
		ifAbsent: [ false ].

	mouseButton1DownHandled ifFalse: [
		self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ]].

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'hover help' stamp: 'HAW 11/30/2021 17:07:26'!
showHoverHelpWith: msgString 
	
	| w balloon h |
	(w _ self world) ifNil: [^ self].
	h _ w activeHand.
	balloon _ HoverHelpMorph contents: msgString.
	
	"Do it in a while. In some cases, processing the event that might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [ balloon popUpForHand: h ]! !


!CodeWindow methodsFor: 'GUI building' stamp: 'HAW 12/1/2021 12:56:31'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	self addShiftClickEventHandlerFor: codePane.
		
	^codePane! !

!methodRemoval: DebuggerWindow #buildMorphicCodePane stamp: 'HAW 12/1/2021 12:58:22'!
DebuggerWindow removeSelector: #buildMorphicCodePane!
!methodRemoval: ChangeListWindow #buildMorphicCodePane stamp: 'HAW 12/1/2021 12:56:59'!
ChangeListWindow removeSelector: #buildMorphicCodePane!
!methodRemoval: BrowserWindow #buildMorphicCodePane stamp: 'HAW 12/1/2021 12:58:18'!
BrowserWindow removeSelector: #buildMorphicCodePane!
!methodRemoval: CodeWindow #shiftClikedPressedOn:at: stamp: 'HAW 12/1/2021 12:24:24'!
CodeWindow removeSelector: #shiftClikedPressedOn:at:!
!methodRemoval: SmalltalkEditor #withSelectorUnderCursorDo:ifErrorsParsing: stamp: 'HAW 11/30/2021 16:17:33'!
SmalltalkEditor removeSelector: #withSelectorUnderCursorDo:ifErrorsParsing:!
!methodRemoval: SmalltalkEditor #compile:for:in:ifFail: stamp: 'HAW 11/30/2021 15:41:59'!
SmalltalkEditor removeSelector: #compile:for:in:ifFail:!
!methodRemoval: SmalltalkEditor #withSelectorAt:do:otherwise: stamp: 'HAW 11/30/2021 17:01:47'!
SmalltalkEditor removeSelector: #withSelectorAt:do:otherwise:!
!methodRemoval: SmalltalkEditor #compile:and: stamp: 'HAW 11/30/2021 15:34:58'!
SmalltalkEditor removeSelector: #compile:and:!
!methodRemoval: SmalltalkEditor #stringResultEvaluating: stamp: 'HAW 11/30/2021 17:09:44'!
SmalltalkEditor removeSelector: #stringResultEvaluating:!
!methodRemoval: SmalltalkEditor #withSelectorUnderCursorDo:otherwise: stamp: 'HAW 11/30/2021 16:45:00'!
SmalltalkEditor removeSelector: #withSelectorUnderCursorDo:otherwise:!
!methodRemoval: SmalltalkEditor #withSelectorAt:do:ifErrorsParsing:ifNoSelector: stamp: 'HAW 11/30/2021 16:29:41'!
SmalltalkEditor removeSelector: #withSelectorAt:do:ifErrorsParsing:ifNoSelector:!
!methodRemoval: SmalltalkEditor #compileSelectionFor:in:ifFail: stamp: 'HAW 11/30/2021 15:20:52'!
SmalltalkEditor removeSelector: #compileSelectionFor:in:ifFail:!
!methodRemoval: SmalltalkEditor #browseImplementorsOfSelectorAt: stamp: 'HAW 11/30/2021 16:36:59'!
SmalltalkEditor removeSelector: #browseImplementorsOfSelectorAt:!
!methodRemoval: SmalltalkEditor #compileSelectionFor:in: stamp: 'HAW 11/30/2021 15:35:52'!
SmalltalkEditor removeSelector: #compileSelectionFor:in:!
!methodRemoval: SmalltalkEditor #withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: stamp: 'HAW 11/30/2021 16:18:35'!
SmalltalkEditor removeSelector: #withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector:!
!methodRemoval: SmalltalkEditor #selectionDoItSourceCodeIn: stamp: 'HAW 11/30/2021 15:20:56'!
SmalltalkEditor removeSelector: #selectionDoItSourceCodeIn:!
!methodRemoval: Scanner class #doItSourceCodeHeaderSizeWithContext: stamp: 'HAW 11/30/2021 19:02:42'!
Scanner class removeSelector: #doItSourceCodeHeaderSizeWithContext:!
!methodRemoval: Scanner class #selectionDoItSourceCodeHeaderSizeWithContext: stamp: 'HAW 11/30/2021 19:02:00'!
Scanner class removeSelector: #selectionDoItSourceCodeHeaderSizeWithContext:!

!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments2: addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:doIt:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('interactive checks')
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
('as yet unclassified' performInteractiveChecks: possibleSelfRecursionWarningMessage: warnIfPossibilityOfSelfRecursion:)
!


!LiteralVariableNode reorganize!
('visiting' accept:)
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('testing' assignmentCheck:at: isReferencingBehavior)
!


!SmalltalkEditor reorganize!
('accessing-selection' selection)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do: afterCompiling:do:for:in:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu: openSmalltalkEditorRefactoringMenu refactoringMenu)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' actualImplementorsOfIt actualImplementorsOfIt: actualLocalImplementorsOfIt actualLocalImplementorsOfIt: actualSendersOfIt actualSendersOfIt: balloonTypeInfoAt: balloonTypeInfoInMethodAt: balloonTypeInfoOf:in:definedAt: balloonTypeInfoOfInstanceVariableAt: showSelectionTypeInfo showSelectionTypeInfo: showSelectionTypeInfoInMethod showTypeInfoOf:in:definedAt: showTypeInfoOfInstanceVariable typeCheckMethod typeCheckMethod: hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
!


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: positionInTextOf: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeystrokeEvent:)
('events-processing' processKeystroke: processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
('copying' okayToDuplicate)
('hover help' showBalloon:hand: wantsBalloon showHoverHelpWith:)
!


!CodeWindow reorganize!
('GUI building' addShiftClickEventHandlerFor: buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane createCodePaneMorph defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseProtocol browseSenders browseSendersOfMessages browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance openSingleMessageBrowser shiftClickPressedOn:at: showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerClassListMenu offerClassListMenu2 offerWhatToShowMenu openMessageListMenu openMessageListMenu2)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from:)
('initialization' model:)
('notification actions' initializeNotificationActions registerNotificationActions registerNotificationActionsIfModelNotNil removeNotificationActions)
!


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow createCodePaneMorph customButtonRow customButtonSpecs initialExtent optionalButtonTuples windowColor)
('menu building' contextFieldListMenu contextStackMenu receiverFieldListMenu)
('menu commands' contextClassHierarchy exploreContextSelection exploreReceiverSelection receiverClassHierarchy returnValue shiftClickPressedOn:at:)
('keyboard shortcuts' browseClassRefsIn: browseFullProtocolIn: browseHierarchyIn: browseMethodFullIn: contextStackKey:from: exploreSelectionIn: inspectSelectionIn: inspectorKey:from:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack peelToFirst proceed restart runToCursor send stepIntoBlock where)
('notification actions' registerNotificationActions)
!

