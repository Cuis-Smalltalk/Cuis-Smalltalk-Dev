'From Cuis 5.0 [latest update: #3970] on 13 December 2019 at 1:28:36 am'!

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:21:02'!
createTempDeclarationInBlockWith: tempName before: aTempsMark 
	"Return the new tempsMark."
	
	| delta insertion offset |
	
	insertion := ' | ' , tempName , ' |'.
	delta := 1.	"the bar"
	offset := self insertWord: insertion at: aTempsMark + 1.
	
	^aTempsMark + offset - delta.! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:20:31'!
createTempDeclarationInMethodWith: aTempName
	"No bars - insert some with CR, tab. Return the new tempsMark."
	
	| characterBeforeMark delta insertion theTextString offset |
	
	theTextString := requestor text string.
	characterBeforeMark := theTextString at: tempsMark - 1 ifAbsent: [$ ].
	insertion := '| ', aTempName, ' |', String newLineString.
	delta := 2.	"the bar and CR"
	characterBeforeMark = Character tab ifTrue: [
		insertion := insertion , String tab.
		delta := delta + 1.	"the tab"
	].
	
	offset := self insertWord: insertion at: tempsMark.

	^tempsMark + offset - delta.! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:19:25'!
insertWord: anInsertion at: aPosition
	
	^self substituteWord: anInsertion
		wordInterval: (aPosition to: aPosition - 1)
		offset: 0.! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:20:01'!
pasteTemp: name before: aTempsMark
	"Return the new position of the tempsMark."
	
	| insertion theTextString  characterBeforeMark offset |
	
	theTextString := requestor text string.
	insertion := name, ' '.
	characterBeforeMark := theTextString at: aTempsMark - 1 ifAbsent: [$ ].
	characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
	offset := self insertWord: insertion at: aTempsMark.
	
	^aTempsMark + offset.! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:09:37'!
pasteTemp: tempName inBlock: aBlockNode

	| theTextString blockTempsMark newTempsMark |

	theTextString := requestor text string.
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	
	newTempsMark := 
		(theTextString at: blockTempsMark) = $| 
			ifTrue: [
		  		"Paste it before the second vertical bar"
				aBlockNode tempsMark: (self pasteTemp: tempName before: blockTempsMark) ] 
			ifFalse: [ 
				"The tempsMark is at the beginning of the block, we should insert after it."
				 self createTempDeclarationInBlockWith: tempName before: blockTempsMark ].
	
	aBlockNode tempsMark: newTempsMark.! !


!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 00:18:45'!
declareUndeclaredTemps: methodNode
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."
	| undeclared userSelection blocksToVars |
	(undeclared _ encoder undeclaredTemps) isEmpty ifTrue: [ ^ self ].
	userSelection _ requestor selectionInterval.
	blocksToVars _ IdentityDictionary new.
	undeclared do: [ :var |
		(blocksToVars
			at: (var tag == #method
				ifTrue: [ methodNode block ]
				ifFalse: [ methodNode accept: (VariableScopeFinder new ofVariable: var) ])
			ifAbsentPut: [ SortedCollection new ]) add: var name ].
	(blocksToVars removeKey: methodNode block ifAbsent: nil) ifNotNil: [ :rootVars |
		rootVars do: [ :varName |
			self pasteTempAtMethodLevel: varName ]].
	(blocksToVars keys sort: [ :a :b |
		a tempsMark < b tempsMark ]) do: [ :block | 
		(blocksToVars at: block) do: [ :varName | self pasteTemp: varName inBlock: block ]].
	requestor
		selectInvisiblyFrom: userSelection first
		to: userSelection last + requestorOffset.
	ReparseAfterSourceEditing signal! !

!Parser methodsFor: 'error correction' stamp: 'EB 12/13/2019 01:26:43'!
pasteTempAtMethodLevel: name

	| theTextString |
	
	theTextString := requestor text string.
	tempsMark := 
		(theTextString at: tempsMark) = $| 
			ifTrue: [
  				"Paste it before the second vertical bar"
		 		self pasteTemp: name before: tempsMark ] 
			ifFalse: [ self createTempDeclarationInMethodWith: name ].
	! !


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken match: matchReturn matchToken: startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain doitPatternInContext: expression keywordPattern messagePart:repeat: method:doIt:context: newMethodNode pattern:inContext: performInteractiveChecks: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable)
('temps' bindArg: bindArg:range: bindTemp:)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createTempDeclarationInBlockWith:before: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: insertWord:at: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTemp:inBlock: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTemps rangesForRemovableUnusedTempsInBlockNode: removeUnusedTempRange:with: removeUnusedTemps removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
!

