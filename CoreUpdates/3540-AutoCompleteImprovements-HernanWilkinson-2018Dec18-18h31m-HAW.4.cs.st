'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 19 December 2018 at 7:42:39 pm'!

!SystemDictionary methodsFor: 'testing' stamp: 'HAW 12/19/2018 16:09:00'!
isLiveTypeInformationInstalled
	
	^(FeatureRequirement name: #LiveTypeInformation) isAlreadySatisfied! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/19/2018 19:41:54'!
returnTypeOfEnclosedExpressionAt: aRange 
	
	Smalltalk isLiveTypeInformationInstalled ifFalse: [ ^nil ].
	
	^self 
		withParserSourceMethodNodeDo: [ :methodNode | self returnTypeOfEnclosedExpresionIn: methodNode at: aRange end ]
		ifError: [ :anError | nil ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/19/2018 16:43:55'!
returnTypeOfUnaryMessage: aSelector at: aRange 
	
	Smalltalk isLiveTypeInformationInstalled ifFalse: [ ^nil ].
	
	^self 
		withParserSourceMethodNodeDo: [ :methodNode | self returnTypeOfNodeUnderCursorIn: methodNode at: aRange end]
		ifError: [ :anError | nil ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/19/2018 16:18:52'!
withParserSourceMethodNodeDo: doBlock ifError: anErrorBlock

	^ [[ doBlock value: (parser classOrMetaClass methodNodeFor: parser source) ] 
		on: UndeclaredVariableReference 
		do: [  :anUndeclareVariableReference | anUndeclareVariableReference declareTempAndResume ]]
		on: Error
		do: anErrorBlock
		
	! !


!UndeclaredVariableReference methodsFor: 'handling' stamp: 'HAW 12/19/2018 19:34:38'!
declareTempAndResume

	parser declareTemp: varName at: #method.
	self resume: varName! !

!UndeclaredVariableReference methodsFor: 'handling' stamp: 'HAW 12/19/2018 19:34:42'!
defaultAction

	^parser correctVariable: varName interval: (varStart to: varEnd)! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 12/18/2018 19:03:40'!
parseNodeIncluding: aPosition ifAbsent: aBlockClosure

	| smallestRangeSize nodeWithRangeAtPosition |

	smallestRangeSize := SmallInteger maxVal.
	nodeWithRangeAtPosition := nil.

	sourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange | | currentNodeRangeSize |
				currentNodeRangeSize := aRange size.
				currentNodeRangeSize < smallestRangeSize ifTrue: [
					smallestRangeSize := currentNodeRangeSize.
					nodeWithRangeAtPosition := nodeAtRange key -> aRange ]]].

	^ nodeWithRangeAtPosition ifNil: aBlockClosure ifNotNil: [ nodeWithRangeAtPosition ]
			! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 12/18/2018 19:04:26'!
parseNodeIncluding: aPosition ifAbsent: aBlockClosure 
	
	^encoder parseNodeIncluding: aPosition ifAbsent: aBlockClosure 
! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 12/18/2018 18:41:14'!
shouldOpenMorphWhenNoPrefixFor: currentChar 
	
	^currentChar isAlphaNumeric 
		or: [ currentChar isRightBracket
		or: [ currentChar = $) ]]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/19/2018 19:41:54'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClassOrEntries |

	id _ allSource copyFrom: range start to: range end.
	receiverClassOrEntries _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ specificModel classOfInstVarNamed: id ].
			[ #methodArg ] 		-> [ specificModel classOfTempVarNamed: id ].
			[ #tempVar ] 			-> [ specificModel classOfTempVarNamed: id ].
			[ #workspaceVar ]		-> [ specificModel classOfWorkspaceVarNamed: id ].
			[ #blockArg ]			-> [ specificModel classOfBlockArgNamed: id ].
			[ #blockTempVar ]		-> [ specificModel classOfBlockTempVarNamed: id ].
			[ #thisContext ] 		-> [ specificModel classOfThisContext ]. 
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #blockEnd ]			-> [ BlockClosure ].
			[ #unary ]				-> [ self returnTypeOfUnaryMessage: id at: range ].
			[ #rightParenthesis ]  -> [ self returnTypeOfEnclosedExpressionAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ 
			receiverClassOrEntries isEmpty
				ifTrue: [ self computeMessageEntries: nil ]
				ifFalse: [ entries _ receiverClassOrEntries asArray sort ]]
		ifFalse: [ self computeMessageEntries: receiverClassOrEntries ]! !


!SHParserST80 methodsFor: 'accessing' stamp: 'HAW 12/19/2018 16:10:09'!
classOrMetaClass
	
	^classOrMetaClass! !

!methodRemoval: MethodNode #topParseNodeIncluding:ifAbsent:!
MethodNode removeSelector: #topParseNodeIncluding:ifAbsent:!
!methodRemoval: Encoder #topParseNodeIncluding:ifAbsent:!
Encoder removeSelector: #topParseNodeIncluding:ifAbsent:!

!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:in: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parseNodeIncluding:ifAbsent: rangesForInstanceVariable:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForTemporaryVariable:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap sourceRangeFor: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
('*Refactoring' messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLasPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: parameterDefinitionPositionFor: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining:)
('*LiveTypeInformation' topParseNodeIncluding:ifAbsent: typedMessageSendSelectorKeywordPositionsOf:ifAbsent:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' arguments arguments: body primitiveErrorVariableName removeProperty: temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' parseNodeIncluding:ifAbsent: rangesForInstanceVariable:ifAbsent: rangesForTemporaryVariable:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordsRanges selectorLastPosition)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('*Refactoring' addPositionTo:of:inside: argumentNames classAndSelector hasArgumentOrTemporary: hasLocalNamed: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLasPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: selectorKeywordPositionAt: selectorKeywordsPositions tempNodes withParseNodeIncluding:do:ifAbsent:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
('*LiveTypeInformation' identifierPrintString nodeTypePrintString topParseNodeIncluding:ifAbsent: typedMessageSendSelectorKeywordPositionsOf:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' canComputeMessageEntriesFor:and: classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: parse:in:and: returnTypeOfEnclosedExpressionAt: returnTypeOfUnaryMessage:at: selectedClassOrMetaClassIn: textProviderOrModel withParserSourceMethodNodeDo:ifError:)
('testing' opensWithTab)
('*LiveTypeInformation' returnTypeOf:in: returnTypeOfEnclosedExpresionIn:at: returnTypeOfNodeUnderCursorIn:at:)
!


!SHParserST80 reorganize!
('accessing' blockDepths blockDepthsStartIndexes classOrMetaClass classOrMetaClass: last3Ranges lastRange ranges source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNames workspaceNamesDo:)
('testing' isMessage: isPartialOrFullIdentifier:)
('*LiveTypeInformation')
!

