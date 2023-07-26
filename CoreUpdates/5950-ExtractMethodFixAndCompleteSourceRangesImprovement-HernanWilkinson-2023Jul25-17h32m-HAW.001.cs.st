'From Cuis 6.0 [latest update: #5948] on 26 July 2023 at 10:24:48 am'!
!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue completeSourceRanges '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Encoder methodsFor: 'source mapping' stamp: 'HAW 7/25/2023 19:25:22'!
initializeCompleteSourceRangesBasedOn: sourceCode

	completeSourceRanges := Dictionary new.
	sourceRanges keysAndValuesDo: [ :parseNode :nodeRanges |
		"leaf nodes excluded because they have the same complete source ranges than the block nodes they wrap - Nahuel"
		parseNode class = LeafNode ifFalse: [
			| expandedNodeSourceRanges |
			expandedNodeSourceRanges := parseNode expandRanges: nodeRanges basedOn: sourceRanges using: sourceCode.
			completeSourceRanges at: parseNode put: expandedNodeSourceRanges ] ].
! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 7/25/2023 19:24:01'!
parseNodesPathAt: aPosition ifAbsent: aBlockClosure

	| nodesWithRangeAtPosition |
	
	nodesWithRangeAtPosition := SortedCollection sortBlock: [ :parseNodeWithSourceRangeOne :parseNodeWithSourceRangeTwo |
		self criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo ].

	completeSourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange |
			nodesWithRangeAtPosition add: (nodeAtRange key -> aRange) ] ].

	^ nodesWithRangeAtPosition
		ifEmpty: aBlockClosure
		ifNotEmpty: [ nodesWithRangeAtPosition ]! !


!MethodReference methodsFor: 'decompiling' stamp: 'HAW 7/26/2023 09:58:00'!
createMethodNodeOptimizeSpecialSends: shouldOptimize
 
	^self compiledMethod createMethodNodeOptimizeSpecialSends: shouldOptimize! !


!CompiledMethod methodsFor: 'ometa2preload' stamp: 'HAW 7/26/2023 10:01:09'!
createMethodNodeOptimizeSpecialSends: doOptimizeSpecialSends
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail.
	To avoid incopatibility with other parsers, we only send the message methodNodeFor:noPattern:optimizeSpecialSends: 
	when it is false that is the special case needed for the refactorings - Hernan"
	^ (doOptimizeSpecialSends
		ifTrue: [ 	(aClass methodNodeFor: source) ]
		ifFalse: [ 	(aClass methodNodeFor: source noPattern: false optimizeSpecialSends: false) ]) 
			ifNil: [ self decompile ].! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 7/25/2023 19:25:49'!
completeSourceRangesBasedOn: sourceCode

	completeSourceRanges ifNil: [ self initializeCompleteSourceRangesBasedOn: sourceCode ].
	^completeSourceRanges ! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 7/25/2023 19:24:25'!
parseNodesPathAt: aPosition ifAbsent: aBlockClosure

	"This is to be sure that the enconder has the completeSourceRanges - Hernan"
	self completeSourceRanges.
	^ encoder
		parseNodesPathAt: aPosition
		ifAbsent: aBlockClosure! !


!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'HAW 7/26/2023 09:58:06'!
shouldBeEnclosedWithParens

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode := methodToExtractFrom createMethodNodeOptimizeSpecialSends: false.
	initialNodeAncestors  := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon := initialNodeAncestors intersection: finalNodeAncestors.

	initialNode := (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode := (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions := initialNode isMessageNode and: [ finalNode isMessageNode ].
	
	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]! !

!methodRemoval: Encoder #parseNodesPathAt:using:ifAbsent: stamp: 'HAW 7/25/2023 19:23:42'!
Encoder removeSelector: #parseNodesPathAt:using:ifAbsent:!
!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue completeSourceRanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: newTemp: newUndeclaredTemp:)
('encoding' asciiStringLiteral: cantStoreInto: encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: completeSourceRangesBasedOn: criteriaToSortSourceRangeBetween:and: globalSourceRanges initializeCompleteSourceRangesBasedOn: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: parseNodesPathAt:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables temps:literals:class:)
('initialize-release' noteSuper release)
('temporaries' maxTemp)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body numberOfStatements primitiveErrorVariableName removeProperty: statements tempNodes temporaries temporaries: temporariesDeclaration temporariesDeclaration:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector ifPrimitivePrintOn: methodClass printCommentOn: printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printSelectorAndArgumentsOn: printTemporariesOn: printWithClosureAnalysisOn: selectorAndArgumentsAsString sourceText tempNames)
('source mapping' addPositionTo:of:inside: completeSourceRanges completeSourceRangesDo: completeSourceRangesOf:ifAbsent: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: parseNodesPathAt:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition singleCompleteSourceRangeOf: singleCompleteSourceRangeOf:ifPresent:ifAbsent: withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: selector:arguments:precedence:temporariesDeclaration:block:encoder:primitive:properties: selector:arguments:precedence:temporariesDeclaration:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' allParseNodesWithin:satisfy: anyParseNodeWithin:satisfy: equivalentTo: hasArgumentOrTemporaryNamed: hasLocalNamed: isMethodNode isMultipleRanges: referencesSelf referencesSuper)
('source ranges' definitionStartPosition)
!

