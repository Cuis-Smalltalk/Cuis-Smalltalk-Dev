'From Cuis 5.0 [latest update: #3987] on 23 December 2019 at 8:33:32 am'!

!CodeNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:32:49'!
arguments: aCollectionOfArguments

	self subclassResponsibility ! !

!CodeNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:29:27'!
block

	self subclassResponsibility ! !

!CodeNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:31:57'!
temporaries: aCollectionOfTemporaries

	self subclassResponsibility ! !

!CodeNode methodsFor: 'printing' stamp: 'HAW 12/23/2019 08:30:31'!
decompileString

	self subclassResponsibility ! !


!BlockNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:32:32'!
arguments: aCollectionOfArguments 
	"Decompile."

	arguments := aCollectionOfArguments! !

!BlockNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:31:43'!
temporaries: aCollectionOfTemporaries
	"Collection of TempVariableNodes"
	temporaries := aCollectionOfTemporaries! !


!MethodNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:32:45'!
arguments: aCollectionOfArguments

	"For transformations etc, not used in compilation"
	arguments := aCollectionOfArguments! !

!MethodNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 08:31:49'!
temporaries: aCollectionOfTemporaries
	"For transformations etc, not used in compilation"
	temporaries := aCollectionOfTemporaries! !


!CodeNode reorganize!
('accessing' arguments arguments: block temporaries temporaries:)
('printing' decompileString)
('testing' hasLocallyArgumentOrTemporaryNamed: hasNodeIn:named: isLocalArgumentOrTemporary:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector decompileString ifPrimitivePrintOn: methodClass printCommentOn: printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printSelectorAndArgumentsOn: printTemporariesOn: printWithClosureAnalysisOn: selectorAndArgumentsAsString sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' hasArgumentOrTemporaryNamed: hasLocalNamed: isMultipleRanges:)
!

