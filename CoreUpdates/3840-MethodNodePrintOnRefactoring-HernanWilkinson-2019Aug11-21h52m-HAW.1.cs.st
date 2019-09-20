'From Cuis 5.0 [latest update: #3839] on 11 August 2019 at 11:41:15 pm'!

!CompiledMethod methodsFor: 'decompiling' stamp: 'HAW 8/11/2019 22:36:12'!
selectorAndArgumentsAsString
	
	^self methodNode selectorAndArgumentsAsString ! !


!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 23:38:22'!
ifPrimitivePrintOn: aStream
	
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream newLineTab: 1.
			 self printPrimitiveOn: aStream]].
! !

!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 23:37:02'!
printCommentOn: aStream
	
	comment ifNotNil: [
		aStream newLineTab: 1.
		self printCommentOn: aStream indent: 1].! !

!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 22:34:14'!
printSelectorAndArgumentsOn: aStream

	| selectorNode |
	
	selectorNode _ self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords withIndexDo:
				[:kwd :i | | arg |
				arg _ arguments at: i.
				i = 1 ifFalse: [ aStream space ].
				aStream nextPutAll: kwd; space; nextPutAll: arg key ]].
! !

!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 23:37:36'!
printTemporariesOn: aStream
	
	block printTemporaries: temporaries on: aStream doPrior: [aStream newLineTab: 1].
! !

!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 22:34:14'!
selectorAndArgumentsAsString
	
	^String streamContents: [ :aStream | self printSelectorAndArgumentsOn: aStream ]! !


!MethodNode methodsFor: 'printing' stamp: 'HAW 8/11/2019 23:39:17'!
printOn: aStream

	self 
		printSelectorAndArgumentsOn: aStream;
		printCommentOn: aStream;
		printTemporariesOn: aStream;
		ifPrimitivePrintOn: aStream;
		printPropertiesOn: aStream;
		printPragmasOn: aStream.
		
	aStream newLineTab: 1.
	block printStatementsOn: aStream indent: 0! !

!methodRemoval: MethodNode #printSelectorOn:!
MethodNode removeSelector: #printSelectorOn:!
!methodRemoval: MethodNode #selectorAsString!
MethodNode removeSelector: #selectorAsString!

!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector ifPrimitivePrintOn: methodClass printCommentOn: printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printSelectorAndArgumentsOn: printTemporariesOn: printWithClosureAnalysisOn: selectorAndArgumentsAsString sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' hasArgumentOrTemporary: hasLocalNamed: isMultipleRanges:)
('*LiveTyping' actualMessageSendsSelectorPositionsOfAll:ifAbsent:withPossibleMessageSends: identifierPrintString nodeTypePrintString topBlockTempsMap topParseNodeIncluding:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
!

