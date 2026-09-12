'From Cuis7.9 [latest update: #8163] on 11 September 2026 at 3:45:48 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 11/Sep/2026, 6:10:03 pm (UTC)'!
rawSourceRangeOf: aParseNode

	^encoder rawSourceRanges at: aParseNode ifAbsent: [ nil ]! !


!CharacterSequence methodsFor: 'accessing' stamp: 'HAW 11/Sep/2026, 3:56:29 pm (UTC)'!
indentationOfLineAt: anIndex

	| lineStart indentationEnd |

	lineStart := (self
		lastIndexOf: Character lf
		startingAt: anIndex - 1
		ifAbsent: [ 0 ]) + 1.
	indentationEnd := lineStart.
	[ indentationEnd < anIndex and: [ (self at: indentationEnd) isSeparator ] ]
		whileTrue: [ indentationEnd := indentationEnd + 1 ].

	^self copyFrom: lineStart to: indentationEnd - 1! !

!CharacterSequence methodsFor: 'converting' stamp: 'HAW 11/Sep/2026, 3:52:15 pm (UTC)'!
withIndentation: anIndentation replacedBy: aNewIndentation

	(self beginsWith: anIndentation) ifFalse: [ ^self ].

	^aNewIndentation , (self allButFirst: anIndentation size)! !

!CharacterSequence methodsFor: 'converting' stamp: 'HAW 11/Sep/2026, 3:58:41 pm (UTC)'!
withIndentationOfLinesAfterTheFirst: anIndentation replacedBy: aNewIndentation

	| lines |

	lines := self lines.

	^String newLineString
		join: { lines first } , (lines allButFirst collect: [ :eachLine |
			eachLine withIndentation: anIndentation replacedBy: aNewIndentation ])! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 11/Sep/2026, 3:58:41 pm (UTC)'!
sourceCodeToExtractIndentedAsAMethodBody

	| baseIndentation |

	baseIndentation := existingMethod sourceCode indentationOfLineAt: intervalToExtract first.
	baseIndentation ifEmpty: [ ^sourceCodeToExtract ].

	^sourceCodeToExtract
		withIndentationOfLinesAfterTheFirst: baseIndentation
		replacedBy: String tab! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 11/Sep/2026, 6:10:03 pm (UTC)'!
coversCompleteAstNodesFrom: anInitialNodeStart

	^ (self trimmed: (SourceCodeInterval from: anInitialNodeStart to: finalNode value last)) = intervalToExtract! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 11/Sep/2026, 6:04:15 pm (UTC)'!
initialNodeStarts

	^ { initialNode value first. (self trimmed: initialNode value) first } asSet! !


!Parser methodsFor: 'expression types' stamp: 'HAW 11/Sep/2026, 6:10:03 pm (UTC)'!
createCascadeNodeWith: receiverNode and: messageNodes

	| sourceRangeOfFirstMessage sourceRangeOfLastMessage |

	parseNode := CascadeNode new receiver: receiverNode messages: messageNodes.
	sourceRangeOfFirstMessage := self rawSourceRangeOf: messageNodes first.
	sourceRangeOfLastMessage := self rawSourceRangeOf: messageNodes last.
	(sourceRangeOfFirstMessage notNil and: [ sourceRangeOfLastMessage notNil ]) ifTrue: [
		encoder
			noteSourceRange: (sourceRangeOfFirstMessage first to: sourceRangeOfLastMessage last)
			forNode: parseNode ]! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 11/Sep/2026, 3:50:40 pm (UTC)'!
initializeNewSourceCode

	| shouldReturnAnyVariable |
	
	shouldReturnAnyVariable := extractMethodProgrammer shouldReturnAnyVariable.

	newSourceCode := String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation.
		extractMethodProgrammer shouldDeclareAnyVariable ifTrue: [
			stream nextPutAll: extractMethodProgrammer declareTempVarsCode ].
		shouldReturnAnyVariable ifFalse: [
			stream nextPutAll: self returnCharacterIfNeeded ].
		stream
			nextPutAll: self sourceCodeToExtractIndentedAsAMethodBody.
		shouldReturnAnyVariable ifTrue: [
			stream
				newLineTab:1;
				nextPutAll: extractMethodProgrammer returnTempVarsCode ].
	].
! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 11/Sep/2026, 6:10:03 pm (UTC)'!
intervalCoversCompleteAstNodes 

	^ self initialNodeStarts anySatisfy: [ :anInitialNodeStart |
		(self coversCompleteAstNodesFrom: anInitialNodeStart)
			or: [ (anInitialNodeStart to: (self trimmed: finalNode value) last) = intervalToExtract ] ]! !

!methodRemoval: ExtractMethodNewMethodSourceCode #indentAsAMethodBody:rebasedOn: stamp: 'HAW 11/Sep/2026, 3:52:18 pm (UTC)'!
ExtractMethodNewMethodSourceCode removeSelector: #indentAsAMethodBody:rebasedOn:!
!methodRemoval: ExtractMethodNewMethodSourceCode #indentationOfTheLineWhereTheCodeToExtractStarts stamp: 'HAW 11/Sep/2026, 3:52:18 pm (UTC)'!
ExtractMethodNewMethodSourceCode removeSelector: #indentationOfTheLineWhereTheCodeToExtractStarts!
