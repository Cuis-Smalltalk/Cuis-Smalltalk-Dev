'From Cuis 5.0 of 7 November 2016 [latest update: #3485] on 11 November 2018 at 7:26:03 pm'!

!Parser methodsFor: 'public access' stamp: 'HAW 11/11/2018 19:31:01'!
parse: sourceStreamOrString class: behavior noPattern: aBoolean

	^ self
		parse: sourceStreamOrString readStream
		class: behavior
		noPattern: aBoolean
		context: nil
		notifying: nil
		ifFail: [^nil]! !

!Workspace methodsFor: 'accessing' stamp: 'HAW 11/11/2018 19:24:17'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error do: aParsingErrorBlock
! !


!CodeProvider methodsFor: 'contents' stamp: 'HAW 11/11/2018 19:21:24'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode ] on: Error do: aParsingErrorBlock
	! !


!Inspector methodsFor: 'contents' stamp: 'HAW 11/11/2018 19:21:12'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode noPattern: true ] on: Error do:  aParsingErrorBlock ! !


!Behavior methodsFor: 'methods' stamp: 'HAW 11/11/2018 15:07:12'!
methodNodeFor: aSourceCode noPattern: aBoolean

	|  parser methodNode |

	parser := self parserClass new
		encoderClass: EncoderForV3PlusClosures;
		yourself.

	methodNode := parser parse: aSourceCode class: self noPattern: aBoolean.
	methodNode sourceText: aSourceCode.

	^methodNode
			! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 11/11/2018 14:27:34'!
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

!Encoder methodsFor: 'source mapping' stamp: 'HAW 11/11/2018 14:26:13'!
withRangesOf: nodeAtRange including: aPosition do: aBlock

	| currentNodeRange ranges |

	currentNodeRange := nodeAtRange value.
	ranges := currentNodeRange isInterval ifTrue: [ Array with: currentNodeRange ] ifFalse: [ currentNodeRange ].

	ranges do: [ :aRange | (aRange includes: aPosition) ifTrue: [ aBlock value: aRange ]].

	! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 11/11/2018 14:16:41'!
parseNodeIncluding: aPosition ifAbsent: aBlockClosure

	^encoder parseNodeIncluding: aPosition ifAbsent: aBlockClosure
! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 14:29:56'!
implementorsOfItWhenErrorsParsing

	"Open an implementors browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector _ self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllImplementorsOf: aSelector! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 14:15:34'!
sendersOfItWhenErrorsParsing

	"Open a senders browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector _ self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllCallsOn: aSelector! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 14:36:04'!
withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock

	self withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: [ morph flash ]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 15:30:45'!
withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

	| methodNode nodeAtRange nodeUnderCursor |

	methodNode := self codeProvider methodNodeOf: model actualContents ifErrorsParsing: [ :anError | ^ aParsingErrorBlock value: anError ].

	self startIndex < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: self startIndex ifAbsent: [ ^ aNoSelectorBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].

	^ aNoSelectorBlock value ! !


!Behavior methodsFor: 'methods' stamp: 'HAW 11/11/2018 15:07:41'!
methodNodeFor: aSourceCode

	^self methodNodeFor: aSourceCode noPattern: false! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 11/11/2018 15:21:21'!
selectorLastPosition

	"If #DoIt selector, returns 0 - Hernan"

	^self selector isUnary
		ifTrue: [ selectorKeywordsRanges ifNil: [ 0 ] ifNotNil: [ selectorKeywordsRanges last last ]]
		ifFalse: [ (encoder sourceRangeFor: arguments last) first last ]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 14:32:52'!
implementorsOfIt

	"Open an implementors browser on the  selector under cursor. If it can not parse the source code it uses
	old implementation that looks for the selected selector if any - Hernan"

	self
		withSelectorUnderCursorDo: [ :aSelector | Smalltalk browseAllImplementorsOf: aSelector ]
		ifErrorsParsing: [ :anError | self implementorsOfItWhenErrorsParsing ]

! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/11/2018 15:13:54'!
sendersOfIt

	"Open a senders browser on the  selector under cursor. If it can not parse the source code it uses
	old implementation that looks for the selected selector if any - Hernan"

	self
		withSelectorUnderCursorDo: [ :aSelector | Smalltalk browseAllCallsOn: aSelector ]
		ifErrorsParsing: [ :anError | self sendersOfItWhenErrorsParsing ]
! !


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
('auto complete' classOfBindingOf: classOfTempVarNamed: classOfWorkspaceVarNamed:)
('testing' canBindVariables is:)
('accessing' methodNodeOf:ifErrorsParsing:)
!


!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForHierarchyFor: annotationForSelector:ofClass: annotationRequests annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol: methodNodeOf:ifErrorsParsing:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is:)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shouldStyle:with:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('auto complete' classOfInstVarNamed:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is:)
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
!
