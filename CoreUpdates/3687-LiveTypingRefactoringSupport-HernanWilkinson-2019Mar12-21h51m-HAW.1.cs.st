'From Cuis 5.0 of 7 November 2016 [latest update: #3659] on 18 March 2019 at 2:46:24 pm'!
!classDefinition: #RefactoringApplier category: #'Tools-Refactoring'!
Object subclass: #RefactoringApplier
	instanceVariableNames: 'requestExitBlock refactoring changes '
	classVariableNames: 'Appliers '
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ClassDescription methodsFor: 'compiling' stamp: 'HAW 3/12/2019 22:09:16'!
localBindingOf: varNameSymbol

	self subclassResponsibility ! !


!Metaclass methodsFor: 'compiling' stamp: 'HAW 3/12/2019 22:08:40'!
localBindingOf: varNameSymbol

	^thisClass localBindingOf: varNameSymbol ! !


!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 3/13/2019 18:05:09'!
browseMessageList: messageList ofSize: messageListSize name: labelString autoSelect: autoSelectString

	| title |

	"Create and schedule a MessageSet browser on the message list."

	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MessageSetWindow
		openMessageList: messageList
		label: title
		autoSelect: autoSelectString! !


!Stream methodsFor: 'accessing' stamp: 'HAW 3/13/2019 20:09:02'!
nextPutAll: aCollection asCommaSeparated: aPrintBlock

	aCollection asCommaSeparated: aPrintBlock on: self! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/15/2019 14:38:44'!
rename: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor name ].

	self ifRenameCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [ ^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [ ^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | potentialBehavior |
			potentialBehavior := aNodeUnderCursor key value.
			potentialBehavior isBehavior ifTrue: [ ^self renameClassOn: self codeProvider for: potentialBehavior theNonMetaClass ]].

		^morph flash ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/15/2019 14:41:17'!
renameSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector
	
	RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model textProvider of: aMessageNode in: aSelectedClass at: aSelectedSelector! !


!RefactoringApplier class methodsFor: 'initialization' stamp: 'HAW 3/12/2019 22:04:08'!
initialize

	Appliers := IdentityDictionary new.! !

!RefactoringApplier class methodsFor: 'appliers' stamp: 'HAW 3/12/2019 22:03:31'!
registerRenameSelectorApplier: aRenameSelectorApplierClass

	Appliers at: #renameSelectorApplier put: aRenameSelectorApplierClass name! !

!RefactoringApplier class methodsFor: 'appliers' stamp: 'HAW 3/12/2019 22:05:45'!
renameSelectorApplier
	
	^Appliers 
		at: #renameSelectorApplier 
		ifPresent: [ :anApplierName | Smalltalk classNamed: anApplierName ]
		ifAbsent: [ RenameSelectorApplier ]! !

!RefactoringApplier class methodsFor: 'appliers' stamp: 'HAW 3/12/2019 22:12:54'!
resetRenameSelectorApplier
	
	Appliers removeKey: #renameSelectorApplier ifAbsent: []! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/12/2019 22:26:11'!
initializeImplementorsAndSenders

	implementors := IdentitySet new.
	senders := IdentitySet new.
! !


!RenameSelectorApplier methodsFor: 'as yet unclassified' stamp: 'HAW 3/14/2019 18:08:53'!
sendersFrom: methodReferences

	^ methodReferences collect: [:aMethodReference | aMethodReference compiledMethod ]! !


!RenameSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 3/12/2019 22:20:18'!
createAndValueHandlingExceptionsOn: aModel for: anOldSelector in: aClassToRefactor

	self createAndValueHandlingExceptions: [ self on: aModel for: anOldSelector in: aClassToRefactor ]! !

!RenameSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 3/15/2019 13:54:21'!
createAndValueHandlingExceptionsOn: aModel of: aMessageNode in: aSelectedClass 
	
	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass ! !


!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 3/13/2019 18:05:40'!
browseMessageList: messageList name: labelString autoSelect: autoSelectString
	
	^self browseMessageList: messageList ofSize: messageList size name: labelString autoSelect: autoSelectString! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/15/2019 13:47:38'!
contextualRename

	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualRenameOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ].

		! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/15/2019 14:36:50'!
contextualRenameOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode selector ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifRenameCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]


! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/12/2019 22:15:38'!
renameSelectorFor: aSelector in: aClassToRefactor

	RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model textProvider for: aSelector in: aClassToRefactor ! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/15/2019 13:47:20'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: model actualContents ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/12/2019 22:15:54'!
renameSelector

	model selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ]! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 3/14/2019 18:08:17'!
changeRequestSenders
	
	applier sendersFrom: model messageList
	! !


!AddInstanceVariable methodsFor: 'accessing' stamp: 'HAW 3/14/2019 09:11:51'!
classToRefactor

	^classToRefactor ! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/12/2019 22:25:49'!
askForImplementosAndSenders

	self 
		askScope;
		initializeImplementorsAndSenders;
		calculateImplementorsAndSenders;
		startWizard ! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/12/2019 22:25:29'!
calculateImplementorsAndSenders
		
	scopeChoice = 1 ifTrue: [ ^self implementorsAndSendersForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndSendersForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndSendersInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndSendersInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndSendersInSystem ].
	
	self error: 'Unknown scope option' 
		
		! !


!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 3/15/2019 15:02:08'!
on: aBrowser for: aSelector in: aClass
	
	self assertCanApplyRefactoringFor: aSelector in: aClass.
	
	^self new initializeOn: aBrowser for: aSelector in: aClass
	! !


!RenameSelectorApplier methodsFor: 'refactoring - creation' stamp: 'HAW 8/18/2018 16:56:24'!
createRefactoring
	
	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	! !

!methodRemoval: RenameSelector class #addActualImplementorsOf:in:to:andActualSendersTo:inSystem:!
RenameSelector class removeSelector: #addActualImplementorsOf:in:to:andActualSendersTo:inSystem:!
!methodMoveToSomePackage: ChangeSelector class #addActualImplementorsOf:in:to:andActualSendersTo:inSystem:!
ChangeSelector class removeSelectorIfInBaseSystem: #addActualImplementorsOf:in:to:andActualSendersTo:inSystem:!
!methodRemoval: SmalltalkEditor #rename:in:!
SmalltalkEditor removeSelector: #rename:in:!
!methodRemoval: SmalltalkEditor #renameSelectorOf:in:!
SmalltalkEditor removeSelector: #renameSelectorOf:in:!

!CompiledMethod reorganize!
('accessing' bytecodeSetName clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' hasArgumentOrTemporary: hasNewPropertyFormat hasReportableSlip is: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isTestMethod readsInstanceVariable: referencesParameterAt: sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreambleFrom:at: getSource getSourceFor:in: getSourceFromFile linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceClass sourceCode sourceFileStream sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('decompiling' compilerClass createMethodNode decompile decompilerClass methodForDecompile methodNode parserClass)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('as yet unclassified' methodNode:)
('*LiveTyping' actualSendersOf: allRawTypesUnload: caretAutoCompleterDocumentation copyTypeInformationFrom:using: doesOnlySurelySend: initializeTypeInformation initializeTypeInformationUsing: isQuickReturnSelf liveTypingAutoCompleterDocumentation methodVariablesTypes parameterTypeAutoCompleterDocumentationOf:using: printTypeInfoOn: returnRawTypes returnTypeAutoCompleterDocumentation returnTypeInfo returnTypes typeAnnotationsHolder typeInfoOfVariableNamed: typeInfoOfVariableNamed:ifAbsent: typesOfInstanceVariableNamed: typesOfInstanceVariableNamed:ifAbsent: typesOfVariableNamed: typesOfVariableNamed:ifAbsent: variablesRawTypes)
('*DenotativeObject' printTheNonMetaClassAndSelectorOn:)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageList:ofSize:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('*LiveTyping' actualImplementorsOf:in: allActualSendersOn: allActualSendersOn:forAll: browseAllActualImplementorsOf:for: browseAllActualImplementorsOf:forAll: browseAllActualSendersOn: browseAllActualSendersOn:forAll: browseAllActualmplementorsOf:)
('*DenotativeObject' isDenotativeObjectNamed:)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap sourceRangeFor: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
('*LiveTyping' actualMessageSendsSelectorPositionsOfAll:ifAbsent:withPossibleMessageSends: topParseNodeIncluding:ifAbsent:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' hasArgumentOrTemporary: hasLocalNamed:)
('*LiveTyping' actualMessageSendsSelectorPositionsOfAll:ifAbsent:withPossibleMessageSends: identifierPrintString nodeTypePrintString topBlockTempsMap topParseNodeIncluding:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
!


!ChangeSelector class reorganize!
('pre-conditions' assertAllImplementors:haveSame: assertAllSenders:send:)
('exceptions' errorMessageForInvalidImplementors: errorMessageForInvalidSenders:of: signalInvalidImplementors: signalInvalidSenders:of:)
('*LiveTyping' addActualImplementorsOf:forTypes:to:andActualSendersTo:inSystem: addActualImplementorsOf:in:to:andActualSendersTo:inSystem:)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of:)
!


!RenameSelector class reorganize!
('pre-conditions' assert:hasTheSameNumberOfArgumentsAs: assert:isNotEqualTo: assert:isOfSameTypeAs: assertIsNotEmpty:signalMessageText: assertIsValidToRenameFrom:to: assertNoImplementorClassIn:implements: isRenamigBetweenBinary:andKeywordOfOneParameter: warnIfImplementionsOf:overridesImplementationInSuperclassesOf:)
('exceptions' errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs: errorMessageForNewSelector:implementedInAll: errorMessageForNewSelector:isNotOfSameTypeAs: implementorsCanNotBeEmptyErrorMessage newSelectorCanNotBeEmptyErrorMessage newSelectorEqualToOldSelectorErrorMessage oldSelectorCanNotBeEmptyErrorMessage signalNewSelector:doesNotHaveSameNumberOfArgumentsAs: signalNewSelector:implementedInAll: signalNewSelector:isNotOfSameTypeAs: signalNewSelectorEqualToOldSelector)
('instance creation' from:to:implementors:senders: from:to:in: from:to:inCategoriesAndHierarchyOf:organizedBy: from:to:inCategoryOf:organizedBy: from:to:inHierarchyOf: from:to:inSystem:)
('warnings' warnImplementionOf:in:willOverrideImplementationIn: warningMessageForImplementationOf:in:willOverrideImplementationIn:)
!

!classDefinition: #RefactoringApplier category: #'Tools-Refactoring'!
Object subclass: #RefactoringApplier
	instanceVariableNames: 'requestExitBlock refactoring changes'
	classVariableNames: 'Appliers'
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
RefactoringApplier initialize!

!RefactoringApplier class reorganize!
('value handling exceptions' createAndValueHandlingExceptions:)
('initialization' initialize)
('appliers' registerRenameSelectorApplier: renameSelectorApplier resetRenameSelectorApplier)
!


!RenameSelectorApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
('instance creation' createAndValueHandlingExceptionsOn:for:in: createAndValueHandlingExceptionsOn:of:in:)
!

