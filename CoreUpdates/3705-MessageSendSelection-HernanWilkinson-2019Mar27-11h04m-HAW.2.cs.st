'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 18 April 2019 at 8:56:36 pm'!
!classDefinition: #TextModelMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #TextModelMorph
	instanceVariableNames: 'shouldAttendActualScopeSenderChanged '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!MessageSet methodsFor: 'source code ranges' stamp: 'HAW 4/18/2019 20:42:29'!
messageSendsRangesOf: aSelector

	^ selectedMessage 
		ifNil: [ #() ]
		ifNotNil: [selectedMessage messageSendsRangesOf: aSelector ]! !


!CompiledMethod methodsFor: 'source code ranges' stamp: 'HAW 4/18/2019 20:41:51'!
messageSendsRangesOf: aSentSelector

	| methodNode ranges |
	
	methodNode := self methodNode.
	ranges := OrderedCollection new.

	methodNode nodesDo: [ :aParseNode |
		(aParseNode isMessageNamed: aSentSelector) ifTrue: [ ranges add: (methodNode sourceRangeFor: aParseNode) ]].
	
	^ranges ! !


!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:51:03'!
selectMessage

	| messageSendsRanges |

	messageSendsRanges := model textProvider messageSendsRangesOf: model autoSelectString.
	self editor messageSendsRanges: messageSendsRanges.
! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:51:08'!
selectString

	self editor
		setSearch: model autoSelectString;
		findAndReplaceMany: true ! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:34:31'!
updateAcceptedContents

	self textMorph hasUnacceptedEdits ifTrue: [
		self textMorph hasEditingConflicts: true.
		^self redrawNeeded ].
	model refetch.
	"#actualContents also signalled in #refetch. No need to repeat what's done there."
	^self ! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:33:38'!
updateActualContents 

	"Some day, it would be nice to keep objects and update them
	instead of throwing them away all the time for no good reason..."
	self textMorph
		releaseEditorAndTextComposition;
		installEditorAndTextComposition;
		formatAndStyleIfNeeded.
	self setScrollDeltas.
	self redrawNeeded.
	^self ! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:53:31'!
updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!!"
	(model autoSelectString isKindOf: Symbol)
		ifTrue: [ self selectMessage ]
		ifFalse: [ self selectString].

	self textMorph updateFromTextComposition.
	^self scrollSelectionIntoView! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:37:48'!
updateClearUserEdits
	
	"Quite ugly"
	^self hasUnacceptedEdits: false
! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:36:13'!
updateInitialSelection

	^self 
		setSelection: model getSelection; 
		redrawNeeded! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:35:31'!
updateRefetched 

	"#actualContents also signalled when #refetched is signalled.
	No need to repeat what's done there."
	self setSelection: model getSelection.
	self hasUnacceptedEdits: false.
	
	^self ! !

!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:38:23'!
updateShoutStyled

	self textMorph stylerStyled.
	^self redrawNeeded ! !


!MethodReference methodsFor: 'source code ranges' stamp: 'HAW 4/18/2019 20:17:52'!
messageSendsRangesOf: aSentSelector

	| compiledMethod |
	
	compiledMethod := self compiledMethodIfAbsent: [ ^#() ].
	^compiledMethod messageSendsRangesOf: aSentSelector ! !


!CompiledMethod methodsFor: 'decompiling' stamp: 'HAW 4/18/2019 20:40:33'!
methodNode: aMethodNode

	self propertyValueAt: #methodNode put: aMethodNode! !


!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 4/18/2019 20:10:24'!
browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral
				autoSelect: aLiteral ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 4/18/2019 20:29:53'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key , ' local to ' , aClass name
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral , ' local to ' , aClass name
				autoSelect: aLiteral ].! !


!TextEditor methodsFor: 'new selection' stamp: 'HAW 4/18/2019 20:56:26'!
messageSendsRanges: aRanges

	selectionStartBlocks := OrderedCollection new.
	selectionStopBlocks := OrderedCollection new.
	
	aRanges do: [ :range | 
		selectionStartBlocks add: (textComposition characterBlockForIndex: range first).
		selectionStopBlocks add: (textComposition characterBlockForIndex: range last + 1) ]! !


!TextModelMorph methodsFor: 'updating' stamp: 'HAW 4/18/2019 20:39:08'!
update: aSymbol

	super update: aSymbol.
	aSymbol ifNil: [^self].

	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [ ^self updateActualContents ].
	aSymbol == #acceptedContents ifTrue: [ ^self updateAcceptedContents ].
	aSymbol == #refetched ifTrue: [ ^self updateRefetched ].
	aSymbol == #initialSelection ifTrue: [ ^self updateInitialSelection ].
	aSymbol == #autoSelect ifTrue: [ ^self updateAutoSelect ].
	aSymbol == #clearUserEdits ifTrue: [ ^self updateClearUserEdits ].
	aSymbol == #shoutStyled ifTrue: [ ^self updateShoutStyled ].
! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 1/1/2015 21:05'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MethodReference methodsFor: 'queries' stamp: 'HAW 4/18/2019 20:17:23'!
compiledMethodIfAbsent: ifAbsentBlock

	^ self actualClass compiledMethodAt: methodSymbol ifAbsent: ifAbsentBlock ! !


!RefactoringApplier methodsFor: 'exception handling' stamp: 'HAW 4/15/2019 15:41:14'!
browseReferencesOn: aCanNotRefactorDueToReferencesError
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoSelect: aCanNotRefactorDueToReferencesError referencee name asString
! !


!MessageSet reorganize!
('message list' addMethodReference: addMethodReference:ifIncluded: growable messageList messageListIndex: removeMessageFromBrowser removeMessageFromBrowserKeepingLabel selectedMessage selectedMessageName sortByDate)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText)
('private' autoSelectString autoSelectString: initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' contents:notifying: labelString reformulateListNoting:)
('shout styling' shouldStyle:with:)
('source code ranges' messageSendsRangesOf:)
!


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
('decompiling' compilerClass createMethodNode decompile decompilerClass methodForDecompile methodNode methodNode: parserClass)
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
('source code ranges' messageSendsRangesOf:)
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
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageList:name:prefixedWith:autoSelect: browseMessageList:ofSize:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
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
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: messageSendsRanges: selectFrom:to: selectInterval: selectLine)
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

!classDefinition: #TextModelMorph category: #'Morphic-Views'!
PluggableScrollPane subclass: #TextModelMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!TextModelMorph reorganize!
('accessing' disableEditing editor enableEditing textMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' scrollSelectionIntoView selectAll)
('events' clickAndHalf:localPosition: doubleClick:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: defaultColor escAction: innerMorphClass model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' selectMessage selectString update: updateAcceptedContents updateActualContents updateAutoSelect updateClearUserEdits updateInitialSelection updateRefetched updateShoutStyled)
!


!MethodReference reorganize!
('queries' actualClass category classIsMeta classSymbol compiledMethod compiledMethodIfAbsent: isValid methodSymbol selector sourceCode)
('setting' setClass:methodSymbol:stringVersion: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
('printing' displayStringOrText printClassAndSelectorOn: printOn:)
('auto complete' dynamicTypingAutoCompleterDocumentation methodClass)
('source code ranges' messageSendsRangesOf:)
!

