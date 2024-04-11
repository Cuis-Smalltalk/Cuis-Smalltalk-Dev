'From Cuis6.3 [latest update: #6320] on 11 April 2024 at 10:38:39 am'!

!SystemDictionary methodsFor: 'globals' stamp: 'jmv 4/11/2024 10:33:30'!
declare: aStringOrSymbol
	"Declare a global variable.
	Do nothing if aSymbol is already a global or class name.
	Smalltalk declare: #Zork
	Zork
	"
	| aSymbol |

	"If name is taken by a class, do nothing and answer nil."
	(self hasClassNamed: aStringOrSymbol)
		ifTrue: [ ^nil ].

	"Add the global name if it wasn't there.
	In any case, answer the Association. May be of use to sender."
	aSymbol := aStringOrSymbol asSymbol.
	self at: aSymbol ifAbsentPut: nil.
	^ self associationAt: aSymbol.! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 4/11/2024 10:38:17'!
hasClassNamed: aStringOrSymbol
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw
		Smalltalk hasClassNamed: 'Morph'
		Smalltalk hasClassNamed: 'Display'
		Smalltalk hasClassNamed: 'xMorph'
	Also handle a symbol as argument
		Smalltalk hasClassNamed: #Morph
		Smalltalk hasClassNamed: #Display
		Smalltalk hasClassNamed: #xMorph
	"

	Symbol hasInterned: aStringOrSymbol ifTrue: [ :aSymbol |
		self at: aSymbol ifPresent: [ :global | ^global class isMeta ]].
	^ false! !


!Parser methodsFor: 'error correction' stamp: 'jmv 4/11/2024 10:35:52'!
declareGlobal: name
	| sym |
	sym := name asSymbol.
	^encoder
		global: (Smalltalk declare: sym)
		name: sym! !


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: at:put: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs:clearAllClassState: saveImage saveNumberedRelease saveRollingReleaseAndQuit saveRollingReleaseAndStay send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:name:autoHighlight: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToSmallIntegerConstants browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive specialSelectors)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments processCommandLineOption:optionArgument:raiseExceptions: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk declare: #Totona!

