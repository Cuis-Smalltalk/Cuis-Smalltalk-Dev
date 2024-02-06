'From Cuis6.3 [latest update: #6206] on 5 February 2024 at 5:33:10 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 2/5/2024 17:31:16'!
browseSendersOf: aSymbol notTo: classes
	"
	Smalltalk browseSendersOf: #new: notTo: {}
	Smalltalk browseSendersOf: #new: notTo: {ArrayedCollection. CharacterSequence. Number. ThirtyTwoBitSlotsObject. BlockClosure. AdditionalMethodState. WeakMessageSend. WeakValueAssociation}
	"
	| hasMatch visitor thisMethod |
	thisMethod := nil.
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node | | receiverClass |
			(node isMessageNode and: [node selector key = aSymbol]) ifTrue: [
"thisMethod selector = #bitReverse: ifTrue: [self halt]."
				receiverClass := nil.
				Smalltalk at: node receiver name ifPresent: [ :referencedClass |
					receiverClass := referencedClass ].
				(node receiver name = #self and: [ thisMethod methodClass isMeta ]) ifTrue: [
					receiverClass := thisMethod methodClass theNonMetaClass ].
				node receiver name = '{self class}' ifTrue: [
					receiverClass := thisMethod methodClass ].
				node receiver name = '{self species}' ifTrue: [
					receiverClass := thisMethod methodClass ].
				(node receiver name = #super and: [ thisMethod methodClass isMeta ]) ifTrue: [
					receiverClass := thisMethod methodClass theNonMetaClass superclass ].
				(receiverClass isNil or: [ classes noneSatisfy: [ :c | receiverClass includesBehavior: c ] ]) ifTrue: [
					hasMatch := true ].
			]]
		select: [ :node |
			hasMatch not ].

	self
		browseAllSelect: [ :method |
			hasMatch := false.
			thisMethod := method.
			method decompile accept: visitor.
			hasMatch ]
		name: 'Senders of: ', aSymbol printString, ' but not to: ', classes printString
		autoHighlight: aSymbol.! !


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs:clearAllClassState: saveImage saveNumberedRelease saveRollingReleaseAndQuit saveRollingReleaseAndStay send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
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
('startup' displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: processCommandLineArguments processCommandLineOption:optionArgument:raiseExceptions: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!

