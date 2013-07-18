'From Cuis 4.1 of 12 December 2012 [latest update: #1725] on 14 June 2013 at 8:04:18.024706 pm'!

!MethodReference methodsFor: 'queries' stamp: 'jmv 6/14/2013 20:01'!
category
	^ self actualClass organization categoryOfElement: methodSymbol! !

!MethodReference methodsFor: 'queries' stamp: 'jmv 6/14/2013 20:01'!
compiledMethod
	^ self actualClass compiledMethodAt: methodSymbol! !

!MethodReference methodsFor: 'queries' stamp: 'jmv 6/14/2013 20:01'!
sourceCode
	^ self actualClass sourceCodeAt: methodSymbol! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 6/14/2013 20:03'!
upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given coellection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (collection species new: 100).
	[self atEnd or: [aCollection includes: (element := self next)]]
		whileFalse: [newStream nextPut: element].
	(aCollection includes: element)
		ifTrue: [self skip: -1].
	^newStream contents! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 6/14/2013 20:02'!
nextNumber
	"Answer a number from the stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $)]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !


!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isEmpty notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: copyMethodChunkFrom: fileIn fileInAnnouncing: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('nonhomogeneous accessing' nextInt32 nextInt32Put: nextLittleEndianNumber: nextLittleEndianNumber:put: nextNumber nextNumber: nextNumber:put: nextString nextStringOld nextStringPut: nextWord nextWordPut:)
('converting' asBinaryOrTextStream)
('data get/put' boolean boolean: int16 int16: int32 int32: string string: uint16 uint16: uint24 uint24: uint32 uint32:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
!


!MethodReference reorganize!
('queries' actualClass asStringOrText category classIsMeta classSymbol compiledMethod isValid methodSymbol printOn: selector sourceCode)
('setting' setClass:methodSymbol:stringVersion: setClassAndSelectorIn: setClassSymbol:classIsMeta:methodSymbol:stringVersion: setStandardClass:methodSymbol:)
('string version' stringVersion stringVersion:)
('comparisons' <= = hash)
('services' updateReferencesTo:toBe:)
!


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend terminate)
('changing suspended state' activateReturn:value: complete: completeStep: completeTo: install: popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString)
('private' suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug:title: debug:title:full: debugFullWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('test support' suspendPrimitivelyOrFail)
('UI support' animatedUI animatedUI:)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClassNamed:as:)
('dictionary access' associationAtOrAbove:ifAbsent: associationOrUndeclaredAt: at:put: atOrAbove:ifAbsent: environmentForCategory: includesKeyOrAbove: scopeFor:from:envtAndPathIfFound:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources fixSourceCodeLineEndings macroBenchmark1 macroBenchmark3 makeInternalRelease obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories removeTextCode testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primVmFileName setMacFileInfoOn: unloadModule: vmFileName vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjectsDo: allObjectsSelect: allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: pointersToEachIn: pointersToItem:of: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition okayToDiscardUnsavedCodeSaving: okayToSave processShutDownList: processStartUpList: quitPrimitive readDocumentFile removeFromShutDownList: removeFromStartUpList: saveAs saveAs: saveAsEmbeddedImage saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences shutDown snapshot:andQuit: snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser snapshot:andQuit:embedded:)
('ui' beep confirmRemovalOf:on: inspectGlobals)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllObjectReferencesTo:except:ifNone: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Compiler swapping' actualCompiledMethodWithNodeClass actualDecompilerClass actualEncoderClass actualParserClass actualScannerClass)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: namesBeginningWith:do: namesBeginningWith:do:in:)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
!


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // raisedTo: \\)
('comparing' hash)
('converting' asArray asBag asByteArray asCharacterSet asIdentitySet asIdentitySkipList asOrderedCollection asSet asSkipList asSkipList: asSortedArray asSortedCollection asSortedCollection:)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan average ceiling cos degreeCos degreeSin exp floor ln log max median min negated range reciprocal roundTo: rounded sign sin sqrt squared sum sum: tan truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionToSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed toBraceStack:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted:)
!

