'From Cuis 5.0 [latest update: #3962] on 4 December 2019 at 11:18:08 am'!
!classDefinition: #RenameClassApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameClassApplier
	instanceVariableNames: 'browser classToRename newClassName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Browser methodsFor: 'class list' stamp: 'HAW 12/4/2019 10:43:04'!
originalSelectedClassName
	"Returns the selectedClassName no matter if it exits or not.
	It is used for refreshing the browser when renaming a class - Hernan"

	^selectedClassName! !


!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'HAW 12/3/2019 18:08:42'!
aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #aboutToRenameClass
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'HAW 12/3/2019 18:09:31'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }! !


!SystemDictionary methodsFor: 'class names' stamp: 'HAW 12/4/2019 10:54:31'!
prepareToRenameClass: aClass as: newName 

	^self prepareToRenameClass: aClass from: aClass name to: newName! !

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 12/4/2019 10:54:28'!
prepareToRenameClass: aClass from: oldName to: newName

	"Rename the class, aClass, to have the title newName."

	| oldref i category |

	category := SystemOrganization categoryOfElement: oldName.
	SystemOrganization classify: newName under: category.
	SystemOrganization removeElement: oldName.
	oldref _ self associationAt: oldName.
	self removeKey: oldName.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i _ list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	
	SystemChangeNotifier uniqueInstance aboutToRenameClass: aClass from: oldName to: newName inCategory: category.
	! !

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 12/3/2019 18:11:03'!
renamedClass: aClass from: oldName 
	
	| newName |
	
	newName := aClass name.
	
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: aClass category.
	Smalltalk 
		logChange: 'Smalltalk renameClassNamed: #', oldName, ' as: #', newName
		preamble: 'classRenamed: #', oldName, ' as: #', newName, Utilities changeStampField! !


!CodeWindow methodsFor: 'updating' stamp: 'HAW 12/3/2019 17:05:07'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	"Do nothing here. Subclasses should implement if necessary - Hernan"! !

!CodeWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:01:38'!
initializeNotificationActions

	"Avoid double registration"
	self 
		removeNotificationActions;
		registerNotificationActionsIfModelNotNil 
! !

!CodeWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:02:26'!
registerNotificationActions

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #updateListsAndCode to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAdded send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self ! !

!CodeWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:02:39'!
registerNotificationActionsIfModelNotNil 

	"model set to nil on delete"
	model ifNotNil: [ self registerNotificationActions ] ! !

!CodeWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:01:04'!
removeNotificationActions

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
! !


!BrowserWindow methodsFor: 'updating' stamp: 'HAW 12/4/2019 10:41:57'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| currentSelectedClass |
	
	self canDiscardEdits ifTrue: [
		self model selectedSystemCategoryName = aCategoryName ifTrue: [
			currentSelectedClass := self model selectedClass.
			currentSelectedClass ifNil: [ 
				self model originalSelectedClassName = oldClassName ifTrue: [ 
					currentSelectedClass := aClass ]].
			
		 	self model changed: #classList.
			self model selectClass: currentSelectedClass ]]! !


!DebuggerWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:09:30'!
registerNotificationActions

	super registerNotificationActions.
	model when: #closeViews send: #closeView to: self ! !


!PreDebugWindow methodsFor: 'notification actions' stamp: 'HAW 12/4/2019 11:09:19'!
registerNotificationActions

	super registerNotificationActions.
	model when: #closeViews send: #closeView to: self ! !


!ChangeSet methodsFor: 'system change notifications' stamp: 'HAW 12/3/2019 18:06:13'!
aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self noteRenameClass: aClass as: newClassName! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'HAW 12/4/2019 10:48:12'!
initializeNotificationActions

	"Avoid double registration"
	self 
		removeNotificationActions;
		registerNotificationActions ! !

!ChangeSet class methodsFor: 'class initialization' stamp: 'HAW 12/4/2019 11:09:58'!
registerNotificationActions

	"Only sent when model is not nil - Hernan"
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self! !

!ChangeSet class methodsFor: 'class initialization' stamp: 'HAW 12/4/2019 10:47:36'!
removeNotificationActions

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

	! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'HAW 12/3/2019 18:06:45'!
aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			aboutToRenameClass: aClass
			from: oldClassName
			to: newClassName
			inCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !


!RenameClassApplier methodsFor: 'initialization' stamp: 'HAW 12/3/2019 17:58:34'!
initializeFor: aClass

	classToRename := aClass.
	! !


!RenameClassApplier class methodsFor: 'instance creation' stamp: 'HAW 12/3/2019 17:58:52'!
for: aClass

	^self new initializeFor: aClass! !


!Browser methodsFor: 'refactorings' stamp: 'HAW 12/3/2019 17:59:08'!
renameClass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(RenameClassApplier for: aBehavior theNonMetaClass) value ].! !


!Class methodsFor: 'class name' stamp: 'HAW 12/4/2019 10:54:09'!
safeRenameTo: newName

	| oldName |
	
	oldName := name.
	Smalltalk prepareToRenameClass: self as: newName.
	name _ newName.
	Smalltalk renamedClass: self from: oldName! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/3/2019 17:59:14'!
renameClassOn: aBrowser for: aClassToRefactor

	(RenameClassApplier for: aClassToRefactor) value! !


!CodeWindow methodsFor: 'initialization' stamp: 'HAW 12/4/2019 11:04:33'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.

	self initializeNotificationActions! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'HAW 12/4/2019 10:47:19'!
initialize
	"
	ChangeSet initialize
	"
	AllChangeSets _ OrderedCollection new.
	
	self initializeNotificationActions! !


!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'HAW 12/3/2019 17:57:58'!
showChanges

	self openChangedMethods
! !

!methodRemoval: RenameClassApplier class #on:for: stamp: 'HAW 12/3/2019 17:59:21'!
RenameClassApplier class removeSelector: #on:for:!
!methodRemoval: RenameClassApplier #initializeOn:for: stamp: 'HAW 12/3/2019 17:58:37'!
RenameClassApplier removeSelector: #initializeOn:for:!
!methodRemoval: RenameClassApplier #informChangesToBrowser stamp: 'HAW 12/3/2019 17:58:03'!
RenameClassApplier removeSelector: #informChangesToBrowser!
!methodRemoval: ChangeSet class #classRenamed:from:to:inCategory: stamp: 'HAW 12/3/2019 18:05:52'!
ChangeSet class removeSelector: #classRenamed:from:to:inCategory:!
!methodRemoval: ChangeSet #classRenamed:from:to:inCategory: stamp: 'HAW 12/3/2019 18:06:19'!
ChangeSet removeSelector: #classRenamed:from:to:inCategory:!
!methodRemoval: PreDebugWindow #model: stamp: 'HAW 12/4/2019 11:04:07'!
PreDebugWindow removeSelector: #model:!
!methodRemoval: DebuggerWindow #model: stamp: 'HAW 12/4/2019 11:04:05'!
DebuggerWindow removeSelector: #model:!
!methodRemoval: SystemDictionary #renamedClass:from:to: stamp: 'HAW 12/3/2019 17:49:51'!
SystemDictionary removeSelector: #renamedClass:from:to:!
!methodRemoval: SystemDictionary #renameClass:from:to: stamp: 'HAW 12/4/2019 10:51:11'!
SystemDictionary removeSelector: #renameClass:from:to:!
!methodRemoval: SystemDictionary #renameClass:as: stamp: 'HAW 12/4/2019 10:50:52'!
SystemDictionary removeSelector: #renameClass:as:!

!Class reorganize!
('accessing' classPool classPool: classPoolFrom: name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
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
('vm parameters' doMixedArithmetic doMixedArithmetic: vmParameterAt: vmParameterAt:put:)
!


!CodeWindow reorganize!
('GUI building' buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseProtocol browseSenders browseSendersOfMessages browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodHierarchy openSingleMessageBrowser showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerClassListMenu offerClassListMenu2 offerWhatToShowMenu openMessageListMenu openMessageListMenu2)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from:)
('initialization' model:)
('notification actions' initializeNotificationActions registerNotificationActions registerNotificationActionsIfModelNotNil removeNotificationActions)
!


!DebuggerWindow reorganize!
('GUI building' buildLowerPanes buildMorphicCodePane buildMorphicWindow customButtonRow customButtonSpecs initialExtent optionalButtonTuples windowColor)
('menu building' contextFieldListMenu contextStackMenu receiverFieldListMenu)
('menu commands' contextClassHierarchy exploreContextSelection exploreReceiverSelection receiverClassHierarchy returnValue)
('keyboard shortcuts' browseClassRefsIn: browseFullProtocolIn: browseHierarchyIn: browseMethodFullIn: contextStackKey:from: exploreSelectionIn: inspectSelectionIn: inspectorKey:from:)
('open/close' closeView delete)
('user interface' ifOkToChangeCodePaneDo: okToChangeCodePane okToChangeContextVariableInspectorText okToChangeDueTo: okToChangeReceiverInspectorText)
('context stack menu' createMethod doStep fullStack proceed restart send stepIntoBlock where)
('notification actions' registerNotificationActions)
!


!PreDebugWindow reorganize!
('GUI building' buildMorphicWindowMessage: buttonRowForPreDebugWindow: initialExtent preDebugButtonSpec windowColor)
('button actions' abandon createMethod debug delete proceed)
('open/close' closeView)
('notification actions' registerNotificationActions)
!

ChangeSet initialize!

!ChangeSet class reorganize!
('class initialization' initialize initializeNotificationActions registerNotificationActions removeNotificationActions)
('instance creation' named: new)
('system change notifications' aboutToRenameClass:from:to:inCategory: classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' baseSystemNameFor: changeSetForBaseSystem changeSetForPackage: currentBaseCSNumber incrementCurrentBaseCSNumber installing:do: installingString isInstalling: notInstallOrTestRun runningTest:do:)
('file list services' fileReaderServicesForFile:suffix: serviceFileIn serviceInstall)
('services' fileIn: install: installNewUpdates installNewUpdates: isNewUpdate: newUpdates:)
('enumerating' allChangeSets allChangeSetsWithClass:selector: changeSetNamed: countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem:)
('removing' removeChangeSet: zapAllChangeSets)
!

!classDefinition: #RenameClassApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #RenameClassApplier
	instanceVariableNames: 'classToRename newClassName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
"Postscript:
Due to changes in the rename class notification, the ChangeSet and CodeWindows
must reinitialize the notification actions"
ChangeSet initializeNotificationActions.
CodeWindow allSubInstances do: [:aCodeWindow | aCodeWindow initializeNotificationActions ].
!

