'From Cuis 5.0 of 7 November 2016 [latest update: #3162] on 4 September 2017 at 5:01:33 pm'!
!classDefinition: #ChangeListWithFileInErrors category: #'Tools-Changes'!
ChangeList subclass: #ChangeListWithFileInErrors
	instanceVariableNames: 'errors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeListWithFileInErrors commentStamp: 'HAW 9/4/2017 10:23:05' prior: 0!
This class is used to keep errors when filing in changes.
I could have use ChangeList directly, selecting changes with errors, then removing them, etc., but it had some problems and that solution is more a hack.
So, instances of this class will keep errors when filing in a change, and it allows the posibility to show the change with the error in a change list window.

A doit change that signaled a MessageNotUnderstood is assume to not be an error becuase those kinds of things are evaluations in specific contexts that will obiously generate errors.
All doits with errors could be assume not to be errors, but I limited to MNU type of errors to avoid filtering errors that should be shown.!


!ChangeListWithFileInErrors methodsFor: 'filing in' stamp: 'HAW 9/4/2017 09:34:35'!
fileInAllKeepingErrors

	errors := Dictionary new.
	changeList do: [ :change | self fileInKeepingError: change ].
! !

!ChangeListWithFileInErrors methodsFor: 'filing in' stamp: 'HAW 9/4/2017 16:36:13'!
fileInKeepingError: change
	
	[ change fileIn ]
		on: Error 
		do: [ :anError | (self hasToKeep: anError for: change) ifTrue: [ errors at: change put: anError ]]! !

!ChangeListWithFileInErrors methodsFor: 'initialization-release' stamp: 'HAW 9/4/2017 09:34:20'!
initialize

	super initialize.
	errors := Dictionary new.! !

!ChangeListWithFileInErrors methodsFor: 'testing' stamp: 'HAW 9/4/2017 09:37:00'!
hasFileInErrors

	^errors notEmpty! !

!ChangeListWithFileInErrors methodsFor: 'testing' stamp: 'HAW 9/4/2017 16:37:07'!
hasToKeep: anError for: change

	^(change isDoIt and: [ anError isKindOf: MessageNotUnderstood ]) not! !

!ChangeListWithFileInErrors methodsFor: 'showing filein errors' stamp: 'HAW 9/4/2017 10:01:06'!
failedFileInChangesLabel

	^'Changes that failed to file in'! !

!ChangeListWithFileInErrors methodsFor: 'showing filein errors' stamp: 'HAW 9/4/2017 10:45:43'!
ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList 
	
	(self wasFiledInWithError: aChange) ifTrue: [ 
		newChangeList add: aChange.
		newList add: ((list at: anIndex) contractTo: 40), ' | ', (errors at: aChange) printString ]! !

!ChangeListWithFileInErrors methodsFor: 'showing filein errors' stamp: 'HAW 9/4/2017 10:04:47'!
removeSucessfullyFiledInChanges

	| newChangeList newList |

	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	changeList withIndexDo: [ :aChange :anIndex | self ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList ].
			
	changeList _ newChangeList.
	list _ newList.
	listIndex _ 0.
	self clearSelections.
	self changed: #list.! !

!ChangeListWithFileInErrors methodsFor: 'showing filein errors' stamp: 'HAW 9/4/2017 10:02:20'!
showChangesWithFileInErrors

	self removeSucessfullyFiledInChanges.
	ChangeListWindow open: self label: self failedFileInChangesLabel

! !

!ChangeListWithFileInErrors methodsFor: 'showing filein errors' stamp: 'HAW 9/4/2017 10:46:01'!
wasFiledInWithError: aChange
	
	^errors includesKey: aChange! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:48:40'!
logSnapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	(SourceFiles at: 2) ifNotNil: [
		msg _ self snapshotMessageFor: save andQuit: quit.
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:42:57'!
nopTag

	^ 'NOP'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:42:27'!
quitNoSaveTag

	^ 'QUIT/NOSAVE' ! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:40:45'!
quitTag
		
	^'QUIT'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:51:51'!
snapshotMessageFor: save andQuit: quit
		
	^String streamContents: [ :stream |
		stream 
			nextPutAll: self tagHeader;
			nextPutAll: (self snapshotTagFor: save andQuit: quit);
			nextPutAll: self tagTail;
			print: Date dateAndTimeNow;
			space;
			nextPutAll: self imageName asFileEntry name;
			nextPutAll: ' priorSource: ';
			print: LastQuitLogPosition ].! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:41:19'!
snapshotTag
		
	^'SNAPSHOT'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:51:20'!
snapshotTagFor: save andQuit: quit
		
	^save
		ifTrue: [ quit
			ifTrue: [ self quitTag ]
			ifFalse: [ self snapshotTag ]]
		ifFalse: [ quit
			ifTrue: [ self quitNoSaveTag ]
			ifFalse: [ self nopTag ]]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:45:54'!
tagHeader

	^ '----'! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:46:02'!
tagTail

	^ self tagHeader! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:11:51'!
hasToRestoreChanges

	^self withChangesFileDo: [ :changesFile |
		changesFile position: self lastQuitLogPosition.
		self hasToRestoreChangesFrom: changesFile ].
	! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 16:41:24'!
hasToRestoreChangesFrom: changesFile

	| chunk  |
	
	[ changesFile atEnd ] whileFalse: [ chunk := changesFile nextChunk ].
	
	^(self isSnapshotQuitOrQuitNoSaveRecord: chunk) not
! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 08:56:02'!
isQuitNoSaveRecord: chunk

	^chunk beginsWith: self tagHeader, self quitNoSaveTag
	! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 08:55:51'!
isQuitRecord: chunk

	^chunk beginsWith: self tagHeader, self quitTag ! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 16:34:34'!
isSnapshotQuitOrQuitNoSaveRecord: chunk

	^(self isSnapshotRecord: chunk) 
		or: [ (self isQuitRecord: chunk) 
		or: [ self isQuitNoSaveRecord: chunk ]]! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 08:57:09'!
isSnapshotRecord: chunk

	^chunk beginsWith: self tagHeader, self snapshotTag 
	! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 09:07:35'!
lostChangesDetectedCaption
	
	^
'Last changes may have been lost
(maybe the VM crashed or you had to kill it)
What do you want to do?'! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:25:45'!
restoreLostChanges

 	| decision |
	
	decision := PopUpMenu withCaption: self lostChangesDetectedCaption chooseFrom: self restoreLostChangesOptions.

	decision = 1 ifTrue: [ ^self restoreLostChangesAutomatically ].
	decision = 2 ifTrue: [ ^self restoreLostChangesManually ]! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:13:01'!
restoreLostChangesAutomatically
	
	self withChangesFileDo: [ :aChangesFile | self restoreLostChangesAutomaticallyFrom: aChangesFile ]! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 10:15:16'!
restoreLostChangesAutomaticallyFrom: aChangesFile

	| changeList |

	changeList := ChangeListWithFileInErrors new.
	changeList scanFile: aChangesFile from: LastQuitLogPosition to: aChangesFile size.
	changeList fileInAllKeepingErrors.
	(changeList hasFileInErrors and: [ self shouldShowFileInErrors ]) ifTrue: [ changeList showChangesWithFileInErrors ]
! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:09:39'!
restoreLostChangesIfNecessary

	"
	Smalltalk restoreLostChangesIfNecessary
	"
	
	self hasToRestoreChanges ifTrue: [ WorldState addDeferredUIMessage: [self restoreLostChanges ]].
! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/1/2017 17:28:22'!
restoreLostChangesManually

	ChangeList browseRecentLog! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:25:55'!
restoreLostChangesOptions
	
	^{'Restore lost changes automatically'. 'Restore lost changes manually'. 'Nothing'}.! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:29:46'!
restoringChangesHasErrorsCaption
	
	^'There were errors filing in the lost changes. Do you want to see them?'! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 09:37:34'!
shouldShowFileInErrors
	
 	^self confirm: self restoringChangesHasErrorsCaption
	! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/4/2017 06:11:39'!
withChangesFileDo: aBlock

	^self currentChangesName asFileEntry readStreamDo: aBlock! !


!ChangeRecord methodsFor: 'testing' stamp: 'HAW 9/4/2017 06:32:29'!
isDoIt

	^type = #doIt! !


!ChangeList methodsFor: 'menu actions' stamp: 'HAW 9/4/2017 10:26:32'!
removeDoIts
	"Remove doits from the receiver, other than initializes. 1/26/96 sw"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	changeList with: list do: [ :chRec :str |
		(chRec isDoIt not or: [str endsWith: 'initialize'])
			ifTrue: [
				newChangeList add: chRec.
				newList add: str]].
	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list.

	! !

!ChangeList methodsFor: 'menu actions' stamp: 'HAW 9/4/2017 10:26:50'!
selectRemovalsOfSent
	"Selects all method removal for sent methods"

	1 to: changeList size do: [ :i | | change |
		change _ changeList at: i.
		listSelections at: i put:
			(change isDoIt and: [
				change string includesSubString: 'removeSelector: #' ] and: [
					Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]) ].
	self changed: #allSelections.
	self changed: #annotation! !


!ChangeList class methodsFor: 'public access' stamp: 'HAW 9/4/2017 10:32:00'!
browseRecentLogOn: origChangesFileName 
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions size > 0 ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((Smalltalk isQuitRecord: chunk) or: [ Smalltalk isSnapshotRecord: chunk ])
							ifTrue: [ positions add: pos ]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	positions isEmpty
		ifTrue: [self inform: 'File ' , origChangesFileName , ' does not appear to be a changes file']
		ifFalse: [self browseRecentLogOn: origChangesFileName startingFrom: positions last]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 9/4/2017 08:46:37'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
| activeProc |
activeProc _ Processor activeProcess.
[ 
	| isARealStartup world reopenTranscript |
	self logSnapshot: save andQuit: quit.

	reopenTranscript _ false.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each | each isInWorld ifTrue: [ each delete. reopenTranscript _ true ]].
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.


	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	
	"Cosas que levanto explicitamente abajo"
	world _ ProjectX ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"world _ clearAllStateFlag ifFalse: [ ProjectX ui ]."
	ProjectX stopUIProcess.
	activeProc isTerminated ifFalse: [
		activeProc terminate ].

	"Clean Globals"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: nil.
	Smalltalk closeSourceFiles. Smalltalk at: #SourceFiles put: nil.
	Smalltalk allClassesDo: [ :cls | cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [	
		Smalltalk allClassesDo: [ :cls | cls releaseClassState ]].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


	"Do image save & quit as apropriate"
	Cursor write activateCursor.
	save
		ifTrue: [
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].


	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: (DisplayScreen extent: `1024@768` depth: 32).
	Smalltalk at: #SourceFiles put: (Array new: 2).
	Smalltalk openSourceFiles.

	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self doStartUp: isARealStartup == true.
	
	
	ProjectX spawnNewMorphicProcessFor: (world ifNil: [ PasteUpMorph newWorld ]).

	reopenTranscript ifTrue: [
		WorldState addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	"
	WorldState addDeferredUIMessage: [
		world fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [	
		WorldState addDeferredUIMessage: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !

!SystemDictionary methodsFor: 'startup' stamp: 'HAW 9/4/2017 06:14:44'!
doStartUp: isARealStartup
	"
	isARealStartup
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal activateCursor.
	self setGCParameters.
	isARealStartup ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self restoreLostChangesIfNecessary ]! !


!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 9/4/2017 10:27:15'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	self isDoIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]! !

!methodRemoval: SystemDictionary #logSapshot:andQuit:!
SystemDictionary removeSelector: #logSapshot:andQuit:!

!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: namesBeginningWith:do: namesBeginningWith:do:in:)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
!

