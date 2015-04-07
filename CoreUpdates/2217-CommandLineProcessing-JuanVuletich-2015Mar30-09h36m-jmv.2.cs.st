'From Cuis 4.2 of 25 July 2013 [latest update: #2214] on 30 March 2015 at 11:05:46.018 am'!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments '
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/27/2015 17:52'!
quitPrimitive: exitCode
	"Primitive. Exit with exitCode to another operating system on the host machine, if one exists.
	All state changes in the object space since the last snapshot are lost.
	Ignore exitCode if it's not supported by the VM."

	<primitive: 113>
	self quitPrimitive! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/30/2015 11:05'!
processCommandLineArgument: rawArgStream
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data |
	p _ rawArgStream next.
	p first = $- ifFalse: [
		^self ].
	p size < 2 ifTrue: [
		^self ].
	"If the command is not included in p, it is next argument"
	p size < 3
		ifTrue: [
			"as in 		-r RequiredFeature1"
			data _ rawArgStream next ]
		ifFalse: [
			"as in 		-rRequiredFeature2"
			data _ p copyFrom: 3 to: p size ].
	p second caseOf: {
		[ $r ] -> [		"as in 		-rRequiredFeature2"
			{ 'Feature require: '. data } print.
			[Feature require: data] on: Error do: [] ].
		[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. data } print.
			[Compiler evaluate: data] on: Error do: [] ].
		[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			startUpScriptArguments _ OrderedCollection new.
			[ rawArgStream atEnd ] whileFalse: [
				startUpScriptArguments add: rawArgStream next ].
			"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
			{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
			(FileStream oldFileOrNoneNamed: data) ifNotNil: [ :stream |
				[ Compiler evaluate: stream contentsOfEntireFile ] on: Error do: []].
			"Maybe we decide to clear them after startup script execution
			startUpScriptArguments _ nil" ]
	}
	otherwise: []! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/30/2015 10:48'!
processCommandLineArguments
	"
	Smalltalk processCommandLineArguments
	"
	| arg i rawArguments |
	rawArguments _ OrderedCollection new.
	i _ 2.
	[ i <= 1000 and: [ (arg _ self getSystemAttribute: i) notNil ] ] whileTrue: [
		rawArguments add: arg.
		i _ i + 1 ].
	rawArguments print.
	rawArguments _ rawArguments readStream.
	[ rawArguments atEnd ] whileFalse: [
		self processCommandLineArgument: rawArguments ]! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/27/2015 17:27'!
setStartupStamp

	StartupStamp _ '----STARTUP----', Time dateAndTimeNow printString, ' as ', Smalltalk imageName! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/27/2015 18:40'!
startUpScriptArguments
	"To make command line arguments to startup scripts available to them"

	^startUpScriptArguments! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 3/27/2015 17:31'!
startup: resuming
	"
	resuming
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal show.
	self setGCParameters.
	resuming ifTrue: [
		self clearExternalObjects ].
	self processStartUpList: resuming.
	resuming ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp.
		self processCommandLineArguments ].! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 3/30/2015 10:44'!
getSystemAttribute: attributeID
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		full path name of this image (better use primImageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"

	<primitive: 149>
	^ nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/27/2015 17:31'!
snapshot: save andQuit: quit embedded: embeddedFlag
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	| resuming msg |
	ActiveModel flushEventSystem.
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: (FileDirectory default localNameFor: self imageName);
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ].
	self processShutDownList: quit.
	Cursor write show.
	save
		ifTrue: [
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].

	"Here, startup begins!! (resuming might be nil)"
	self startup: resuming == true.

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	^ resuming! !

!methodRemoval: SystemDictionary #readDocumentFile!
SystemDictionary removeSelector: #readDocumentFile!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments'
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp'
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources fixSourceCodeLineEndings macroBenchmark1 macroBenchmark3 makeInternalRelease obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories removeTextCode someCleanup testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primVmFileName setMacFileInfoOn: unloadModule: vmFileName vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjectsDo: allObjectsSelect: allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition okayToDiscardUnsavedCode okayToSave processShutDownList: processStartUpList: quitPrimitive quitPrimitive: removeFromShutDownList: removeFromStartUpList: saveAs saveAs: saveAsEmbeddedImage saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:embedded: snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: inspectGlobals primitiveBeep)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Compiler swapping' actualCompiledMethodWithNodeClass actualDecompilerClass actualEncoderClass actualScannerClass)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: namesBeginningWith:do: namesBeginningWith:do:in:)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' processCommandLineArgument: processCommandLineArguments setStartupStamp startUpScriptArguments startup:)
!

