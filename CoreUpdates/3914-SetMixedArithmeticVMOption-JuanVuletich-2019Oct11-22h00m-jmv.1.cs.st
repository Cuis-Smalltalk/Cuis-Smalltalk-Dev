'From Cuis 5.0 [latest update: #3913] on 11 October 2019 at 10:12:30 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'eem 8/28/2009 14:52'!
voidCogVMState
	"Void any internal caches the VM maintains other than the method lookup caches.
		 These comprise
				- the stack zone, where method activations are stored, and
				- the machine code zone, where the machine code form of CompiledMethods is held."
	<primitive: 214>
	^self primitiveFailed

	"Time millisecondsToRun: [Smalltalk voidCogVMState]"
	"(1 to: 10) collect: [:ign| Time millisecondsToRun: [Smalltalk voidCogVMState]]"! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 10/11/2019 22:09:36'!
doMixedArithmetic
	"If true, then primitives can handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.
	See doMixedArithmetic:
	
	Note:
		OpenSmalltalk VMs after March, 2019 can set the option and will honor it. The comparison operation behaves as if the Float was converted #asTrueFraction. This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk doMixedArithmetic: false'.
		Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior."

	^ ((Smalltalk vmParameterAt: 48) allMask: 64) not! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 10/11/2019 22:10:11'!
doMixedArithmetic: aBoolean
	"If set to true, then primitives can handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.

	Please see  comment at #doMixedArithmetic"

	"Ignore request if VM doesn't support it"
	[
		self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 64) + (aBoolean ifTrue: [0] ifFalse: [64]).
	] on: Error do: [].! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 10/11/2019 22:11:55'!
doStartUp: isARealStartup
	"
	isARealStartup
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"
	"Here, startup begins!!"
	Cursor defaultCursor activateCursor.
	self setGCParameters.
	isARealStartup ifTrue: [ self clearExternalObjects ].

	"We need to do this on startup because we can not know if the image was saved with a pre March2019 VM,
	and started with a later VM that handles the option.
	Please see  comment at #doMixedArithmetic"
	self doMixedArithmetic: false.

	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp ].! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 10/11/2019 19:12:58'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers. 
	Answer with the current value of that register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	end (v3) / size(Spur) of old-space (0-based, read-only)
		2	end of young-space (v3) / size of new-space (Spur) (read-only)
		3	end (v3) / size(Spur) of heap (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	incremental GCs (v3) / scavenges (Spur) since startup (read-only)
		10	total milliseconds in incremental GCs (v3) / scavenges (Spur) since startup (read-only)
		11	tenures of surving objects since startup or reset (read-write)
		12-20 were specific to ikp's JITTER VM, now 12-15 are open for use
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in compaction phase of full GC since start-up (Spur only)
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled. Newer Spur VMs only.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which shrinking object memory (rw)
		25	memory headroom when growing object memory (rw)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (rw)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	Spur: bytes allocated in total since start-up or reset (read-write) (Used to be number of allocations done before current IGC/FGC (read-only))
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use
		43	desired number of stack pages (stored in image file header, max 65535)
		44	size of eden, in bytes
		45	desired size of eden, in bytes (stored in image file header)
		46	machine code zone size, in bytes (Cog only; otherwise nil)
		47	desired machine code zone size (stored in image file header; Cog only; otherwise nil)
		48	various header flags.  
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	max size the image promises to grow the external semaphore table to (0 sets to default, which is 256 as of writing)
		50-51 nil; reserved for VM parameters that persist in the image (such as eden above)
		52	root/remembered table capacity
		53	number of segments (Spur only; otherwise nil)
		54	total size of free old space (Spur only, otherwise nil)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of ForceInterruptCheck calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only)
		61	number of stack page divorces since startup (read-only)
		62	compiled code compactions since startup (read-only; Cog only; otherwise nil)
		63	total milliseconds in compiled code compactions since startup (read-only; Cog only; otherwise nil)
		64	the number of methods that currently have jitted machine-code
		65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMUTABILITY is bit 1
		66	the byte size of a stack page
		67	the max allowed size of old space (Spur only; nil otherwise; 0 implies no limit except that of the underlying platform)
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)
		71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)
		72  total milliseconds in full GCs Mark phase since startup (read-only)
		73  total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74  maximum pause time due to segment allocation"

	<primitive: 254>
	^nil! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 10/11/2019 19:12:16'!
vmParameterAt: parameterIndex put: newValue
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Store newValue (a positive integer) into that
	register and answer with the previous value that was stored there.
	Fail if newValue is out of range, if parameterIndex has no corresponding
	register, or if the corresponding register is read-only.

	As of mid 2017 the parameters which can be set are
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		17	proportion of code zone available for use (Sista VMs only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		34	bytes allocated in total since start-up or reset (read-write)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	the size of the external semaphore table (read-write; Cog VMs only)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur only)."

	<primitive: 254>
	self primitiveFailed! !


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk doMixedArithmetic: false.!

