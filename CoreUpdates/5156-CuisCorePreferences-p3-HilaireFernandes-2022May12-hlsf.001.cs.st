'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 10:40:55 am'!

!FileList methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 10:25:53'!
initialDirectoryList

	| initialDirectoryListFromPreferences wrapperCreator |
	wrapperCreator _ [ :directoryEntry |
		FileDirectoryWrapper
			with: directoryEntry
			name: (directoryEntry name ifNil: [ '/' ])
			model: self ].
	(initialDirectoryListFromPreferences _ PreferenceNG at: #initialFileListDirectories) 
		caseOf: {
			[ #roots ] -> [ 	| dirList |
				dirList _ DirectoryEntry roots collect: wrapperCreator.
				dirList isEmpty ifTrue: [
					dirList _ Array with: (FileDirectoryWrapper 
						with: directory
						name: directory localName 
						model: self) ].
				^ dirList ].
			[ #image ] -> [ 
				^ { wrapperCreator value: DirectoryEntry smalltalkImageDirectory } ].
			[ #vm  ] -> [ 
				^ { wrapperCreator value: DirectoryEntry vmDirectory } ].
			[ #current ] -> [
				^ { wrapperCreator value: DirectoryEntry currentDirectory } ] }
		otherwise: [ ^ initialDirectoryListFromPreferences collect: wrapperCreator ]! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'hlsf 5/12/2022 10:24:46'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ UISupervisor newUIProcessIfNeeded.
	p ifNil: [
		p _ Sensor shiftPressed | (PreferenceNG at: #cmdDotInterruptTakesStatistics)
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	(PreferenceNG at: #cmdDotEnabled) ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		p == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(p name beginsWith: '[system]') ifTrue: [
			 ('Process {', p printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			p isTerminated
				ifTrue: [ ('Process {', p printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [ UISupervisor userInterrupt: p ]] fork
		]! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'hlsf 5/12/2022 10:36:58'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	PreferenceNG at: #warnIfNoChangesFile put: false.
	PreferenceNG at: #warnIfNoSourcesFile put: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'hlsf 5/12/2022 10:07:51'!
askConfirmationOnQuit
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk askConfirmationOnQuit
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages _ ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	(PreferenceNG at: #askConfirmationOnQuit) ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'hlsf 5/12/2022 10:15:03'!
setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |
	(PreferenceNG at: #automaticPlatformSettings) ifFalse:[^self].
	platform _ self platformName.
	specs _ 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs _ #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs _ #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do: [:tuple |
		PreferenceNG name: tuple first category: #system value: (tuple last == #true) 	]
! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'hlsf 5/12/2022 10:15:33'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(PreferenceNG at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine ].
	StartupStamp _ nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'hlsf 5/12/2022 10:39:43'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ PreferenceNG at: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [PreferenceNG at: #warnIfNoChangesFile])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].
	ChangesInitialFileSize _ changes ifNotNil: [ changes position ].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'hlsf 5/12/2022 10:21:15'!
hasToRestoreChanges

	^(PreferenceNG at: #checkLostChangesOnStartUp) and: [ 
		self withChangesFileDo: [ :changesFile | self hasToRestoreChangesFrom: changesFile ]].
	! !


!Exception methodsFor: 'priv handling' stamp: 'hlsf 5/12/2022 10:36:08'!
evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"Please see
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000800.html
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000809.html
					Also see the rest of the tread in detail.
					This is work in progress."
					(PreferenceNG at: #allowNonLocalReturnsInExceptionHandlers)
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							(PreferenceNG at: #warnAboutNonLocalReturnsInExceptionHandlers)
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed _ true ].
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !


!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'hlsf 5/12/2022 10:11:31'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(PreferenceNG at: #askToInstallComplexPackage) ifTrue: [
		answer _ PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature require: #'Complex'.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			(PreferenceNG at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction! !


!Editor methodsFor: 'private' stamp: 'hlsf 5/12/2022 10:33:01'!
setIndices: shiftPressed forward: forward
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices _ Dictionary new.
	(shiftPressed and:[PreferenceNG at: #selectionsMayShrink])
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices! !


!TextEditor methodsFor: 'private' stamp: 'hlsf 5/12/2022 10:16:35'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [PreferenceNG at: #caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true! !


!WorldMorph methodsFor: 'update cycle' stamp: 'hlsf 5/12/2022 10:33:25'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	(PreferenceNG at: #serverMode)
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !


!SystemWindow methodsFor: 'events' stamp: 'hlsf 5/12/2022 10:34:28'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph"
	^aMorph isWorldMorph or:[PreferenceNG at: #systemWindowEmbedOK]! !

!SystemWindow methodsFor: 'open/close' stamp: 'hlsf 5/12/2022 10:25:09'!
closeBoxHit
	"The user clicked on the close-box control in the window title.  For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down."

	(PreferenceNG at: #dismissAllOnOptionClose) ifTrue:
		[Sensor rawMacOptionKeyPressed ifTrue:
			[^ self world closeUnchangedWindows]].
	self delete
! !


!FileListWindow methodsFor: 'menu building' stamp: 'hlsf 5/12/2022 10:29:53'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	model currentDirectorySelected
		ifNil: [ aMenu add: 'initial directory' action: #yourself :: isEnabled: false ]
		ifNotNil: [ :selectedWrapper |
	aMenu 
		add: (Preferences isInitialFileListDirectory: selectedWrapper item)
			asMenuItemTextPrefix, 'initial directory'
		action: #toggleInitialDirectory ::
			setBalloonText: 'The selected directory is an initial director for new file list windows' ].
	initialDirectoriesMenu _ MenuMorph new.
	#(
		(roots  'default roots' 'Use the usual root directories. Drives on Windows; "/" on Unix')
	   	(image 'image directory' 'Use the directory with Smalltalk image')
		(vm 'VM directory' 'Use the virtual machine directory')
		(current 'current directory' 'Use the current directory; usually the directory the VM was started in')
	)
		do: [ :entry |
			initialDirectoriesMenu
				add: entry second
				target: PreferenceNG
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!MenuMorph methodsFor: 'control' stamp: 'hlsf 5/12/2022 10:34:41'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[PreferenceNG at: #systemWindowEmbedOK]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'hlsf 5/12/2022 10:18:30'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse _ (PreferenceNG at: #changeSetVersionNumbers)
				ifTrue: [
					DirectoryEntry currentDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	(PreferenceNG at: #checkForSlips) 		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !


!ChangeSet class methodsFor: 'services' stamp: 'hlsf 5/12/2022 10:35:05'!
install: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
		('Installed ChangeSet: ', aFileEntry name) print]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'hlsf 5/12/2022 10:35:10'!
fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]! !


!CodePackageFile methodsFor: 'services' stamp: 'hlsf 5/12/2022 10:35:17'!
install
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS _ ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS _ nil ].
		pckClass _ CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				((self packageName asIdentifier: true), 'Package') = ee name ifTrue: [
					ee fileInDefinitionAndMetaclass.
					pckClass _ Smalltalk at: ee name ]]].
		newCodePackage _ pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.

		fullName asFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !


!TestResult methodsFor: 'logging' stamp: 'hlsf 5/12/2022 10:35:22'!
reportAboutToRun: aTestCase
	(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]! !

!TestResult methodsFor: 'logging' stamp: 'hlsf 5/12/2022 10:35:27'!
reportFailed: aTestCase because: anException
	(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
		Transcript print: anException; newLine].! !

!TestResult methodsFor: 'logging' stamp: 'hlsf 5/12/2022 10:35:31'!
reportPassed: aTestCase
	(PreferenceNG at: #transcriptLogVerbose) ifTrue: [
		Transcript show: 'finished.'; newLine]! !

