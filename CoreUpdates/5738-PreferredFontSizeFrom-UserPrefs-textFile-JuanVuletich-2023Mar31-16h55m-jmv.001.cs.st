'From Cuis 6.0 [latest update: #5737] on 31 March 2023 at 5:03:58 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/31/2023 17:01:34'!
readAndApplyUserPrefs
	"
	Smalltalk readAndApplyUserPrefs
	"
	| file line parts |
	file := DirectoryEntry smalltalkImageDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[PreferenceSet perform: parts first asSymbol with: parts second]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/31/2023 17:02:35'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
	"This is the main method for image save and / or quit.
	See senders."
	"WARNING: Current process will be killed. UI Process will be restarted"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic
	checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	activeProc := Processor activeProcess.
	[ | isARealStartup guiRootObject guiRootObjectClass |
	save not & quit
		ifTrue: [
			(SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [
			self
				logSnapshot: save
				andQuit: quit ].
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [
				each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.
	"These could be moved to some #shutDown"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Stuff needed to relaunch UI on startup"
	guiRootObjectClass := UISupervisor ui class.
	guiRootObject := UISupervisor ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"guiRootObject _ clearAllStateFlag ifFalse: [ UISupervisor ui ]."
	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	guiRootObject ifNotNil: [ guiRootObject releaseCachedState ].
	"Clean Globals"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: nil.
	Smalltalk closeSourceFiles.
	Smalltalk
		at: #SourceFiles
		put: nil.
	Smalltalk allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		Smalltalk allClassesDo: [ :cls |
			cls releaseClassState ]].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		"Smalltalk printStuffToCleanOnImageSave".
		].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup := embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup := false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].

	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: DisplayScreen new.
	Smalltalk
		at: #SourceFiles
		put: (Array new: 2).
	self logStartupDebugAid: 'To #openSourceFiles'.
	Smalltalk openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	Smalltalk allClassesDo: [ :cls |
		cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	self logStartupDebugAid: 'To #restoreLostChangesIfNecessary'.
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #recreateDefaultDesktop'.
			guiRootObject recreateDefaultDesktop; restoreDisplay ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #restoreDisplay'.
			guiRootObject restoreDisplay ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
			self readAndApplyUserPrefs.
			self logStartupDebugAid: 'To #processCommandLineArguments'.
			self processCommandLineArguments.
			self logStartupDebugAid: 'To #launchApp'.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self logStartupError: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.! !


!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 3/31/2023 15:44:28'!
defaultFontSize: pointSizeNumberOrString
	"
	Argument may be a String if reading UserPrefs.txt
	PreferenceSet defaultFontSize: 11
	PreferenceSet defaultFontSize: 14
	PreferenceSet defaultFontSize: 21
	"
	| pointSize font titleFont |
	pointSize := pointSizeNumberOrString asNumber.
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 30).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !

