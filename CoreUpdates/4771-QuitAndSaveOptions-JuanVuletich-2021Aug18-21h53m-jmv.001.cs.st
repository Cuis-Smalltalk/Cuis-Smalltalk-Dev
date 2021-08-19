'From Cuis 5.0 [latest update: #4768] on 18 August 2021 at 10:08:36 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 19:52:01'!
quit
	"Just quit. No questions asked. No validations done.
	Smalltalk quit.
	"
	self snapshot: false andQuit: true embedded: false clearAllClassState: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 21:03:40'!
saveAndQuit
	"Save image and quit. No questions asked.
	Smalltalk saveAndQuit.
	"
	ChangeSet zapAllChangeSets.
	^ self
		snapshot: true
		andQuit: true
		embedded: false
		clearAllClassState: false.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 21:03:59'!
saveAs: newName clearAllClassState: clearAllStateFlag
	"Save the image  under a new name. Don't quit.
	See senders."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFileEntry to: newChangesName asFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	ChangeSet zapAllChangeSets.
	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false embedded: false clearAllClassState: clearAllStateFlag.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 21:05:18'!
saveImage
	"Save image. Don't quit. No questions asked.
	Smalltalk saveImage.
	"
	ChangeSet zapAllChangeSets.
	^ self
		snapshot: true
		andQuit: false
		embedded: false
		clearAllClassState: false.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 20:26:01'!
okayToDiscardUnsavedCode
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCode
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages _ ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Continue?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Continue?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Continue?' ].
	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 20:47:51'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	self request: 'New file name?' initialAnswer: self imageName asFileEntry name do: [:newName|
		((((self fullNameForImageNamed: newName) asFileEntry exists not
			and: [(self fullNameForChangesNamed: newName) asFileEntry exists not])
			or: [self confirm: ('{1} already exists. Overwrite?' format: {newName})]))
					ifTrue: [
						self saveAs: newName clearAllClassState: false]]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 21:04:40'!
saveAsNewVersion
	"Save the image/changes using the next available version number.
	This is usually only used to prepare updated Cuis images for distribution."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName clearAllClassState: true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2021 21:54:21'!
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
	activeProc _ Processor activeProcess.
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
	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Cosas que levanto explicitamente abajo"
	guiRootObjectClass _ UISupervisor ui class.
	guiRootObject _ UISupervisor ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"guiRootObject _ clearAllStateFlag ifFalse: [ UISupervisor ui ]."
	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
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
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Smalltalk printStuffToCleanOnImageSave.
		"Remove this call to actually see the image clean report."
		Transcript clear.
		].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
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
	Smalltalk openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls |
		cls initClassCachedState ].
	self doStartUp: isARealStartup == true.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ guiRootObjectClass newWorld ]).
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			guiRootObject recreateDefaultDesktop; restoreDisplay.
			Display triggerEvent: #screenSizeChanged ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			guiRootObject restoreDisplay.
			Display triggerEvent: #screenSizeChanged ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 8/18/2021 20:51:39'!
cuisDefaults
	self setPreferencesFrom: #(
			#(#balloonHelpEnabled true )
			#(#biggerCursors false )
			#(#browseWithPrettyPrint false )
			#(#caseSensitiveFinds false )
			#(#checkForSlips true )
			#(#cmdDotEnabled true )
			#(#diffsInChangeList true )
			#(#diffsWithPrettyPrint false )
			#(#menuKeyboardControl true )
			#(#optionalButtons true )
			#(#extraDebuggerButtons true )
			#(#subPixelRenderFonts true )
			#(#thoroughSenders true )
			#(#cheapWindowReframe false )
			#(#syntaxHighlightingAsYouType true )
			#(#tapAndHoldEmulatesButton2 true )
			#(#clickGrabsMorphs false )
			#(#syntaxHighlightingAsYouTypeAnsiAssignment false )
			#(#syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		).
	self useMenuIcons.! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/18/2021 20:15:28'!
quitSession

	Smalltalk okayToDiscardUnsavedCode ifFalse: [ ^ self ].
	Smalltalk quit! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 8/18/2021 22:04:22'!
worldMenuOptions

	^`{{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		10.
		#label 				-> 		'Open...'.
		#selector 				-> 		#openOpenMenu.
		#icon 				-> 		#openIcon.
		#balloonText				-> 		'Submenu to open various system tools'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		20.
		#label 				-> 		'New morph...'.
		#selector				-> 		#newMorph.
		#icon 				-> 		#morphsIcon.
		#balloonText 				-> 		'Offers a variety of ways to create new objects'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		30.
		#label 				-> 		'Preferences...'.
		#selector 				-> 		#openPreferencesMenu.
		#icon 				-> 		#preferencesIcon.
		#balloonText 				-> 		'Opens a menu offering many controls over appearance and system preferences.'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		40.
		#label 				-> 		'Windows...'.
		#selector 				-> 		#openWindowsMenu.
		#icon 				-> 		#windowIcon.
		#balloonText 				-> 		'Manage open windows'.
	} asDictionary.
	{
		#itemGroup 				-> 		10.
		#itemOrder 				-> 		50.
		#label 				-> 		'Help...'.
		#selector 				-> 		#openHelpMenu.
		#icon 				-> 		#helpIcon.
		#balloonText 				-> 		'Opens a menu of useful items for updating the system, determining what version you are running, and much else'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		10.
		#label 				-> 		'Changes...'.
		#selector 				-> 		#openChangesMenu.
		#icon 				-> 		#changesIcon.
		#balloonText 				-> 		'Opens a menu of useful tools for dealing with changes'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		20.
		#label 				-> 		'Debug...'.
		#selector 				-> 		#openDebugMenu.
		#icon 				-> 		#debugIcon.
		#balloonText 				-> 		'a menu of debugging items'.
	} asDictionary.
	{
		#itemGroup 				-> 		20.
		#itemOrder 				-> 		30.
		#label 				-> 		'Restore Display'.
		#object 				-> 		#myWorld.
		#selector 				-> 		#restoreDisplay.
		#icon 				-> 		#displayIcon.
		#balloonText 				-> 		'Repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		10.
		#label 				-> 		'Save Image'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveImage.
		#icon 				-> 		#saveIcon.
		#balloonText 				-> 		'Save the current version of the image on disk. Overwrite existing file.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		20.
		#label 				-> 		'Save as...'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAs.
		#icon 				-> 		#saveAsIcon.
		#balloonText 				-> 		'Save the current version of the image on disk under a new name.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		30.
		#label 				-> 		'Save new Cuis Release'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAsNewVersion.
		#icon 				-> 		#saveAsNewVersionIcon.
		#balloonText 				-> 		('Save as a new release of Cuis.\',
								'Use an updated version-stamped name\',
								'and save it under that name on disk.\',
								'Clear all user preferences and user state (class vars).') withNewLines.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		40.
		#label 				-> 		'Save and Quit'.
		#object 				-> 		Smalltalk.
		#selector 				-> 		#saveAndQuit.
		#icon 				-> 		#saveAndQuitIcon.
		#balloonText 				-> 		'Save the image and quit out of Cuis. Overwrite existing file.'.
	} asDictionary.
	{
		#itemGroup 				-> 		30.
		#itemOrder 				-> 		50.
		#label 				-> 		'Quit'.
		#selector 				-> 		#quitSession.
		#icon 				-> 		#quitIcon.
		#balloonText 				-> 		('Quit out of Cuis without saving the image.\',
								'Ask for confirmation if there are unsaved changes.') withNewLines.
	} asDictionary.
}`! !

!methodRemoval: TheWorldMenu #saveAndQuit stamp: 'jmv 8/18/2021 22:04:27'!
TheWorldMenu removeSelector: #saveAndQuit!
!methodRemoval: Preferences class #askForSaveOnQuit stamp: 'jmv 8/18/2021 21:59:14'!
Preferences class removeSelector: #askForSaveOnQuit!
!methodRemoval: SystemDictionary #saveAs:andQuit:clearAllClassState: stamp: 'jmv 8/18/2021 21:58:39'!
SystemDictionary removeSelector: #saveAs:andQuit:clearAllClassState:!
!methodRemoval: SystemDictionary #okayToSave stamp: 'jmv 8/18/2021 21:58:58'!
SystemDictionary removeSelector: #okayToSave!
!methodRemoval: SystemDictionary #saveSession stamp: 'jmv 8/18/2021 21:58:42'!
SystemDictionary removeSelector: #saveSession!
!methodRemoval: SystemDictionary #snapshot:andQuit:clearAllClassState: stamp: 'jmv 8/18/2021 21:58:51'!
SystemDictionary removeSelector: #snapshot:andQuit:clearAllClassState:!
!methodRemoval: SystemDictionary #snapshot:andQuit:embedded: stamp: 'jmv 8/18/2021 21:58:35'!
SystemDictionary removeSelector: #snapshot:andQuit:embedded:!
!methodRemoval: SystemDictionary #snapshot:andQuit: stamp: 'jmv 8/18/2021 21:58:31'!
SystemDictionary removeSelector: #snapshot:andQuit:!
