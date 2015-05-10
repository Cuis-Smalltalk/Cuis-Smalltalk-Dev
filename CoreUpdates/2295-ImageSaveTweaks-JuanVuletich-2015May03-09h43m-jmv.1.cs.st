'From Cuis 4.2 of 25 July 2013 [latest update: #2295] on 3 May 2015 at 9:54:33.592594 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:49'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	self currentChangesName ifNotNil: [ :oldChangesName | | newChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:48'!
snapshot: save andQuit: quit clearAllClassState: clearAllStateFlag
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSet zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				self okayToDiscardUnsavedCode ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false
		clearAllClassState: clearAllStateFlag! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:47'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	"
	To test the cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false
	"
| activeProc |
activeProc _ Processor activeProcess.
[ 
	| resuming world someTranscript |
	self logSapshot: save andQuit: quit.

	someTranscript _ false.
	TranscriptWindow allInstancesDo: [ :each | each isInWorld ifTrue: [ each delete. someTranscript _ true ]].
	Transcript logToFile: false.
	ActiveModel flushEventSystem.
	self processShutDownList: quit.


	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	
	"Cosas que levanto explicitamente abajo"
	world _ ProjectX ui.
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
	false not ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


	"Do image save & quit as apropriate"
	Cursor write activateCursor.
	save
		ifTrue: [
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].


	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: (DisplayScreen extent: 1024@768 depth: 32).
	Smalltalk at: #SourceFiles put: (Array new: 2).	Smalltalk openSourceFiles.

	"Here, startup begins!! (resuming might be nil)"
	self startup: resuming == true.
	
	
	ProjectX spawnNewMorphicProcessFor: world.
	
	someTranscript ifTrue: [
		WorldState addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	WorldState addDeferredUIMessage: [
		world fullRepaintNeeded ].

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 5/2/2015 17:45'!
releaseClassState
	"Will be called for each class on 'Save as new version'.
	All class vars or class instVar vars that can be recreated lazily on demand, should be nilled.
	See implementors for examples"! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:49'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	| newName |
	newName _ self getFileNameFromUser.
	newName ifNil: [^ self].
 	self okayToSave ifFalse: [^self].
	self saveAs: newName andQuit: false clearAllClassState: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:48'!
saveAsEmbeddedImage
	"Save the current state of the system as an embedded image"

	| dir newName newImageName |
	dir _ FileDirectory default.
	newName _ FillInTheBlankMorph
		request: 'Select existing VM file'
		initialAnswer: (FileDirectory localNameFor: '').
	newName = '' ifTrue: [^ self].
	newName _ FileDirectory baseNameFor: newName asFileName.

	newImageName _ newName.
	(dir includesKey: newImageName) ifFalse:
		[^ self inform: 'Unable to find name ', newName, ' Please choose another name.'].

	self logChange: '----SAVEAS (EMBEDDED) ', newName, '----', Date dateAndTimeNow printString.
	self imageName: (dir fullNameFor: newImageName).
	LastImageName _ self imageName.
	self closeSourceFiles.
	self snapshot: true andQuit: true embedded: true clearAllClassState: false
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:50'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, FileDirectory dot,  FileDirectory imageSuffix.
	(FileDirectory default includesKey: newName) ifTrue: [
		newName _ FileDirectory default
			nextNameFor: fileName
			extension: FileDirectory imageSuffix ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory default includesKey: changesName) ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 09:50'!
saveSession
	self snapshot: true andQuit: false clearAllClassState: false! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 5/3/2015 09:50'!
runtimeDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."

	| file |
	file := FileStream newFileNamed: ('error', Utilities dateTimeSuffix, FileDirectory dot, 'log') asFileName.
	Smalltalk timeStamp: file.
	(thisContext sender stackOfSize: 20) do: [:ctx | file newLine. ctx printOn: file].
	file close.

	Smalltalk snapshot: false andQuit: true clearAllClassState: false! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 5/3/2015 09:50'!
quitSession
	Smalltalk 
		snapshot: (self confirm: 'Save the current image before quitting?' orCancel: [^ self])
		andQuit: true
		clearAllClassState: false! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 5/3/2015 09:50'!
saveAndQuit

	Smalltalk snapshot: true andQuit: true clearAllClassState: false! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/3/2015 09:53'!
saveMenu

        | menu |

        menu _ self menu: 'Save options...'.
        ^self fillIn: menu from: { 
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name\and save it under that name on disk.\Clear all user preferences and user state (class vars).' withNewLines}.
"				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the current image on disk, and quit out of Cuis.'}."
        }! !

!methodRemoval: SystemDictionary #saveAs:!
SystemDictionary removeSelector: #saveAs:!
!methodRemoval: SystemDictionary #saveAs:andQuit:!
SystemDictionary removeSelector: #saveAs:andQuit:!
!methodRemoval: SystemDictionary #snapshot:andQuit:!
SystemDictionary removeSelector: #snapshot:andQuit:!
!methodRemoval: SystemDictionary #snapshot:andQuit:embedded:!
SystemDictionary removeSelector: #snapshot:andQuit:embedded:!
