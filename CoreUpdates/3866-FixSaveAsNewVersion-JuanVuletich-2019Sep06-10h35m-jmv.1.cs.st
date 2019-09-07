'From Cuis 5.0 [latest update: #3865] on 6 September 2019 at 10:37:54 am'!

!Utilities class methodsFor: 'default desktop' stamp: 'jmv 9/6/2019 09:31:05'!
defaultTextEditorContents
	^ ('
', 
('Cuis Smalltalk

' centered blue pointSize: FontFamily defaultPointSize * 3),
('
"Yay, Juan.  You GO, guy!! ...a great example of malleable software (and a clever mind) at work."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Dan Ingalls
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I like it... It''s nice and clean and simple and pretty. Nice stuff!!"
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Alan Kay
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I think you have a very elegant design aesthetic."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('John Maloney
'italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
'

',
'Cuis is a modern, Open Source, multiplatform, Smalltalk-80 system.


',
'Cuis is:

' bold,
'    - Small
    - Clean
    - Appropriable
' blue,
'

Additionally, Cuis is:

' bold, 
'    - Open Source
    - Multiplatform
' blue,
'
 
Like other Smalltalk systems, Cuis is also:

' bold, 
'    - A complete development environment written in itself
    - A pure, dynamic Object Oriented language
' blue,
'

Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, MacOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine] (http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can''t evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"] (http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!!

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

As Cuis evolves, we keep on these values. Every update, be it a bug fix or a feature enhancement, is reviewed carefully to avoid adding unneded complexity to the system. Every opportunity to remove unneded complexity is followed. As we go, features are enhanced, and any reported bugs fixed. We also adopt selected enhancements from Squeak and Pharo, and share our work with the wider Smalltalk community.


' justified,
'License
' bold,
'
Cuis is distributed subject to the MIT License, as in http://www.opensource.org/licenses/mit-license.php . Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.

Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982
Copyright (c) Apple Computer, Inc. 1985-1996
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2019')! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 9/6/2019 09:41:59'!
defaultFamilyName: aStringOrNil defaultPointSize: aNumberOrNil
	"Nil just means leave it as it is now."
	| family |
	aStringOrNil ifNotNil: [DefaultFamilyName _ aStringOrNil].
	aNumberOrNil ifNotNil: [DefaultPointSize _ aNumberOrNil].
	self revisar. "Please remove this hack meant to install larger DejaVu StrikeFonts on demand."
	DefaultFamilyName =  'DejaVu' ifTrue: [
		family _ AvailableFamilies at: DefaultFamilyName.
		(family pointSizes includes: DefaultPointSize) ifFalse: [ StrikeFont install: DefaultFamilyName]].
	UISupervisor ui ifNotNil: [ :world | world recreateDefaultDesktop ].! !


!PasteUpMorph methodsFor: 'defaul desktop' stamp: 'jmv 9/6/2019 09:35:32'!
recreateDefaultDesktop
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		submorphs 
			do: [ :ea |
				(ea class == SystemWindow) ifTrue: [ ea delete ].
				(ea class == TranscriptWindow) ifTrue: [ ea delete ]].
		TranscriptWindow openTranscript
			morphPosition: 5 @ 290;
			morphExtent: 990 @ 400.
		editor _ TextEditor openTextEditor
			morphPosition: 456 @ 10;
			morphExtent: 900 @ 680.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		self showTaskbar.
	].! !

!PasteUpMorph methodsFor: 'defaul desktop' stamp: 'jmv 9/6/2019 09:35:42'!
tearDownDesktop
	self whenUIinSafeState: [
		self hideTaskbar.
		submorphs 
			do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]]].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/6/2019 10:37:39'!
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
	clearAllStateFlag ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].
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
	Display triggerEvent: #screenSizeChanged.
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			guiRootObject recreateDefaultDesktop ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/6/2019 09:22:28'!
setSystemFontTo: aFont
	"Establish the default text font and style"

	aFont ifNil: [^ self].
	FontFamily defaultFamilyName: aFont familyName defaultPointSize: aFont pointSize.! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 9/6/2019 09:42:26'!
defaultFamilyName: aString
	self defaultFamilyName: aString defaultPointSize: nil! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 9/6/2019 09:42:35'!
defaultPointSize: aNumber
	self defaultFamilyName: nil defaultPointSize: aNumber! !

!methodRemoval: Utilities class #pvtDefaultTextEditorContents!
Utilities class removeSelector: #pvtDefaultTextEditorContents!
!methodRemoval: Utilities class #recreateDefaultDesktop!
Utilities class removeSelector: #recreateDefaultDesktop!
!methodRemoval: Utilities class #tearDownDesktop!
Utilities class removeSelector: #tearDownDesktop!
