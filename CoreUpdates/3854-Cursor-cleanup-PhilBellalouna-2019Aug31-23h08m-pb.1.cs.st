'From Cuis 5.0 [latest update: #3852] on 1 September 2019 at 5:49:54 am'!
!classDefinition: #Cursor category: #'Graphics-Display Objects'!
Form subclass: #Cursor
	instanceVariableNames: ''
	classVariableNames: 'BlankCursor BottomLeftCursor BottomRightCursor CornerCursor CrossHairCursor CurrentCursor DownCursor MarkerCursor MenuCursor MoveCursor NormalCursor OriginCursor ReadCursor ResizeLeftCursor ResizeTopCursor ResizeTopLeftCursor ResizeTopRightCursor RightArrowCursor SquareCursor TargetCursor TopLeftCursor TopRightCursor UpCursor WaitCursor WebLinkCursor WriteCursor XeqCursor CursorDict '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!Cursor commentStamp: '<historical>' prior: 0!
I am a Form that is a possible appearance for a mouse cursor.  My size is always 16x16, ever since the original implementation on the Alto.

Predefined cursors should accessed via #cursorAt: which caches Cursor instances.  For example "Cursor cursorAt: #normalCursorWithMask".  You can also dynamically add your own cursors or modify existing ones via #cursorAt:put: as desired.!

!classDefinition: #WindowEdgeAdjustingMorph category: #'Morphic-Layouts'!
LayoutAdjustingMorph subclass: #WindowEdgeAdjustingMorph
	instanceVariableNames: 'selector coordinateGetter cursorGetter cursorKey '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!Cursor class methodsFor: 'accessing' stamp: 'pb 9/1/2019 00:43:01'!
cursorAt: cursorKey
	^ CursorDict
		at: cursorKey
		ifAbsent: [
			(self respondsTo: cursorKey) ifTrue: [ | newCursor |
				newCursor _ self perform: cursorKey.
				newCursor hasMask ifFalse: [ newCursor _ newCursor withMask ].
				self
					cursorAt: cursorKey
					put: newCursor ]].! !

!Cursor class methodsFor: 'accessing' stamp: 'pb 8/31/2019 23:11:45'!
cursorAt: cursorKey put: aCursor
	^ CursorDict at: cursorKey put: aCursor! !

!Cursor class methodsFor: 'accessing' stamp: 'pb 9/1/2019 04:32:41'!
defaultCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ Preferences biggerCursors
		ifTrue: [ CursorWithAlpha biggerNormal ]
		ifFalse: [ self cursorAt: #normalCursorWithMask ].! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:40:46'!
blankCursor
	"Answer the instance of me that is all white."
	^ self new.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:29:22'!
bottomLeftCursor
	^ self
		extent: 16 @ 16
		fromArray: #(49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 65532 65532 0 0 )
		offset: 0 @ -16.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:29:16'!
bottomRightCursor
	^ self
		extent: 16 @ 16
		fromArray: #(12 12 12 12 12 12 12 12 12 12 12 12 65532 65532 0 0 )
		offset: -16 @ -16.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:41:03'!
cornerCursor
	"Answer the instance of me that is the shape of the bottom right corner 
	of a rectangle."
	^ self
		extent: 16 @ 16
		fromArray: #(12 12 12 12 12 12 12 12 12 12 12 12 65532 65532 0 0 )
		offset: -16 @ -16.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:41:14'!
crossHairCursor
	"Answer the instance of me that is the shape of a cross."
	^ self
		extent: 16 @ 16
		fromArray: #(0 256 256 256 256 256 256 32764 256 256 256 256 256 256 0 0 )
		offset: -7 @ -7.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:41:27'!
downCursor
	"Answer the instance of me that is the shape of an arrow facing 
	downward."
	^ self
		extent: 16 @ 16
		fromArray: #(12288 12288 12288 12288 12288 12288 12288 64512 30720 12288 0 0 0 0 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:42:56'!
executeCursor
	"Answer the instance of me that is the shape of an arrow slanted left 
	with a star next to it."
	^ self
		extent: 16 @ 16
		fromArray: #(32800 49184 57456 62462 63884 64648 65272 61656 55692 39172 3072 3072 1536 1536 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:43:07'!
markerCursor
	"Answer the instance of me that is the shape of a small ball."
	^ self
		extent: 16 @ 16
		fromArray: #(28672 63488 63488 28672 0 0 0 0 0 0 0 0 0 0 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:43:17'!
menuCursor
	"Answer the instance of me that is the shape of a menu."
	^ self
		extent: 16 @ 16
		fromArray: #(65504 32800 42528 32800 54112 65504 32800 45728 32800 44192 32800 42272 32800 65504 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:43:27'!
moveCursor
	"Answer the instance of me that is the shape of a cross inside a square."
	^ self
		extent: 16 @ 16
		fromArray: #(65532 65532 49932 49932 49932 49932 65532 65532 49932 49932 49932 49932 65532 65532 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:43:41'!
normalCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ self
		extent: 16 @ 16
		fromArray: #(32768 49152 57344 61440 63488 64512 65024 63488 63488 38912 3072 3072 1536 1536 768 768 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:54:01'!
normalCursorWithMask
	"Cursor initNormalWithMask.  Cursor normal show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor _ CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ (CursorWithMask
		extent: 16 @ 16
		depth: 1
		fromArray: #(0 1073741824 1610612736 1879048192 2013265920 2080374784 2113929216 2130706432 2139095040 2080374784 1811939328 1174405120 100663296 50331648 50331648 0 )
		offset: -1 @ -1) setMaskForm:
		(Form
			extent: 16 @ 16
			depth: 1
			fromArray: #(3221225472 3758096384 4026531840 4160749568 4227858432 4261412864 4278190080 4286578688 4290772992 4292870144 4261412864 4009754624 3472883712 2273312768 125829120 58720256 )
			offset: 0 @ 0).! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:43:57'!
originCursor
	"Answer the instance of me that is the shape of the top left corner of a 
	rectangle."
	^ self
		extent: 16 @ 16
		fromArray: #(65532 65532 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:44:07'!
readCursor
	"Answer the instance of me that is the shape of eyeglasses."
	^ self
		extent: 16 @ 16
		fromArray: #(0 0 4104 10260 16416 64480 33824 33824 46496 31680 0 0 0 0 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:07:52'!
resizeBottomCursor
	^ self cursorAt: #resizeTopCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:08:37'!
resizeBottomLeftCursor
	^ self cursorAt: #resizeTopRightCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:11:24'!
resizeBottomRightCursor
	^ self cursorAt: #resizeTopLeftCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:38:39'!
resizeLeftCursor
	^ (self
		extent: 16 @ 16
		fromArray: #(0 1152 1152 1152 5280 13488 29880 64764 29880 13488 5280 1152 1152 1152 0 0 )
		offset: -7 @ -7) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:12:01'!
resizeRightCursor
	^ self cursorAt: #resizeLeftCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:39:40'!
resizeTopCursor
	^ (self
		extent: 16 @ 16
		fromArray: #(256 896 1984 4064 256 32764 0 0 32764 256 4064 1984 896 256 0 )
		offset: -7 @ -7) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:40:11'!
resizeTopLeftCursor
	^ (self
		extent: 16 @ 16
		fromArray: #(0 31760 30752 28740 26760 17680 544 1088 2176 4420 8748 1052 2108 124 0 0 )
		offset: -7 @ -7) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:40:38'!
resizeTopRightCursor
	^ (self
		extent: 16 @ 16
		fromArray: #(0 4220 2108 17436 8748 4420 2176 1088 544 17680 26760 28736 30752 31744 0 0 )
		offset: -7 @ -7) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:44:30'!
rightArrowCursor 
	"Answer the instance of me that is the shape of an arrow pointing to the right."
	^ self
		extent: 16 @ 16
		fromArray: #(1536 1920 2016 65528 2016 1920 1536 0 0 0 0 0 0 0 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:44:49'!
squareCursor
	"Answer the instance of me that is the shape of a square."
	^ self
		extent: 16 @ 16
		fromArray: #(0 0 0 0 0 960 960 960 960 0 0 0 0 0 0 0 )
		offset: -8 @ -8.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:45:00'!
targetCursor
	"Answer the instance of me that is the shape of a gunsight."
	^ self
		extent: 16 @ 16
		fromArray: #(1984 6448 8456 16644 17284 33026 35106 65278 35106 33026 17284 16644 8456 6448 1984 0 )
		offset: -7 @ -7.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:44:44'!
topLeftCursor
	^ self
		extent: 16 @ 16
		fromArray: #(65532 65532 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 49152 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 8/31/2019 23:45:08'!
topRightCursor
	^ self
		extent: 16 @ 16
		fromArray: #(65532 65532 12 12 12 12 12 12 12 12 12 12 12 12 0 0 )
		offset: -16 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:45:13'!
upCursor
	"Answer the instance of me that is the shape of an arrow facing upward."
	^ self
		extent: 16 @ 16
		fromArray: #(12288 30720 64512 12288 12288 12288 12288 12288 12288 12288 0 0 0 0 0 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:45:22'!
waitCursor
	"Answer the instance of me that is the shape of an Hourglass (was in the 
	shape of three small balls)."
	^ self
		extent: 16 @ 16
		fromArray: #(65532 32772 16392 8208 7392 4032 1920 1920 2368 4384 8592 17352 36852 65532 0 )
		offset: 0 @ 0.! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:45:34'!
webLinkCursor
	"Return a cursor that can be used for emphasizing web links"
	^ (CursorWithMask
		extent: 16 @ 16
		fromArray: #(3072 4608 4608 4608 4608 5046 4681 29257 37449 37449 32769 32769 49155 16386 24582 16380 )
		offset: -5 @ 0) setMaskForm:
		(Form
			extent: 16 @ 16
			fromArray:
				(#(3072 7680 7680 7680 7680 8118 8191 32767 65535 65535 65535 65535 65535 32766 32766 16380 ) collect: [ :bits |
					bits bitShift: 16 ])
			offset: 0 @ 0).! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:45:43'!
writeCursor
	"Answer the instance of me that is the shape of a pen writing."
	^ self
		extent: 16 @ 16
		fromArray: #(24 60 72 144 288 580 1156 2316 4624 9232 30728 20728 57728 32512 0 0 )
		offset: 0 @ 0.! !


!Debugger methodsFor: 'initialization' stamp: 'pb 9/1/2019 04:32:41'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices , (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			open: self
			label: label
			message: msg ].! !


!EventSensor methodsFor: 'private' stamp: 'pb 9/1/2019 04:32:41'!
eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."
	| delay delta |
	delay _ Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.
	"ensure not nil."
	[
	[
	delay wait.
	delta _ Time localMillisecondClock - lastEventPoll.
	delta > self class eventPollPeriod ifTrue: [
		"See #doOneCycleNowFor:"
		Cursor currentCursor = Cursor defaultCursor ifTrue: [ (Cursor cursorAt: #waitCursor) activateCursor ].
		"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
					interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
					In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
					running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
					Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
					events makes the 'processing' of those very quick."
		self fetchMoreEventsDiscardingMouseEvents: true ]]
		on: Error
		do: [ :ex |
			nil ]] repeat.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'pb 9/1/2019 01:30:55'!
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
	[ | isARealStartup guiRootObject guiRootObjectClass reopenTranscript |
	save not & quit
		ifTrue: [
			(SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [
			self
				logSnapshot: save
				andQuit: quit ].
	reopenTranscript _ false.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [
				each delete.
				reopenTranscript _ true ]].
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
	UISupervisor whenUIinSafeState: [
		"guiRootObject fullRepaintNeeded."
		reopenTranscript ifTrue: [ TranscriptWindow openTranscript ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.! !

!SystemDictionary methodsFor: 'startup' stamp: 'pb 9/1/2019 04:32:41'!
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
	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp ].! !


!UISupervisor class methodsFor: 'services' stamp: 'pb 9/1/2019 04:32:41'!
restoreDisplay
	DisplayScreen startUp.
	self ui ifNotNil: [ :guiRootObject |
		guiRootObject restoreDisplay ].
	self whenUIinSafeState: [ Cursor defaultCursor activateCursor ].! !


!Cursor class methodsFor: 'class initialization' stamp: 'pb 9/1/2019 03:36:04'!
initialize
	CursorDict _ Dictionary new.
! !

!Cursor class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 03:19:53'!
resizeForEdge: aSymbol 
	"Cursor resizeForEdge: #topCursor"
	"Cursor resizeForEdge: #bottomLeftCursor"

	"Do not erase this literal, as it helps 'senders' workproperly, and it protects these methods from accidental deletion"
	self nominallyUnsent: #(#resizeBottomCursor #resizeBottomLeftCursor #resizeBottomRightCursor #resizeLeftCursor #resizeRightCursor #resizeTopCursor #resizeTopLeftCursor #resizeTopRightCursor ).
	
	^ self perform: ('resize' , aSymbol first asString asUppercase
			, (aSymbol copyFrom: 2 to: aSymbol size), 'Cursor') asSymbol! !


!CursorWithAlpha methodsFor: 'accessing' stamp: 'pb 9/1/2019 04:02:28'!
fallback
	^fallback ifNil: [self class cursorAt: #normalCursorWithMask]! !


!Point class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 04:32:41'!
fromUser
	Sensor waitNoButton.
	(Cursor cursorAt: #crossHairCursor) activateCursor.
	Sensor waitButton.
	Cursor defaultCursor activateCursor.
	^ Sensor mousePoint"Point fromUser".! !


!Rectangle class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:30:37'!
fromUser
	"Answer a Rectangle that is determined by having the user 
	designate the top left and bottom right corners."
	| originRect |
	originRect _ (Cursor cursorAt: #originCursor) showWhile: [
		(Sensor mousePoint extent: 0 @ 0) newRectFrom: [ :f |
			Sensor mousePoint extent: 0 @ 0 ]].
	^ (Cursor cursorAt: #cornerCursor) showWhile: [
		originRect newRectFrom: [ :f |
			f origin corner: Sensor mousePoint ]].! !

!Rectangle class methodsFor: 'instance creation' stamp: 'pb 9/1/2019 01:30:41'!
originFromUser: extentPoint
	"Answer an instance of me that is determined by having the user 
	designate the top left corner. The width and height are determined by 
	extentPoint."
	^ (Cursor cursorAt: #originCursor) showWhile: [
		(Sensor mousePoint extent: extentPoint) newRectFrom: [ :f |
			Sensor mousePoint extent: extentPoint ]].! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'pb 9/1/2019 04:32:41'!
initClassCachedState
	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each isWorldMorph ifTrue: [ each redrawNeeded ]].
	Cursor defaultCursor activateCursor.! !


!HandMorph methodsFor: 'drawing' stamp: 'pb 9/1/2019 01:31:41'!
drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	aCanvas
		stencil: (Cursor cursorAt: #moveCursor)
		at: 0 @ 0
		color: Color black.! !

!HandMorph methodsFor: 'drawing' stamp: 'pb 9/1/2019 01:31:53'!
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"
	(savedPatch notNil or: [
		submorphs anySatisfy: [ :ea |
			ea visible ]]) ifTrue: [
		"using the software cursor; hide the hardware one"
		Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifFalse: [ (Cursor cursorAt: #blankCursor) activateCursor ].
		^ true ].
	^ false.! !

!HandMorph methodsFor: 'drawing' stamp: 'pb 9/1/2019 04:32:41'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas
			image: savedPatch
			at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self
			invalidateDisplayRect: (self morphPosition extent: extent)
			from: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !

!HandMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 04:32:41'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask defaultCursor extent.
	damageRecorder _ DamageRecorder new.
	grabMorphData _ IdentityDictionary new.
	self initForEvents.! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'pb 9/1/2019 03:02:06'!
cursor
	^ owner direction == #horizontal
		ifTrue: [ Cursor cursorAt: #resizeLeftCursor ]
		ifFalse: [ Cursor cursorAt: #resizeTopCursor ].! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'pb 9/1/2019 04:32:41'!
mouseLeave: anEvent
	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand _ nil.
		Cursor defaultCursor activateCursor ].! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'pb 9/1/2019 04:32:41'!
stepAt: millisecondSinceLast
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			Preferences cheapWindowReframe ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]]]
		ifFalse: [
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor defaultCursor activateCursor ]].! !


!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:40'!
initializeBottom
	selector _ #windowBottom:.
	coordinateGetter _ #y.
	cursorKey _ #resizeBottomCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:14'!
initializeBottomLeft
	selector _ #windowBottomLeft:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeBottomLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:22'!
initializeBottomRight
	selector _ #windowBottomRight:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeBottomRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:07'!
initializeLeft
	selector _ #windowLeft:.
	coordinateGetter _ #x.
	cursorKey _ #resizeLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:34'!
initializeRight
	selector _ #windowRight:.
	coordinateGetter _ #x.
	cursorKey _ #resizeRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:48:00'!
initializeTop
	selector _ #windowTop:.
	coordinateGetter _ #y.
	cursorKey _ #resizeTopCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:55'!
initializeTopLeft
	selector _ #windowTopLeft:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeTopLeftCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'pb 9/1/2019 05:47:49'!
initializeTopRight
	selector _ #windowTopRight:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeTopRightCursor! !

!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'pb 9/1/2019 05:47:45'!
cursor
	^ Cursor cursorAt: cursorKey.! !


!WorldState methodsFor: 'update cycle' stamp: 'pb 9/1/2019 04:32:41'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop."
	"See #eventTickler"
	| hadAnyEvent |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAnyEvent.! !

!methodRemoval: Cursor class #blank!
Cursor class removeSelector: #blank!
!methodRemoval: Cursor class #bottomLeft!
Cursor class removeSelector: #bottomLeft!
!methodRemoval: Cursor class #bottomRight!
Cursor class removeSelector: #bottomRight!
!methodRemoval: Cursor class #corner!
Cursor class removeSelector: #corner!
!methodRemoval: Cursor class #crossHair!
Cursor class removeSelector: #crossHair!
!methodRemoval: Cursor class #down!
Cursor class removeSelector: #down!
!methodRemoval: Cursor class #execute!
Cursor class removeSelector: #execute!
!methodRemoval: Cursor class #initBottomLeft!
Cursor class removeSelector: #initBottomLeft!
!methodRemoval: Cursor class #initBottomRight!
Cursor class removeSelector: #initBottomRight!
!methodRemoval: Cursor class #initCorner!
Cursor class removeSelector: #initCorner!
!methodRemoval: Cursor class #initCrossHair!
Cursor class removeSelector: #initCrossHair!
!methodRemoval: Cursor class #initDown!
Cursor class removeSelector: #initDown!
!methodRemoval: Cursor class #initMarker!
Cursor class removeSelector: #initMarker!
!methodRemoval: Cursor class #initMenu!
Cursor class removeSelector: #initMenu!
!methodRemoval: Cursor class #initMove!
Cursor class removeSelector: #initMove!
!methodRemoval: Cursor class #initNormal!
Cursor class removeSelector: #initNormal!
!methodRemoval: Cursor class #initNormalWithMask!
Cursor class removeSelector: #initNormalWithMask!
!methodRemoval: Cursor class #initOrigin!
Cursor class removeSelector: #initOrigin!
!methodRemoval: Cursor class #initRead!
Cursor class removeSelector: #initRead!
!methodRemoval: Cursor class #initResizeLeft!
Cursor class removeSelector: #initResizeLeft!
!methodRemoval: Cursor class #initResizeTop!
Cursor class removeSelector: #initResizeTop!
!methodRemoval: Cursor class #initResizeTopLeft!
Cursor class removeSelector: #initResizeTopLeft!
!methodRemoval: Cursor class #initResizeTopRight!
Cursor class removeSelector: #initResizeTopRight!
!methodRemoval: Cursor class #initRightArrow!
Cursor class removeSelector: #initRightArrow!
!methodRemoval: Cursor class #initSquare!
Cursor class removeSelector: #initSquare!
!methodRemoval: Cursor class #initTarget!
Cursor class removeSelector: #initTarget!
!methodRemoval: Cursor class #initTopLeft!
Cursor class removeSelector: #initTopLeft!
!methodRemoval: Cursor class #initTopRight!
Cursor class removeSelector: #initTopRight!
!methodRemoval: Cursor class #initUp!
Cursor class removeSelector: #initUp!
!methodRemoval: Cursor class #initWait!
Cursor class removeSelector: #initWait!
!methodRemoval: Cursor class #initWrite!
Cursor class removeSelector: #initWrite!
!methodRemoval: Cursor class #initXeq!
Cursor class removeSelector: #initXeq!
!methodRemoval: Cursor class #makeCursorsWithMask!
Cursor class removeSelector: #makeCursorsWithMask!
!methodRemoval: Cursor class #marker!
Cursor class removeSelector: #marker!
!methodRemoval: Cursor class #menu!
Cursor class removeSelector: #menu!
!methodRemoval: Cursor class #move!
Cursor class removeSelector: #move!
!methodRemoval: Cursor class #normal!
Cursor class removeSelector: #normal!
!methodRemoval: Cursor class #normalOrBiggerCursor!
Cursor class removeSelector: #normalOrBiggerCursor!
!methodRemoval: Cursor class #origin!
Cursor class removeSelector: #origin!
!methodRemoval: Cursor class #read!
Cursor class removeSelector: #read!
!methodRemoval: Cursor class #resizeBottom!
Cursor class removeSelector: #resizeBottom!
!methodRemoval: Cursor class #resizeBottomLeft!
Cursor class removeSelector: #resizeBottomLeft!
!methodRemoval: Cursor class #resizeBottomRight!
Cursor class removeSelector: #resizeBottomRight!
!methodRemoval: Cursor class #resizeLeft!
Cursor class removeSelector: #resizeLeft!
!methodRemoval: Cursor class #resizeRight!
Cursor class removeSelector: #resizeRight!
!methodRemoval: Cursor class #resizeTop!
Cursor class removeSelector: #resizeTop!
!methodRemoval: Cursor class #resizeTopLeft!
Cursor class removeSelector: #resizeTopLeft!
!methodRemoval: Cursor class #resizeTopRight!
Cursor class removeSelector: #resizeTopRight!
!methodRemoval: Cursor class #rightArrow!
Cursor class removeSelector: #rightArrow!
!methodRemoval: Cursor class #square!
Cursor class removeSelector: #square!
!methodRemoval: Cursor class #target!
Cursor class removeSelector: #target!
!methodRemoval: Cursor class #topLeft!
Cursor class removeSelector: #topLeft!
!methodRemoval: Cursor class #topRight!
Cursor class removeSelector: #topRight!
!methodRemoval: Cursor class #up!
Cursor class removeSelector: #up!
!methodRemoval: Cursor class #wait!
Cursor class removeSelector: #wait!
!methodRemoval: Cursor class #webLink!
Cursor class removeSelector: #webLink!
!methodRemoval: Cursor class #write!
Cursor class removeSelector: #write!
!methodRemoval: Cursor class #xeqCursor!
Cursor class removeSelector: #xeqCursor!

!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('testing' is: isEditingMethod)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
!


!EventSensor reorganize!
('accessing' eventTicklerProcess flushAllButDandDEvents flushEvents nextEvent peekButtons peekEvent peekMousePt peekPosition)
('mouse' createMouseEvent)
('private' eventTickler flushNonKbdEvents installEventTickler isKbdEvent: lastEventPoll primInterruptSemaphore: primKbdNext primKbdPeek primMouseButtons primMousePt primSetInterruptKey:)
('private-I/O' fetchMoreEvents fetchMoreEventsDiscardingMouseEvents: mapButtons:modifiers: primGetNextEvent: primSetInputSemaphore: processKeyboardSensorEvent: processMouseSensorEvent: processSensorEvent:discardingMouseEvents: queueEvent:)
('test' printEventBuffer: test)
('initialization' initialize shutDownSensor)
!


!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClass:from:to: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmParameterAt: vmParameterAt:put: vmVersion)
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
!


!UISupervisor class reorganize!
('gui process' newProcessIfUI: spawnNewMorphicProcessFor: stopUIProcess ui userInterrupt)
('services' restoreDisplay whenUIinSafeState:)
!

!classDefinition: #Cursor category: #'Graphics-Display Objects'!
Form subclass: #Cursor
	instanceVariableNames: ''
	classVariableNames: 'CurrentCursor CursorDict'
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!
Cursor initialize!

!Cursor class reorganize!
('accessing' cursorAt: cursorAt:put: defaultCursor)
('class initialization' initialize startUp)
('current cursor' currentCursor currentCursor:)
('instance creation' blankCursor bottomLeftCursor bottomRightCursor cornerCursor crossHairCursor downCursor executeCursor extent:fromArray:offset: markerCursor menuCursor moveCursor new normalCursor normalCursorWithMask originCursor readCursor resizeBottomCursor resizeBottomLeftCursor resizeBottomRightCursor resizeForEdge: resizeLeftCursor resizeRightCursor resizeTopCursor resizeTopLeftCursor resizeTopRightCursor rightArrowCursor squareCursor targetCursor topLeftCursor topRightCursor upCursor waitCursor webLinkCursor writeCursor)
!


!CursorWithMask class reorganize!
('as yet unclassified' derivedFrom:)
!


!Point class reorganize!
('instance creation' fromUser fromUserWithCursor: r:degrees: rho:theta: x:y:)
!


!PasteUpMorph class reorganize!
('system startup' initClassCachedState releaseClassCachedState)
('instance creation' newWorld)
('new-morph participation' categoryInNewMorphMenu)
!


!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:dragSel:)
('drawing' drawOn: fullDrawHandOn: hasChanged needsToBeDrawn restoreSavedPatchOn: savePatchFrom:appendDamageTo:)
('event handling' createEventFrom:ofType: flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startDropFilesEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('grabbing/dropping' attachMorph: attachMorphBeside: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating' redrawNeeded)
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateDropFilesEvent: generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!LayoutAdjustingMorph reorganize!
('accessing' adoptWidgetsColor: cursor)
('adjusting' adjustOwnerAt:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseEnter: mouseLeave:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('stepping' stepAt: stepTime)
('testing' isOpaqueMorph)
!

!classDefinition: #WindowEdgeAdjustingMorph category: #'Morphic-Layouts'!
LayoutAdjustingMorph subclass: #WindowEdgeAdjustingMorph
	instanceVariableNames: 'selector coordinateGetter cursorKey'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!WorldState reorganize!
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('canvas' canvas clearCanvas doFullRepaint ensureNonDisplayCanvas recordDamagedRect: setCanvas:)
('hands' activeHand hands handsDo: handsReverseDo: selectHandsToDrawForDamage:)
('initialization' clearWaitDelay initialize stepListSortBlock world:)
('stepping' cleanseStepList runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('update cycle' checkIfUpdateNeeded displayWorldSafely doOneCycle doOneCycleNow doOneMinimalCycleNow handleFatalDrawingError: tryDeferredUpdatingAndSetCanvas)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('drawing' displayWorldAndSubmorphs: drawInvalidAreasSubmorphs: simpleDrawInvalidAreasSubmorphs:)
('accessing' runLocalStepMethods:)
!

Utilities runningWorld hideTaskbar!
TranscriptWindow allInstancesDo: [ :t | t delete ]!
SystemWindow allInstancesDo: [ :t | t delete ]!
Cursor initialize!
Utilities recreateDefaultDesktop!
