'From Cuis 4.2 of 25 July 2013 [latest update: #2296] on 3 May 2015 at 10:38:51.446611 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 10:12'!
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
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true
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


!Taskbar class methodsFor: 'class state access' stamp: 'jmv 5/3/2015 10:12'!
releaseClassState
	"Not just cached state but actual state. User preferences will be forgotten, and defaults will be assumed.
	Taskbar releaseClassState
	"
	"
	SoleInstance ifNotNil: [
		SoleInstance dismissMorph.
		SoleInstance _ nil ]
	"! !


!Preferences class methodsFor: 'standard queries'!
extraDebuggerButtons
	^ self
		valueOfFlag: #extraDebuggerButtons
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
menuIcons
	^ self
		valueOfFlag: #menuIcons
		ifAbsent: [ #allIcons ].! !

!Preferences class methodsFor: 'standard queries'!
optionalButtons
	^ self
		valueOfFlag: #optionalButtons
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'class state access' stamp: 'jmv 5/3/2015 10:37'!
releaseClassState
	"Not just cached state but actual state. User preferences will be forgotten, and defaults will be assumed.
	Preferences releaseClassState
	"
	Parameters _ nil.
	DictionaryOfPreferences _ nil! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/3/2015 10:19'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict shortCat class |
	menu _ self menu: 'Add a new morph'.
	menu

		add: 'From Clipboard'
		target: myHand
		action: #pasteMorph;
		
		add: 'From Alphabetical List'
		subMenu: self alphabeticalMorphMenu.
	menu addLine.
		
	"Add 'Classic' New Morph menu"
	catDict _ Dictionary new.
	SystemOrganization categories do: [ :cat |
		((cat beginsWith: 'Morphic-') and: [ (#('Morphic-Menus' 'Morphic-Support' ) includes: cat) not ]) ifTrue: [
			shortCat _ (cat
				copyFrom: 'Morphic-' size + 1
				to: cat size).
			(SystemOrganization listAtCategoryNamed: cat) do: [ :cName |
				class _ Smalltalk at: cName.
				((class inheritsFrom: Morph) and: [ class includeInNewMorphMenu ]) ifTrue: [
					(catDict includesKey: shortCat)
						ifTrue: [ (catDict at: shortCat) addLast: class ]
						ifFalse: [
							catDict
								at: shortCat
								put: (OrderedCollection with: class) ]]]]].
	catDict keys sort do: [ :categ |
		subMenu _ MenuMorph new.
		((catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ]) do: [ :cl |
			subMenu
				add: cl name
				target: self
				selector: #newMorphOfClass:event:
				argument: cl ].
		menu
			add: categ
			subMenu: subMenu ].

	self doPopUp: menu.! !


!Theme methodsFor: 'accessing' stamp: 'jmv 5/3/2015 10:29'!
decorateMenu: aMenu

	"decorate aMenu with icons"

	| numberAdded |
	
	self flag: #todo. "Do I really belong on Theme, and not some menu class somewhere?"

	menuItemIcons ifEmpty: [
		^ self ].

	numberAdded _ 0.

	aMenu items do: [ :item | 
		| decoration | decoration _ menuItemIcons 
				at: item contents asString asLowercase 
				ifAbsent: nil.

		decoration ifNotNil: [
			item icon: decoration.
			numberAdded _ numberAdded + 1. ]].

	numberAdded isZero ifTrue: [^ self].
	
	aMenu addBlankIconsIfNecessary: self blankIcon! !

!methodRemoval: Preferences class #classicNewMorphMenu!
Preferences class removeSelector: #classicNewMorphMenu!
!methodRemoval: Preferences class #menuWithIcons!
Preferences class removeSelector: #menuWithIcons!
