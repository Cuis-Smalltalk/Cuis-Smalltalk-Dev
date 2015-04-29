'From Cuis 4.2 of 25 July 2013 [latest update: #2258] on 20 April 2015 at 4:35:24.731872 pm'!

!MessageTally class methodsFor: 'cached state access' stamp: 'jmv 4/20/2015 15:47'!
releaseClassCachedState

	DefaultPollPeriod _ nil! !


!Time class methodsFor: 'cached state access' stamp: 'jmv 4/20/2015 15:47'!
releaseClassCachedState

	MillisecondClockOffset _ 0! !


!HaloMorph class methodsFor: 'cached state access' stamp: 'jmv 4/20/2015 15:50'!
handleSize
	HandleSize ifNil: [
		HandleSize _ 16 ].
	^ HandleSize! !

!HaloMorph class methodsFor: 'cached state access' stamp: 'jmv 4/20/2015 15:52'!
icons
	Icons ifNil: [
		Icons _ Dictionary new ].
	^ Icons! !

!HaloMorph class methodsFor: 'cached state access' stamp: 'jmv 4/20/2015 15:52'!
releaseClassCachedState

	HandleSize _ nil.
	Icons _ nil! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/20/2015 15:37'!
snapshot: save andQuit: quit embedded: embeddedFlag
"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
| activeProc |
activeProc _ Processor activeProcess.
[ 
	| resuming world |
	self logSapshot: save andQuit: quit.
	TranscriptWindow allInstancesDo: [ :each | each delete ].
	Transcript logToFile: false.
	ActiveModel flushEventSystem.
	self processShutDownList: quit.


	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	world _ ProjectX ui.
	ProjectX stopUIProcess.
	activeProc isTerminated ifFalse: [
		activeProc terminate ].

	"Clean Globals"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: nil.
	Smalltalk closeSourceFiles. Smalltalk at: #SourceFiles put: nil.
	Smalltalk allClassesDo: [ :cls | cls releaseClassCachedState ].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	
	"To keep cleaning stuff that shouldn't be saved..."
	false not ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


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
	
	WorldState addDeferredUIMessage: [
		TranscriptWindow openTranscript ].

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 4/20/2015 15:52'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: self class handleSize asPoint).
	(iconName _ handleSpec iconSymbol) ifNotNil: [
			| form |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/20/2015 15:51'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self class handleSize.
	anExtent _ ((extent x + self class handleSize + 8) max: minSide) @
				((extent y + self class handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !


!HaloMorph class methodsFor: 'class initialization' stamp: 'jmv 4/20/2015 15:54'!
initialize
	"HaloMorph initialize"
	
	Preferences preferenceAt: #haloEnclosesFullBounds ifAbsent: [
		Preferences addPreference: #haloEnclosesFullBounds
			category: #halos
			default: false
			balloonHelp: 'if true, halos will enclose the full bounds of the target Morph, rather than just the bounds'
	].
 
	Preferences resetHaloSpecifications! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:52'!
haloColorIcon

	^ self icons
		at: #haloColorIcon
		ifAbsentPut: [ Theme current haloColorIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloDebugIcon

	^ self icons
		at: #haloDebugIcon
		ifAbsentPut: [ Theme current haloDebugIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloDismissIcon

	^ self icons
		at: #haloDismissIcon
		ifAbsentPut: [ Theme current haloDismissIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloDragIcon

	^ self icons
		at: #haloDragIcon
		ifAbsentPut: [ Theme current haloDragIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloDuplicateIcon

	^ self icons
		at: #haloDuplicateIcon
		ifAbsentPut: [ Theme current haloDuplicateIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloFontEmphasisIcon

	^ self icons
		at: #haloFontEmphasisIcon
		ifAbsentPut: [ Theme current haloFontEmphasisIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloFontSizeIcon

	^ self icons
		at: #haloFontSizeIcon
		ifAbsentPut: [ Theme current haloFontSizeIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloGrabIcon

	^ self icons
		at: #haloGrabIcon
		ifAbsentPut: [ Theme current haloGrabIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloHelpIcon

	^ self icons
		at: #haloHelpIcon
		ifAbsentPut: [ Theme current haloHelpIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloMenuIcon

	^ self icons
		at: #haloMenuIcon
		ifAbsentPut: [ Theme current haloMenuIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloRotateIcon

	^ self icons
		at: #haloRotateIcon
		ifAbsentPut: [ Theme current haloRotateIcon ]! !

!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 4/20/2015 15:53'!
haloScaleIcon

	^ self icons
		at: #haloScaleIcon
		ifAbsentPut: [ Theme current haloScaleIcon ]! !

HaloMorph initialize!
!methodRemoval: HaloMorph #handleSize!
HaloMorph removeSelector: #handleSize!
!methodRemoval: Time class #clearMillisecondClockOffset!
Time class removeSelector: #clearMillisecondClockOffset!
