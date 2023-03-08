'From Cuis 6.0 [latest update: #5677] on 8 March 2023 at 6:00:16 pm'!
!classDefinition: #WorldMorph category: #'Morphic-Kernel'!
PasteUpMorph subclass: #WorldMorph
	instanceVariableNames: 'activeHand hands canvas damageRecorder stepList lastCycleTime alarms lastAlarmTime deferredUIMessages drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent taskbar '
	classVariableNames: 'ScaleFactor '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:39:59'!
hostOSScaleFactor
	"
	DisplayScreen hostOSScaleFactor
	On MacOS, answers 2.0 for Retina displays set at lower resolutions (default for MacBooks internal displays).
	"
	^self primitiveScreenScaleFactor! !

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:43:00'!
hostOSWindowExtent
	"Actual size of the host OS Window.
	Also see #actualScreenSize

	DisplayScreen hostOSWindowSize
	"

	^self primitiveWindowSize: 1! !

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:43:13'!
hostOSWindowExtent: aPoint
	"Set the actual size of the host OS Window.

	DisplayScreen hostOSWindowExtent: 1024@768
	DisplayScreen hostOSWindowExtent: 1024@768*2
	Display extent
	"

	^self primitiveWindowSize: 1 x: aPoint x asInteger y: aPoint y asInteger! !

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:06:47'!
hostOSWindowPosition
	"Answer the topLeft of the host OS Window.

	DisplayScreen hostOSWindowPosition
	"

	^self primitiveWindowPosition: 1! !

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:39:06'!
hostOSWindowPosition: aPoint
	"Set the topLeft of the host OS Window.

	DisplayScreen hostOSWindowPosition: 100@100
	"

	^self primitiveWindowPosition: 1 x: aPoint x asInteger y: aPoint y asInteger! !

!DisplayScreen class methodsFor: 'host OS window' stamp: 'jmv 3/8/2023 17:45:26'!
hostOSWindowTitle: aString
	"Set the label of the title bar of the host OS Window

	DisplayScreen hostOSWindowTitle: 'Cuis is Cool!!'
	"

	^self primitiveWindowTitle: 1 string: aString asUtf8BytesOrByteString! !


!WorldMorph class methodsFor: 'class initialization' stamp: 'jmv 3/8/2023 17:59:31'!
initClassCachedState
	ScaleFactor _ DisplayScreen hostOSScaleFactor! !


!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:39:32'!
primitiveScreenScaleFactor

	<primitive: 'primitiveScreenScaleFactor'>
	^1.0! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:06:58'!
primitiveWindowPosition: id

	<primitive: 'primitiveHostWindowPosition' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:39:15'!
primitiveWindowPosition: id x: x y: y

	<primitive: 'primitiveHostWindowPositionSet' module: 'HostWindowPlugin'>
	
	"The primitive seems to always fail on Linux, but it works nevertheless."
	"^self primitiveFailed"! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:40:15'!
primitiveWindowSize: id

	<primitive: 'primitiveHostWindowSize' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:45:45'!
primitiveWindowTitle: id string: titleStringOrUtf8Bytes

	<primitive: 'primitiveHostWindowTitle' module: 'HostWindowPlugin'>
	^self primitiveFailed! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 3/8/2023 17:59:50'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e d p |
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Minimize the risk of going out of memory:
		- First clear existing canvas, to free the memory it uses.
		- Then, setup the display.
		- Then set up new canvas."
		self clearCanvas.
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor _ DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor _ newScaleFactor.
			e _ Display extent.
			d _ DisplayScreen hostOSWindowExtent * ScaleFactor - DisplayScreen actualScreenSize.
			p _ DisplayScreen hostOSWindowPosition.
			DisplayScreen hostOSWindowExtent: e+d.
			DisplayScreen hostOSWindowPosition: p.
		].
		DisplayScreen setupDisplay.
		self setMainCanvas.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !


!DisplayScreen class reorganize!
('display box access' actualScreenDepth actualScreenSize depth:width:height:fullscreen: isDisplayExtentOk)
('snapshots' setupDisplay shutDown startUp)
('screen update' installScreenUpdater screenUpdateRequired: screenUpdater terminateScreenUpdater)
('host OS window' hostOSScaleFactor hostOSWindowExtent hostOSWindowExtent: hostOSWindowPosition hostOSWindowPosition: hostOSWindowTitle:)
('primitives' primitiveScreenScaleFactor primitiveWindowPosition: primitiveWindowPosition:x:y: primitiveWindowSize: primitiveWindowSize:x:y: primitiveWindowTitle:string:)
!

!classDefinition: #WorldMorph category: #'Morphic-Kernel'!
PasteUpMorph subclass: #WorldMorph
	instanceVariableNames: 'activeHand hands canvas damageRecorder stepList lastCycleTime alarms lastAlarmTime deferredUIMessages drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent taskbar'
	classVariableNames: 'ScaleFactor'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!WorldMorph class reorganize!
('instance creation' newWorld)
('class initialization' initClassCachedState)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
WorldMorph initClassCachedState!

