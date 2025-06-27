'From Cuis7.5 [latest update: #7285] on 26 June 2025 at 5:39:06 pm'!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'Damage ScreenUpdaterProcess ScreenUpdaterSemaphore '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 6/26/2025 17:02:54'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	Display ifNil: [
		Smalltalk at: #Display put: DisplayScreen new.
		"See #recreateSpecialObjectsArray"
		Smalltalk specialObjectsArray at: 15 put: Display ].
	[
		Display setExtent: self actualScreenSize depth: Display nativeDepth.
		"{'New Smalltalk Display:'. self actualScreenSize} print."
	] on: OutOfMemory do: [
		"In case of memory emergency, use a much smaller Display"
		'Display allocation failed - Using Emergency (small) Display.'  print.
		'Resize Display (or OS Window) to retry Display allocation.'  print.
		Display setExtent: self actualScreenSize // 4 depth: Display nativeDepth ].! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 6/26/2025 17:03:00'!
shutDown 
	"Minimize Display memory saved in image"
	Display prepareForImageSave.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 6/26/2025 17:02:39'!
onDisplay
	"Answer an instance of the appropriate subclass targetting Display,
	or an intermediate buffer, as appropriate.
	Create a new VectorEngineDrawer or BitBltCanvasEngine as appropriate."
	        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"

	| canvasTarget |
	canvasTarget := (Display deferUpdates: true)
		"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
		ifNotNil: [ Display ]
		"Mac VMs don't support deferUpdates:, so Morphic Canvas target is this aux Form."
		ifNil: [ Form extent: Display extent depth: Display depth ].
	^self onForm: canvasTarget.! !

!methodRemoval: DisplayScreen class #terminateScreenUpdater stamp: 'jmv 6/26/2025 17:37:48'!
DisplayScreen class removeSelector: #terminateScreenUpdater!
!methodRemoval: DisplayScreen class #installScreenUpdater stamp: 'jmv 6/26/2025 17:37:47'!
DisplayScreen class removeSelector: #installScreenUpdater!
!methodRemoval: DisplayScreen class #screenUpdater stamp: 'jmv 6/26/2025 17:37:50'!
DisplayScreen class removeSelector: #screenUpdater!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!DisplayScreen class reorganize!
('display box access' actualScreenDepth actualScreenSize depth:width:height:fullscreen: isDisplayExtentOk)
('snapshots' setupDisplay shutDown startUp)
('host OS window' hostOSScaleFactor hostOSWindowExtent hostOSWindowExtent: hostOSWindowPosition hostOSWindowPosition: hostOSWindowTitle:)
('primitives' primitiveScreenScaleFactor primitiveWindowPosition: primitiveWindowPosition:x:y: primitiveWindowSize: primitiveWindowSize:x:y: primitiveWindowTitle:string:)
!

