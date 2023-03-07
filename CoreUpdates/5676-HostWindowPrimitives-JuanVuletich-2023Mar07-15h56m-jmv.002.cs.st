'From Cuis 6.0 [latest update: #5674] on 7 March 2023 at 4:34:36 pm'!

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:34:31'!
primitiveScreenScaleFactor
	"
	DisplayScreen primitiveScreenScaleFactor
	On MacOS, answers 2 for Retina displays set at lower resolutions (default for MacBooks internal displays).
	"
	<primitive: 'primitiveScreenScaleFactor'>
	^1.0! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:26:18'!
primitiveWindowPosition: id
	"Answer the topLeft of the host OS Window.

	DisplayScreen primitiveWindowPosition: 1
	"
	<primitive: 'primitiveHostWindowPosition' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:26:26'!
primitiveWindowPosition: id x: x y: y
	"Set the topLeft of the host OS Window.

	DisplayScreen primitiveWindowPosition: 1 x: 20 y: 200
	"
	<primitive: 'primitiveHostWindowPositionSet' module: 'HostWindowPlugin'>
	
	"The primitive seems to always fail on Linux, but it works nevertheless."
	"^self primitiveFailed"! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:22:49'!
primitiveWindowSize: id
	"Actual size of the host OS Window.
	Also see #actualScreenSize

	DisplayScreen primitiveWindowSize: 1
	"

	<primitive: 'primitiveHostWindowSize' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:23:13'!
primitiveWindowSize: id x: x y: y
	"Set the actual size of the host OS Window.

	DisplayScreen primitiveWindowSize: 1 x: 1024 y: 768
	DisplayScreen primitiveWindowSize: 1 x: 1024*2 y: 768*2
	Display extent
	"
	<primitive: 'primitiveHostWindowSizeSet' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:24:00'!
primitiveWindowTitle: id string: titleString
	"Set the label of the title bar of the host OS Window

	DisplayScreen primitiveWindowTitle: 1 string: 'Cuis is Cool!!'
	"

	<primitive: 'primitiveHostWindowTitle' module: 'HostWindowPlugin'>
	^self primitiveFailed! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:20:59'!
actualScreenSize
	"
	Actual size of the Display available to Cuis.
	Also see #primitiveWindowSize:
	
	DisplayScreen actualScreenSize
	"

	<primitive: 106>
	^ `640@480`! !

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 3/7/2023 16:03:03'!
depth: depthInteger width: widthInteger height: heightInteger fullscreen: aBoolean
	"Force Squeak's window (if there's one) into a new size and depth."
	"
	DisplayScreen depth: 8 width: 1024 height: 768 fullscreen: true
	Seems to work on Windows, but not on Mac or Linux."

	<primitive: 92>
	self primitiveFail! !


!DisplayScreen class reorganize!
('display box access' actualScreenDepth actualScreenSize depth:width:height:fullscreen: isDisplayExtentOk primitiveScreenScaleFactor primitiveWindowPosition: primitiveWindowPosition:x:y: primitiveWindowSize: primitiveWindowSize:x:y: primitiveWindowTitle:string:)
('snapshots' setupDisplay shutDown startUp)
('screen update' installScreenUpdater screenUpdateRequired: screenUpdater terminateScreenUpdater)
!

