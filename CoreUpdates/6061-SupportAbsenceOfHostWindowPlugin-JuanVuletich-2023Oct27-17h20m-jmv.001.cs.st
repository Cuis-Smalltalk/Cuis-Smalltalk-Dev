'From Cuis 6.0 [latest update: #6060] on 27 October 2023 at 5:27:16 pm'!

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 10/27/2023 17:26:29'!
primitiveWindowPosition: id

	<primitive: 'primitiveHostWindowPosition' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 10/27/2023 17:22:07'!
primitiveWindowSize: id

	<primitive: 'primitiveHostWindowSize' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 10/27/2023 17:21:53'!
primitiveWindowSize: id x: x y: y
	"Set the actual size of the host OS Window.

	DisplayScreen primitiveWindowSize: 1 x: 1024 y: 768
	DisplayScreen primitiveWindowSize: 1 x: 1024*2 y: 768*2
	Display extent
	"
	<primitive: 'primitiveHostWindowSizeSet' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 10/27/2023 17:21:17'!
primitiveWindowTitle: id string: titleStringOrUtf8Bytes

	<primitive: 'primitiveHostWindowTitle' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/27/2023 17:25:56'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p |
	DisplayScreen isDisplayExtentOk ifFalse: [
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		DisplayScreen setupDisplay.
		UISupervisor newUIProcess.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !

