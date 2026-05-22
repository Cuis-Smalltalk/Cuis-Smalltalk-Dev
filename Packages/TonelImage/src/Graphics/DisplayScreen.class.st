"
There is only one instance of me, Display. It is a global and is used to handle general user requests to deal with the whole display screen. 
	Although I offer no protocol, my name provides a way to distinguish this special instance from all other Forms. This is useful, for example, in dealing with saving and restoring the system.
	To change the depth of your Display...
		Display newDepth: 16.
		Display newDepth: 8.
		Display newDepth: 1.
Valid display depths are 1, 2, 4, 8, 16 and 32.  It is suggested that you run with your monitors setting the same, for better speed and color fidelity.  Note that this can add up to 4Mb for the Display form.  Finally, note that newDepth: ends by executing a 'ControlManager restore' which currently terminates the active process, so nothing that follows in the doit will get executed.

Depths 1, 2, 4 and 8 bits go through a color map to put color on the screen, but 16 and 32-bit color use the pixel values directly for RGB color (5 and 8 bits per, respectivlely).  The color choice an be observed by executing Color fromUser in whatever depth you are using.

"
Class {
	#name : 'DisplayScreen',
	#superclass : 'Form',
	#classInstVars : [
		'DeferredUpdatingDisplay'
	],
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'display box access' }
DisplayScreen class >> actualScreenDepth [
	<primitive: 'primitiveScreenDepth'>
	^ Display ifNotNil: [ Display depth ]
]

{ #category : 'display box access' }
DisplayScreen class >> actualScreenSize [
	"
	Actual size of the Display available to Cuis.
	Also see #primitiveWindowSize:
	
	DisplayScreen actualScreenSize
	"

	<primitive: 106>
	^ `640@480`
]

{ #category : 'deferred updating' }
DisplayScreen class >> clearDeferredUpdatingDisplay [

	DeferredUpdatingDisplay ifNotNil: [
		DeferredUpdatingDisplay bits unpin.
		DeferredUpdatingDisplay := nil ].
]

{ #category : 'deferred updating' }
DisplayScreen class >> deferredUpdatingDisplay [
	"Answer the instance to be used from most clients, including Morphic.
	If the platform supports #deferUpdates:
		Set Display to deferred upodating
		Answer will be Display itself.
	If the platform does NOT support #deferUpdates
		Answer a separate Form.
	Methods in this category provide convenient access."

	DeferredUpdatingDisplay ifNil: [
		 DeferredUpdatingDisplay := (Display deferUpdates: true)
			"Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display.
			Mac VMs newer than July 2025 also do support it.
			For VMs that don't support deferUpdates:, use this aux Form as Morphic Canvas target."
			ifNil: [ Form extent: Display extent depth: Display depth ]].
	^DeferredUpdatingDisplay
]

{ #category : 'display box access' }
DisplayScreen class >> depth: depthInteger width: widthInteger height: heightInteger fullscreen: aBoolean [
	"Force Squeak's window (if there's one) into a new size and depth."
	"
	DisplayScreen depth: 8 width: 1024 height: 768 fullscreen: true
	Seems to work on Windows, but not on Mac or Linux."

	<primitive: 92>
	self primitiveFail
]

{ #category : 'deferred updating' }
DisplayScreen class >> forceDeferredToScreen [
	"Force the entire display area to the screen"
	DeferredUpdatingDisplay ifNotNil: [
		self forceDeferredToScreen: DeferredUpdatingDisplay boundingBox ]
]

{ #category : 'deferred updating' }
DisplayScreen class >> forceDeferredToScreen: updateRect [
	"Force the given rectangular section of the Display to be copied to the host screen. Make it visible to user."

	DeferredUpdatingDisplay ifNil:
		[ ^self ].
	DisplayScreen isDisplayExtentOk ifFalse:
		[ ^self ].
	DeferredUpdatingDisplay == Display
		ifTrue:
			[Display forceToScreen: updateRect ]
		ifFalse:
			[
			(BitBlt toForm: Display)
				sourceForm: DeferredUpdatingDisplay;
				combinationRule: Form over;
				sourceRect: updateRect;
				destOrigin: updateRect topLeft;
				copyBits.
			"In Pre-July 2025 Mac VMs this step wasn't necessary
			Just blitting the off-Display on the actual Display made it visible.
			On newer VMs this step is also needed.
			It is also needed on Linux and Windows VMs if a separate
			DeferredUpdatingDisplay is used."
			Display forceToScreen: updateRect ].
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSScaleFactor [
	"
	DisplayScreen hostOSScaleFactor
	On MacOS, answers 2.0 for Retina displays set at lower resolutions (default for MacBooks internal displays).
	"
	^self primitiveScreenScaleFactor
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSWindowExtent [
	"Actual size of the host OS Window.
	Also see #actualScreenSize

	DisplayScreen hostOSWindowExtent
	"

	^self primitiveWindowSize: 1
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSWindowExtent: aPoint [
	"Set the actual size of the host OS Window.

	DisplayScreen hostOSWindowExtent: 1024@768
	DisplayScreen hostOSWindowExtent: 1024@768*2
	Display extent
	"

	^self primitiveWindowSize: 1 x: aPoint x asInteger y: aPoint y asInteger
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSWindowPosition [
	"Answer the topLeft of the host OS Window.

	DisplayScreen hostOSWindowPosition
	"

	^self primitiveWindowPosition: 1
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSWindowPosition: aPoint [
	"Set the topLeft of the host OS Window.

	DisplayScreen hostOSWindowPosition: 100@100
	"

	^self primitiveWindowPosition: 1 x: aPoint x asInteger y: aPoint y asInteger
]

{ #category : 'host OS window' }
DisplayScreen class >> hostOSWindowTitle: aString [
	"Set the label of the title bar of the host OS Window

	DisplayScreen hostOSWindowTitle: 'Cuis is Cool!'
	"

	^self primitiveWindowTitle: 1 string: aString asUtf8BytesOrByteString
]

{ #category : 'deferred updating' }
DisplayScreen class >> isDisplay: aFormOrDisplayScreen [
	"Answer true if:
		Display
		DeferredUpdatingDisplay
	flase otherwise."
	aFormOrDisplayScreen == Display ifTrue: [ ^true ].
	aFormOrDisplayScreen == DeferredUpdatingDisplay ifTrue: [ ^true ].
	^false.
]

{ #category : 'display box access' }
DisplayScreen class >> isDisplayExtentOk [
	"False if Cuis main window size has changed, but Display hasn't been updated yet."

	Display extent = DisplayScreen actualScreenSize ifTrue: [ ^true ].

	"Note: We assume here that this condition means that in last #setupDisplay a memory allocation
	problem occured, and we are using a 1/4 emergency Display.
	If this is the case, don't try to allocate a new Display unless the OS Display is resized again."
	Display extent = (DisplayScreen actualScreenSize // 4) ifTrue: [ ^true ].

	^false
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveScreenScaleFactor [

	<primitive: 'primitiveScreenScaleFactor'>
	^1.0
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveWindowPosition: id [

	<primitive: 'primitiveHostWindowPosition' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveWindowPosition: id x: x y: y [

	<primitive: 'primitiveHostWindowPositionSet' module: 'HostWindowPlugin'>
	
	"The primitive seems to always fail on Linux, but it works nevertheless."
	"^self primitiveFailed"
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveWindowSize: id [

	<primitive: 'primitiveHostWindowSize' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveWindowSize: id x: x y: y [
	"Set the actual size of the host OS Window.

	DisplayScreen primitiveWindowSize: 1 x: 1024 y: 768
	DisplayScreen primitiveWindowSize: 1 x: 1024*2 y: 768*2
	Display extent
	"
	<primitive: 'primitiveHostWindowSizeSet' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil
]

{ #category : 'primitives' }
DisplayScreen class >> primitiveWindowTitle: id string: titleStringOrUtf8Bytes [

	<primitive: 'primitiveHostWindowTitle' module: 'HostWindowPlugin'>
	"Answer nil if HostWindowPlugin is not supported."
	^nil
]

{ #category : 'cached state access' }
DisplayScreen class >> releaseClassCachedState [
	self clearDeferredUpdatingDisplay
]

{ #category : 'snapshots' }
DisplayScreen class >> setupDisplay [
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
		Display setExtent: self actualScreenSize // 4 depth: Display nativeDepth ].
]

{ #category : 'snapshots' }
DisplayScreen class >> shutDown  [
	"Minimize Display memory saved in image"
	Display prepareForImageSave.
]

{ #category : 'snapshots' }
DisplayScreen class >> startUp [
	"
	DisplayScreen startUp.
	Display forceToScreen.
	"
	self setupDisplay.
]

{ #category : 'displaying' }
DisplayScreen >> beDisplay [
	"Tell the interpreter to use the receiver as the current display image.
	Essential. See Object documentation whatIsAPrimitive.

	Note: Some VMs (in particular MacOS OpenSmalltalk VM as of 2023, but maybe others) will pin our bits,
	store the pointer in the VM and use it at will (for instance for updating OS Display).
	This means that Smalltalk must hold a strong reference to 'bits' until a new one is stored in a later call
	to this primitive.
	See senders (usually only one).
	See http://lists.squeakfoundation.org/archives/list/vm-dev@lists.squeakfoundation.org/message/F3I7L5IJT2J5RXCS3QXLLQ3TZ6KU6FD6/"

	<primitive: 102>
	self primitiveFailed.
]

{ #category : 'copying' }
DisplayScreen >> copyToDisableImmediateUpdates [
	"Answer an instance that shares our pixel storage.
	This is useful to prevent the VM from immediately updating the host platform pixels the user actually sees,
	especially on platforms where #deferUpdates: answers ni, meaning it doesn't really support doing this.
	Flicking is possible. For this reason Morphic doesn't use this technique.
	Preferred technique is to use #deferredUpdatingDisplay and related methods.
	"
	^self shallowCopy
]

{ #category : 'other' }
DisplayScreen >> deferUpdates: aBoolean [
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does.
	This may help eliminate possible flickering:
	Preferences
		name: #useAllwaysOffScreenDisplay
		category: #system
		value: false
	"
	"(Preferences at: #useAllwaysOffScreenDisplay)"
	"Just always do it on MacOS. The VM says the feature is supported as on Linux and Windows, but there is (sometimes)
	heavy flickering."
	Smalltalk platformName = 'Mac OS'
		ifTrue: [ ^nil ].

	"Note: If we disable VM defer updates (with this `& false` below), but answer notNil, the the Morphic workaround is not used.
	Then:
	- on Mac VMs you get a terrible slowdown, because each Display write requires an expensive thread switch in the VM.
	- On Linux and Windows you get a lot of flicking.
	(Note: Mac VMs older than July 2025 #primitiveDeferUpdates: would answer nil, as the primitive always failed)"
	"Both Linux and Windows VM do support display deferred updates, so Morphic Canvas target is Display."
	^self primitiveDeferUpdates: aBoolean "& false"
]

{ #category : 'private' }
DisplayScreen >> findAnyDisplayDepth [
	"Return any display depth that is supported on this system."
	^self findAnyDisplayDepthIfNone: [
		"Ugh .... now this is a biggie - a system that does not support
		any of the Squeak display depths at all."
		Smalltalk
			logError: 'Fatal error: This system has no support for any display depth at all.'
			inContext: thisContext
			to: 'CuisDebug'.
		Smalltalk quitPrimitive. "There is no way to continue from here"
	]
]

{ #category : 'private' }
DisplayScreen >> findAnyDisplayDepthIfNone: aBlock [
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock."
	#(32 16 8 4 2 1 -32 -16 -8 -4 -2 -1) do:[:bpp|
		(self supportsDisplayDepth: bpp) ifTrue:[^bpp].
	].
	^aBlock value
]

{ #category : 'displaying' }
DisplayScreen >> flash: aRectangle  [
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
]

{ #category : 'displaying' }
DisplayScreen >> flash: aRectangle with: aColor [

	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle
]

{ #category : 'other' }
DisplayScreen >> forceDisplayUpdate [
"We stopped using it because it seems that with #deferUpdates: true (as supported by Linux / Windows / Mac OS)
what we need is #forceToScreen instead. You might check code containing 'forceDisplayUpdate'"

	"On platforms that buffer screen updates, force the screen to be updated immediately. On other platforms, or if the primitive is not implemented, do nothing."

	<primitive: 231>
	"do nothing if primitive fails"
]

{ #category : 'other' }
DisplayScreen >> forceToScreen [
	"Force the entire display area to the screen"
	^self forceToScreen: self boundingBox
]

{ #category : 'other' }
DisplayScreen >> forceToScreen: aRectangle [
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.

]

{ #category : 'other' }
DisplayScreen >> fullScreenMode: aBoolean [
	"On platforms that support it, set full-screen mode to the value of the argument. (Note: you'll need to restore the Display after calling this primitive."
	"Display fullScreenMode: true. Display newDepth: Display depth"

	<primitive: 233>
	self primitiveFailed

]

{ #category : 'initialization' }
DisplayScreen >> initialize [
	self
		setExtent: self class actualScreenSize
		depth: (self class actualScreenDepth ifNil: [ 32 ])
]

{ #category : 'other' }
DisplayScreen >> newDepth: pixelSize [
"
	Display newDepth: 8.
	Display newDepth: 1.
"
	(self supportsDisplayDepth: pixelSize)
		ifFalse: [ ^self inform:'Display depth ', pixelSize printString, ' is not supported on this system' ].
	self newDepthNoRestore: pixelSize.
	self runningWorld ifNotNil: [ :w | w  buildMagnifiedBackgroundImage ].
	self restore.
]

{ #category : 'private' }
DisplayScreen >> newDepthNoRestore: pixelSize [
	"Change depths.  Check if there is enough space!  , di"
	| area need |
	pixelSize = depth ifTrue: [^ self  "no change"].
	pixelSize abs < self depth ifFalse:
		["Make sure there is enough space"
		area := Display boundingBox area. "pixels"
		need := (area * (pixelSize abs - self depth) // 8)  "new bytes needed"
				+ Smalltalk lowSpaceThreshold.
		(Smalltalk garbageCollectMost <= need
			and: [Smalltalk garbageCollect <= need])
			ifTrue: [self error: 'Insufficient free space']].
	self setExtent: self extent depth: pixelSize.
	DisplayScreen startUp
]

{ #category : 'disk I/O' }
DisplayScreen >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a reference to the Display in the other system instead.  "

	"A path to me"
	dp := DiskProxy global: #Display selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp

]

{ #category : 'private' }
DisplayScreen >> prepareForImageSave [
	"Use little memory in saved image"

	self setExtent: `240@120` depth: self nativeDepth.
	bits unpin
]

{ #category : 'private' }
DisplayScreen >> primRetryShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. Do nothing if it fails. "

	<primitive: 127>
	"do nothing if primitive fails"

]

{ #category : 'private' }
DisplayScreen >> primShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates."

	<primitive: 127>
	"if this fails, coerce coordinates to integers and try again"
	self primRetryShowRectLeft: l truncated
		right: r rounded
		top: t truncated
		bottom: b rounded.

]

{ #category : 'other' }
DisplayScreen >> primitiveDeferUpdates: aBoolean [
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails."

	<primitive: 126>
	^ nil  "answer nil if primitive fails"

]

{ #category : 'other' }
DisplayScreen >> restore [
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]
]

{ #category : 'other' }
DisplayScreen >> restoreAfter: aBlock [
	"
	- Evaluate the block
	- Update host OS Display
	- Wait for a mouse click
	- And then restore the Morphic World"

	aBlock value.
	self forceToScreen.
	Sensor waitButton.
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]
]

{ #category : 'private' }
DisplayScreen >> setExtent: aPoint depth: bitsPerPixel [
	"DisplayScreen startUp"
	"This method is critical. If the setExtent fails, there will be no
	proper display on which to show the error condition."

	| bitsPerPixelToUse oldBits |
	DisplayScreen clearDeferredUpdatingDisplay.
	(depth = bitsPerPixel and: [aPoint = self extent and: [
					self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bitsPerPixelToUse := (self supportsDisplayDepth: bitsPerPixel)
			ifTrue: [ bitsPerPixel ]
			ifFalse: [
				(self supportsDisplayDepth: bitsPerPixel negated)
					ifTrue: [ bitsPerPixel negated ]
					ifFalse: [ self findAnyDisplayDepth ]].
		"Keep the old bits with a hard reference until the #beDisplay call below."
		oldBits := bits.
		"This super call will assign the new contents of the 'bits' instance variable.
		The old one may be still used by the VM (see comment at #beDisplay),
		but it is also stored in the oldBits variable, so it is safe."
		super setExtent: aPoint depth: bitsPerPixelToUse.
		"Let the VM know our bits have changed.
		See http://lists.squeakfoundation.org/archives/list/vm-dev@lists.squeakfoundation.org/message/F3I7L5IJT2J5RXCS3QXLLQ3TZ6KU6FD6/	"
		"Usually you don't create new instances of DisplayScreen, but if ever done, do what follows only for the real Display."
		self == Display ifTrue: [
			"It is important to keep oldBits referencing the bits passed to the VM in the previous
			call to #beDisplay until AFTER we passed it the new value in the call that follows."
			self beDisplay ].
		"Most likely not needed, even if some VMs could have pinned it.
		Still, this is a way to ensure that no optimizer may ever try to kill the oldBits before we're sure the VM doesn't need us to keep a reference to it."
		oldBits unpin.
		"Only now it is safe to no longer reference oldBits."
		oldBits := nil.
	].
]

{ #category : 'private' }
DisplayScreen >> setExtent: extent depth: bitsPerPixel bits: bitmap [
	"Please call #setExtent:depth:
	Or eventually implement this method like that."

	self shouldNotImplement.
]

{ #category : 'printing' }
DisplayScreen >> storeOn: aStream [
	self == Display ifTrue: [
		^ aStream nextPutAll: 'Display'].
	^ super storeOn: aStream
]

{ #category : 'other' }
DisplayScreen >> supportedDisplayDepths [
	"Return all pixel depths supported on the current host platform."
	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]
]

{ #category : 'other' }
DisplayScreen >> supportsDisplayDepth: pixelDepth [
	"Return true if this pixel depth is supported on the current host platform.
	Primitive. Optional."
	<primitive: 91>
	^#(1 2 4 8 16 32) includes: pixelDepth
]
