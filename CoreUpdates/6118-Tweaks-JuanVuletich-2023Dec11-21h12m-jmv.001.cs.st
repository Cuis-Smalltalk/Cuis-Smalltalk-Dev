'From Cuis6.0 [latest update: #6115] on 11 December 2023 at 9:19:39 pm'!

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 12/11/2023 21:14:51'!
beDisplay
	"Tell the interpreter to use the receiver as the current display image.
	Essential. See Object documentation whatIsAPrimitive.

	Note: Some VMs (in particular MacOS OpenSmalltalk VM as of 2023, but maybe others) will pin our bits,
	store the pointer in the VM and use it at will (for instance for updating OS Display).
	This means that Smalltalk must hold a strong reference to 'bits' until a new one is stored in a later call
	to this primitive.
	See senders (usually only one).
	See http://lists.squeakfoundation.org/archives/list/vm-dev@lists.squeakfoundation.org/message/F3I7L5IJT2J5RXCS3QXLLQ3TZ6KU6FD6/"

	<primitive: 102>
	self primitiveFailed.! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 12/11/2023 16:54:59'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	Display ifNil: [
		Smalltalk at: #Display put: DisplayScreen new.
		"See #recreateSpecialObjectsArray"
		Smalltalk specialObjectsArray at: 15 put: Display ].
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	self installScreenUpdater.! !

