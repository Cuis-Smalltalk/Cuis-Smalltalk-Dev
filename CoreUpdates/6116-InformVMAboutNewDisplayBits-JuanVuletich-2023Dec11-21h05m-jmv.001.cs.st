'From Cuis6.0 [latest update: #6115] on 11 December 2023 at 9:10:30 pm'!

!DisplayScreen methodsFor: 'private' stamp: 'jmv 12/11/2023 21:08:53'!
setExtent: aPoint depth: bitsPerPixel
	"DisplayScreen startUp"
	"This method is critical. If the setExtent fails, there will be no
	proper display on which to show the error condition."

	| bitsPerPixelToUse oldBits |
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
		"Most likely not needed, even if some VMs could have pinned it."
		oldBits unpin.
		"Only now it is safe to no longer reference oldBits."
		oldBits := nil.
	].! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 12/11/2023 20:13:07'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	self installScreenUpdater.! !

