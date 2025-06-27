'From Cuis7.5 [latest update: #7289] on 27 June 2025 at 10:08:39 am'!
!classDefinition: 'DisplayScreen class' category: #'Graphics-Display Objects'!
DisplayScreen class
	instanceVariableNames: 'DeferredUpdatingDisplay '!

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 09:49:49'!
clearDeferredUpdatingDisplay

	DeferredUpdatingDisplay ifNotNil: [
		DeferredUpdatingDisplay bits unpin.
		DeferredUpdatingDisplay := nil ].! !

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 09:43:00'!
deferredUpdatingDisplay
	"Answer the instance to be used from most clients, including Morphic.
	If the platform supports #deferUpdates:, it will be Display itself.
	If the platform does NOT support #deferUpdates, it will be a separate Form.
	Methods in this category provide convenient access."

	DeferredUpdatingDisplay ifNil: [
		 DeferredUpdatingDisplay := (Display deferUpdates: true)
			"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
			ifNotNil: [ Display ]
			"Mac VMs don't support deferUpdates:, so Morphic Canvas target is this aux Form."
			ifNil: [ Form extent: Display extent depth: Display depth ]].
	^DeferredUpdatingDisplay! !

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 10:06:24'!
isDisplay: aFormOrDisplayScreen
	"Answer true if:
		Display
		DeferredUpdatingDisplay
	flase otherwise."
	aFormOrDisplayScreen == Display ifTrue: [ ^true ].
	aFormOrDisplayScreen == DeferredUpdatingDisplay ifTrue: [ ^true ].
	^false.! !

!DisplayScreen class methodsFor: 'cached state access' stamp: 'jmv 6/27/2025 09:59:01'!
releaseClassCachedState
	self clearDeferredUpdatingDisplay! !


!DisplayScreen methodsFor: 'private' stamp: 'jmv 6/27/2025 09:50:50'!
setExtent: aPoint depth: bitsPerPixel
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
	].! !

!DisplayScreen methodsFor: 'initialization' stamp: 'jmv 1/15/2018 15:51:20'!
initialize
	self
		setExtent: self class actualScreenSize
		depth: (self class actualScreenDepth ifNil: [ 32 ])! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 6/27/2025 09:51:05'!
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


!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 6/27/2025 10:07:23'!
unpinAndFreeMemory

	"Unpin, if possible, any pinned large arrays."
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [
		(DisplayScreen isDisplay: targetForm) ifFalse: [
			targetForm bits unpin ]].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	affectedBits ifNotNil: [ affectedBits unpin ].
	contour ifNotNil: [ contour unpin ].

	targetForm := nil.
	morphIds := nil.
	edgeCounts := nil.
	alphaMask := nil.
	affectedBits := nil.
	contour := nil.! !

!classDefinition: 'DisplayScreen class' category: #'Graphics-Display Objects'!
DisplayScreen class
	instanceVariableNames: 'DeferredUpdatingDisplay'!

!DisplayScreen class reorganize!
('display box access' actualScreenDepth actualScreenSize depth:width:height:fullscreen: isDisplayExtentOk)
('snapshots' setupDisplay shutDown startUp)
('host OS window' hostOSScaleFactor hostOSWindowExtent hostOSWindowExtent: hostOSWindowPosition hostOSWindowPosition: hostOSWindowTitle:)
('primitives' primitiveScreenScaleFactor primitiveWindowPosition: primitiveWindowPosition:x:y: primitiveWindowSize: primitiveWindowSize:x:y: primitiveWindowTitle:string:)
('deferred updating' clearDeferredUpdatingDisplay deferredUpdatingDisplay isDisplay:)
('cached state access' releaseClassCachedState)
!

