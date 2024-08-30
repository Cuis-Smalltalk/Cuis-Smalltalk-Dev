'From Cuis7.1 [latest update: #6662] on 30 August 2024 at 1:54:39 pm'!

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 8/30/2024 11:07:16'!
pinArrays! !


!VectorCanvas methodsFor: 'other' stamp: 'jmv 8/30/2024 11:03:07'!
pinArrays
	engine pinArrays.
	super pinArrays.! !


!HybridCanvas methodsFor: 'other' stamp: 'jmv 8/30/2024 11:03:13'!
pinArrays
	vectorCanvas pinArrays.
	super pinArrays.! !


!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 8/30/2024 11:15:42'!
pinArrays
	"Pin if pinning is supported by the VM, NOP otherwise."
	"Warning: #pin may fail for Spur images if run on the SqueakJS VM, at least on October 2023."

	targetForm bits pin.
	morphIds pin.
	edgeCounts pin.
	alphaMask pin.
	contour pin.! !


!Form methodsFor: 'accessing' stamp: 'jmv 8/30/2024 13:52:50'!
getMainCanvas
	"Return a Canvas that can be used to draw onto the receiver.
	In case of lack of memory, use a much cheaper (but limited) BitBltCanvas."

	| answer |
	answer := [ MorphicCanvas onForm: self ]
				on: OutOfMemory
				do: [
					'Not enough memory to run VectorEngine. Using BitBltCanvas instead.' print.
					MorphicCanvas activeSubclass: BitBltCanvas.
					MorphicCanvas onForm: self ].
	^answer.! !


!DisplayScreen methodsFor: 'private' stamp: 'jmv 8/30/2024 10:57:57'!
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
		"Most likely not needed, even if some VMs could have pinned it.
		Still, this is a way to ensure that no optimizer may ever try to kill the oldBits before we're sure the VM doesn't need us to keep a reference to it."
		oldBits unpin.
		"Only now it is safe to no longer reference oldBits."
		oldBits := nil.
	].! !


!WorldMorph methodsFor: 'caching' stamp: 'jmv 8/30/2024 13:45:10'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	canvas ifNotNil: [
		canvas unpinAndFreeMemory ].
	canvas := nil.
	damageRecorder := nil.
	hands do: [ :h | h releaseCachedState ].! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/30/2024 13:47:56'!
mainCanvas: aMorphicCanvas
	"Set the main canvas"

	canvas == aMorphicCanvas ifFalse: [
		canvas ifNotNil: [
			canvas unpinAndFreeMemory ].
		(Preferences at: #saveReleaseOptionsInWorldMenu) ifFalse: [
			aMorphicCanvas pinArrays ].
		canvas := aMorphicCanvas.
	].
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self localBounds for: nil.
	waitDelay := Delay forMilliseconds: 50.! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 8/30/2024 11:08:02'!
pvtSetForm: aForm

	| s |
	"Unpin if pinning is supported by the VM, NOP otherwise."
	self unpinAndFreeMemory.

	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	s := targetWidth * targetHeight.
	self flag: #VEPapi5.		"Eventually remove the +1.
	Needed for VEPapi=5 and 6 (for plugins older than Sept-21-2021, and version less than 1.29).
	Avoid it altoghether if the plugin is not available (#pluginApiVersion answers 0)"
	morphIds := WordArray new: (
		(VectorEngineWithPlugin pluginApiVersion between: 5 and: 6)
			ifTrue: [s+1]
			ifFalse: [s]).
	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	contour := Float32Array new: targetHeight * 2.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 8/30/2024 11:04:33'!
unpinAndFreeMemory

	"Unpin, if possible, any pinned large arrays."
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [
		targetForm == Display ifFalse: [
			targetForm bits unpin ]].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	contour ifNotNil: [ contour unpin ].

	targetForm := nil.
	morphIds := nil.
	edgeCounts := nil.
	alphaMask := nil.
	contour := nil.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Bitmap allInstancesDoAlt: [ :fd | fd == Display bits ifFalse: [ fd unpin ]].
WordArray allInstancesDoAlt: [ :fd | fd unpin ].
ByteArray allInstancesDoAlt: [ :fd | fd unpin ].
Float32Array allInstancesDoAlt: [ :fd | fd unpin ].
IntegerArray allInstancesDoAlt: [ :fd | fd unpin ].
Smalltalk garbageCollect.!

