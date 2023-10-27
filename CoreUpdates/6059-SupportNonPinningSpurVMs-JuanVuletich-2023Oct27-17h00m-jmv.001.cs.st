'From Cuis 6.0 [latest update: #6053] on 27 October 2023 at 5:06:21 pm'!

!Object methodsFor: 'private' stamp: 'jmv 10/27/2023 16:55:58'!
setPinned: aBoolean
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This primitive either pins or unpins an object, and answers if it was already pinned."
	"Note: In case of primitive error, assume pinning is not supported by this VM.
	Senders must check for nil return value and act accordingly.
	For instance, on October 2023, SqueakJS doesn't support pinning, even if it
	supports Spur images."

	<primitive: 184 error: ec>
	"^self primitiveFailed"
	^nil! !

!Object methodsFor: 'pinning' stamp: 'jmv 10/27/2023 16:56:38'!
pin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is pinned, and answers whether it was already pinned."
	"Note: Some VMs might not support pinning (for instance, SqueakJS on October 2023).
	If the primitive fails, answer nil.
	Senders must check for nil return value and act accordingly."

	^self setPinned: true! !

!Object methodsFor: 'pinning' stamp: 'jmv 10/27/2023 16:56:46'!
unpin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is unpinned, and answers whether it was pinned."
	"Note: Some VMs might not support pinning (for instance, SqueakJS on October 2023).
	If the primitive fails, answer nil.
	Senders must check for nil return value and act accordingly."

	^self setPinned: false! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 10/27/2023 17:05:46'!
pvtSetForm: aForm

	| s |
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [ targetForm bits unpin ].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	contour ifNotNil: [ contour unpin ].

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

	"Pin if pinning is supported by the VM, NOP otherwise."
	"Pinning these large arrays should relieve some burden on the GC.
	Additionally, VectorEngineWithPlugin>>#protected: ensures that while drawing a path no one else will
	use the plugin until path is finished. This means that, on Spur with pinned arrays, arguments
		targetBits, targetWidth, targetHeight, morphIds, edgeCounts, alphaMask, contour
	can be removed from most calls. (see #initializePath)"
	"Warning: #pin may fail for Spur images if run on the SqueakJS VM, at least on October 2023.
	In order to remove them from any calls, it would be needed to ensure that the VM supports pinning!!"
	targetForm bits pin. morphIds pin. edgeCounts pin. alphaMask pin. contour pin.

	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

