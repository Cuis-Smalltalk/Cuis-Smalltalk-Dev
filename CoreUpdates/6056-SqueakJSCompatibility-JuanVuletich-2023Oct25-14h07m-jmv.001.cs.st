'From Cuis 6.0 [latest update: #6055] on 25 October 2023 at 2:07:44 pm'!

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 10/25/2023 14:07:07'!
pvtSetForm: aForm

	| s |
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
	Smalltalk isSpur ifTrue: [
		Smalltalk primitiveGarbageCollect.
		"Display bits is pinned by the VM. Pinning these large arrays should relieve some burden on the GC.
		Additionally, VectorEngineWithPlugin>>#protected: ensures that while drawing a path no one else will
		use the plugin until path is finished. This means that, on Spur with pinned arrays, arguments
			targetBits, targetWidth, targetHeight, morphIds, edgeCounts, alphaMask, contour
		can be removed from most calls. (see #initializePath)"
		aForm bits pin.
		morphIds pin. edgeCounts pin. alphaMask pin. contour pin ].
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

