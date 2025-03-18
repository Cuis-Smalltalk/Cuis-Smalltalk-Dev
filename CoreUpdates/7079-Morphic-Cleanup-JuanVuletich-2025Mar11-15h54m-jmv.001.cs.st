'From Cuis7.3 [latest update: #7071] on 11 March 2025 at 3:56:39 pm'!

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 3/10/2025 10:45:16'!
pvtSetForm: aForm

	| s |
	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	s := targetWidth * targetHeight.
	morphIds := WordArray new: s.
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


!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/11/2025 15:56:19'!
primResetContourTop: t bottom: b contour: aFloat32Array
	<primitive: 'primReset2Contour' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/11/2025 15:56:23'!
primUpdateContourLastLine: aFloat32Array
	<primitive: 'primUpdate2ContourLastLine' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !


!VectorEngineWithPlugin class methodsFor: 'instance creation' stamp: 'jmv 3/11/2025 15:55:49'!
isPluginAvailable
	"
	VectorEngineWithPlugin isPluginAvailable
	Check if services provided by VM plugin match this class requirements.
	"
	| pluginApiVersion |
	pluginApiVersion := self pluginApiVersion.
	^ pluginApiVersion = 6! !

!methodRemoval: VectorEngineWithPlugin #primUpdateContourLastLine stamp: 'jmv 3/11/2025 15:56:05'!
VectorEngineWithPlugin removeSelector: #primUpdateContourLastLine!
!methodRemoval: VectorEngineWithPlugin #primResetContourTop:bottom: stamp: 'jmv 3/11/2025 15:55:53'!
VectorEngineWithPlugin removeSelector: #primResetContourTop:bottom:!
