'From Cuis7.3 [latest update: #7108] on 3 April 2025 at 9:51:26 am'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight targetAssumedOpaque morphIds edgeCounts alphaMask contour strokeColor dashedStrokeBits dashBitCount dashBitLength dashBitOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec affectedBits '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 4/1/2025 15:42:31'!
pvtSetForm: aForm

	| s |
	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	targetAssumedOpaque := true. 		"Set to false for true translucency on target."
	s := targetWidth * targetHeight.
	morphIds := WordArray new: s.
	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	affectedBits := ByteArray new: s+15 // 16.
	contour := Float32Array new: targetHeight * 2.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 4/1/2025 10:30:00'!
pinArrays
	"Pin if pinning is supported by the VM, NOP otherwise."
	"Warning: #pin may fail for Spur images if run on the SqueakJS VM, at least on October 2023."

	targetForm bits pin.
	morphIds pin.
	edgeCounts pin.
	alphaMask pin.
	affectedBits pin.
	contour pin.! !

!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 4/1/2025 10:29:52'!
unpinAndFreeMemory

	"Unpin, if possible, any pinned large arrays."
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [
		targetForm == Display ifFalse: [
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

!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight targetAssumedOpaque morphIds edgeCounts alphaMask affectedBits contour strokeColor dashedStrokeBits dashBitCount dashBitLength dashBitOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| activeProc guiRoot |
(nil inform: 'After this update, #7206,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates].').
[
	ChangeSet installing: '7206-affectedBits-ClassShape-JuanVuletich-2025Apr03-09h51m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '7206') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 7206-affectedBits-ClassShape-JuanVuletich-2025Apr03-09h51m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates].' print.
	Utilities logsUserChanges: true.
] forkAt: 39.
UISupervisor ui setupNewCanvasAndEngine.
UISupervisor newUIProcess.
Processor activeProcess terminate.
!

