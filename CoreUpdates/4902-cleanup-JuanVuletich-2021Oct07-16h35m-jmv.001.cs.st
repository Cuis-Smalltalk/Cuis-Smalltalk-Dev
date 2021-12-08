'From Cuis 5.0 [latest update: #4901] on 7 October 2021 at 4:39:35 pm'!

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 15:41:39'!
restorePatch: savedPatch bounds: savedBounds
	"Argument must previously be obtained by calling #savePatch:bounds:"

	self
		image: savedPatch
		at: savedBounds origin
		sourceRect: (0@0 extent: savedBounds extent)! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 15:42:14'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:bounds:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch prevExtent extent |
	savedPatch _ prevSavedPatch.
	prevExtent _ 0@0.
	((savedPatch is: #Form) and: [
		prevExtent _ savedPatch extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				savedPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent _ aRectangle extent max: prevExtent.
				extent _ (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				savedPatch _ Form extent: extent depth: form depth ].
	[
		engine sourceForm: form; combinationRule: Form over;
			sourceX: aRectangle left; sourceY: aRectangle top;
			width: aRectangle width; height: aRectangle height;
			copyBits.
	] ensure: [
		engine setDestForm: form; sourceForm: nil ].
	^savedPatch! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 16:37:05'!
drawCurrentAsOutline

	currentMorph visible ifTrue: [
		currentMorph displayBoundsSetFrom: self.
		self frameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 16:37:09'!
drawCurrentAndSubmorphs
	
	currentMorph visible ifTrue: [

		currentMorph drawOn: self.
		currentMorph displayBoundsSetFrom: self.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			currentMorph displayBoundsUpdateFrom: self ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!methodRemoval: MorphicCanvas #savePatch:bounds: stamp: 'jmv 10/7/2021 16:35:35'!
MorphicCanvas removeSelector: #savePatch:bounds:!
!methodRemoval: MorphicCanvas #restorePatch:bounds: stamp: 'jmv 10/7/2021 16:35:35'!
MorphicCanvas removeSelector: #restorePatch:bounds:!
!methodRemoval: MorphicCanvas #image:at:sourceRect: stamp: 'jmv 10/7/2021 16:38:41'!
MorphicCanvas removeSelector: #image:at:sourceRect:!
!methodRemoval: MorphicCanvas #fillRectangle:tilingWith:multipliedBy: stamp: 'jmv 10/7/2021 16:39:03'!
MorphicCanvas removeSelector: #fillRectangle:tilingWith:multipliedBy:!
!methodRemoval: MorphicCanvas #isCurrentMorphVisible stamp: 'jmv 10/7/2021 16:37:26'!
MorphicCanvas removeSelector: #isCurrentMorphVisible!
