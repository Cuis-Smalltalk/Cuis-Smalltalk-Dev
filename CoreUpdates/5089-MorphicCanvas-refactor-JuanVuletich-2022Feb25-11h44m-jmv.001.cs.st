'From Cuis 6.0 [latest update: #5087] on 25 February 2022 at 11:46:13 am'!

!Morph methodsFor: 'private' stamp: 'jmv 2/25/2022 10:14:37'!
privateDisplayBounds
	^privateDisplayBounds! !

!Morph methodsFor: 'private' stamp: 'jmv 2/25/2022 10:14:44'!
privateDisplayBounds: aRectangle
	privateDisplayBounds _ aRectangle! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:08'!
contourClearOnCurrentMorph
	"Remove knowledge of any contour."

	currentMorph setProperty: #contour toValue: nil.
	currentMorph setProperty: #contourY0 toValue: nil.
	currentMorph setProperty: #contourY1 toValue: nil.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:24:04'!
displayBoundsSetOnCurrentMorph
	"At time of last draw on world canvas. nil if unknown.
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Clipped appropiately if some owner clips currentMorph.
	See also #displayBoundsUpdateOn:"

	currentMorph canvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: self canvasToUse boundingRectOfCurrentMorphAfterDraw ]].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:24:29'!
displayBoundsUpdateOnCurrentMorph
	"At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Rectangle is clipped appropiately if some owner clips currentMorph.
	To be called after #postDrawOn:. See also #displayBoundsSetOn:
	This more complicated update is needed because the contour information from #drawOn: was lost
	when drawing submorphs. So, the contour from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	currentMorph canvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: 
				(currentMorph privateDisplayBounds quickMerge:
					self canvasToUse boundingRectOfCurrentMorphAfterDraw) ]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:31'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBoundsForError.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft+1 to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight + (w negated@1) to: r bottomLeft + (1@ w negated) width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourClearOnCurrentMorph.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:28'!
drawCurrentAsOutline

	currentMorph visible ifTrue: [
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.
		self frameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:25:11'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	self displayBoundsSetOnCurrentMorph.! !

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 10:25:17'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	self displayBoundsSetOnCurrentMorph.! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 2/25/2022 11:05:23'!
drawCurrentAndSubmorphs
	
	currentMorph visible ifTrue: [

		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!methodRemoval: MorphicCanvas #contourOfCurrentMorphAfterDrawInto:into: stamp: 'jmv 2/25/2022 11:45:28'!
MorphicCanvas removeSelector: #contourOfCurrentMorphAfterDrawInto:into:!
!methodRemoval: BoxedMorph #displayBoundsUpdateFrom: stamp: 'jmv 2/25/2022 11:45:28'!
BoxedMorph removeSelector: #displayBoundsUpdateFrom:!
!methodRemoval: Morph #displayBoundsSetFrom: stamp: 'jmv 2/25/2022 11:45:28'!
Morph removeSelector: #displayBoundsSetFrom:!
!methodRemoval: Morph #displayBoundsUpdateFrom: stamp: 'jmv 2/25/2022 11:45:28'!
Morph removeSelector: #displayBoundsUpdateFrom:!
