'From Cuis 5.0 [latest update: #4112] on 14 April 2020 at 6:03:54 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/14/2020 17:53:47'!
clippingByCurrentMorphDo: aBlock
	| prevClipRect |

	prevClipRect _ self clipRect.
	self setClipRect: (prevClipRect intersect: self clippingRectForCurrentMorph).
	self clippingMorph: currentMorph.
	aBlock ensure: [
		self clippingMorph: nil.
		self setClipRect: prevClipRect. ]! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 3/6/2020 20:03:32'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						self textMorph hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].
	model actualContents isEmpty ifTrue: [
		self
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas  clippingByCurrentMorphDo: [
					aCanvas
						drawString: msg
						at: self viewableAreaTopLeft
						font: nil
						color: Theme current textEmptyDisplayMessage ]]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/14/2020 18:01:25'!
clippingRectForCurrentMorph
	"This rectangle is used for clipping submorphs in BitBltCanvas.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn."

	self subclassResponsibility! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/14/2020 17:55:05'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph"
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

