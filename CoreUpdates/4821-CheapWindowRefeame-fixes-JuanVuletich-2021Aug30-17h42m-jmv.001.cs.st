'From Cuis 5.0 [latest update: #4820] on 30 August 2021 at 5:45:18 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/30/2021 17:42:48'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
				self knowsOwnLocalBounds
					ifTrue: [
						"A good optimization in many cases.
						But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
						for example) this is essential."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds]
					ifFalse: [
						"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self wantsContour
							ifTrue: [
								 (aCanvas
									contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
									into: [ :contourArray :contourY0 :contourY1 |
										self setProperty: #contour toValue: contourArray.
										self setProperty: #contourY0 toValue: contourY0.
										self setProperty: #contourY1 toValue: contourY1.
								])]
							ifFalse: [
								self setProperty: #contour toValue: nil.
								self setProperty: #contourY0 toValue: nil.
								self setProperty: #contourY1 toValue: nil ]]]]].! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 8/30/2021 17:43:48'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		Preferences cheapWindowReframe or: [millisecondSinceLast > 200]]) ifTrue: [
			Preferences cheapWindowReframe ifFalse: [ Preferences enable: #cheapWindowReframe ].
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtentInWorld ]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/30/2021 17:44:18'!
drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		currentMorph displayBoundsSetFrom: self.
		self frameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].! !

