'From Cuis 5.0 [latest update: #4753] on 13 August 2021 at 4:08:28 pm'!

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 8/13/2021 16:07:35'!
knowsOwnLocalBounds
	"HaloMorph is a bit special because of the drawing of the coordinate systems.
	In VectorCanvas, with possible rotation, we must answer false, so #drawOn: is taking into accoung in computing bounds.
	In BitBltCanvas, #morphLocalBounds (that leaves extra room for coordinate syste) is enough, especially because there is no rotation.
	In this case, #morphLocalBounds will be used. BitBltCanvas cannot do anything else!!"

	^false! !

!HaloMorph methodsFor: 'geometry' stamp: 'jmv 8/13/2021 15:51:20'!
morphLocalBounds

	"Leave some room for coordinate system labels (in BitBltCanvas)"
	^`-65 @ -30` corner: extent! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/13/2021 15:51:43'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].
	"Debugging aid"
	"aCanvas
		fillRectangle: haloBox
		color: (Color red alpha: 0.2).
	aCanvas
		fillRectangle: self morphLocalBounds
		color: (Color gray alpha: 0.4)."
	Preferences halosShowCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.3`.
			b _ target displayBounds.
			b = fb ifFalse: [
				aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.3` ].
			self drawCoordinateSystemOn: aCanvas ]].! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/13/2021 15:56:25'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self morphPosition: aDisplayRectangle topLeft extent: aDisplayRectangle extent.
	haloBox _ self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	self redrawNeeded.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/13/2021 16:06:55'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph displayBoundsSetFrom: self].! !

