'From Cuis 6.0 [latest update: #5932] on 19 July 2023 at 2:47:07 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/19/2023 14:46:14'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: on a BoundsFinderCanvas
	to explore affected pixels to deduce actual morph bounds.

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/19/2023 14:46:16'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If False, we can be drawn by BitBltCanvas, and needs to implement:
		#morphExtent and #topLeftInOwner

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ true! !


!BoxedMorph methodsFor: 'geometry testing' stamp: 'jmv 7/19/2023 14:46:23'!
knowsOwnLocalBounds
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: on a BoundsFinderCanvas
	to explore affected pixels to deduce actual morph bounds.
	
	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^true! !

!BoxedMorph methodsFor: 'geometry testing' stamp: 'jmv 7/19/2023 14:46:54'!
requiresVectorCanvas
	"Widgets can usually run with any kind of Canvas, but not if zoomed or rotated.
	Note: Subclasses that use VectorGraphics for their drawing should answer true.

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ location isPureTranslation not or: [ owner notNil and: [ owner requiresVectorCanvas ]].! !

