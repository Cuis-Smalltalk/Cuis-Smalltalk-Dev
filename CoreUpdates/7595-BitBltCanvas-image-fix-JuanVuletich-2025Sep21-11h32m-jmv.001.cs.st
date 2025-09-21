'From Cuis7.5 [latest update: #7594] on 21 September 2025 at 11:33:03 am'!

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/21/2025 11:32:53'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	| r p |
	"p := (currentTransformation transform: aPoint) roundedHAFZ."
	p := (currentTransformation transform: (sourceRect translatedBy: aPoint) origin) roundedHAFZ.
	r := (form depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(form depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]! !

