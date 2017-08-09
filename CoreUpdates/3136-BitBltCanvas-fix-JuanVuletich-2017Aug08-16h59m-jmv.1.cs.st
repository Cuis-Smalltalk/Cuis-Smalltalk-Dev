'From Cuis 5.0 of 7 November 2016 [latest update: #3134] on 9 August 2017 at 11:37:48 am'!

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 8/9/2017 11:37:34'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r.
	(self depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		port sourceForm: nil.
		port combinationRule: 40. "fixAlpha:with:"
		port copyBits ]! !

