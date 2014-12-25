'From Cuis 4.2 of 25 July 2013 [latest update: #2133] on 14 December 2014 at 2:39:25.858571 pm'!

!Form methodsFor: 'converting' stamp: 'jmv 12/14/2014 00:37'!
convertAlphaToZeroValueTransparency
	"For a 32bpp Form, for each pixel, take the alpha value, and if less than 128
	make the pixel value zero (i.e. transparent for 2, 4, 8 and 16 bpp)
	Side effect: make value of alpha = 0 or 128 for all pixels. Not a problem if we're converting to lower bpp anyway...
	This method also loses the lsb of the color components. Again, not a problem for going to lower bpp.
	"
	"
	| bananas1 |
	bananas1 _ Form fromFileNamed: 'bananas1.png'.
	bananas1 convertAlphaToZeroValueTransparency..
	bananas1 convertAlphaToZeroValueTransparency.
	(bananas1 asFormOfDepth: 16) display
	"
	| bitBlt map multiplier |
	self depth = 32 ifFalse: [
		^self ].
	
	"Prepare multiplier to multiply each pixel by 0.5 or 0 (according to alpha)"
	multiplier _ Form extent: self extent depth: 32.
	map _ ColorMap
		masks: { 16r80000000. 16r80000000. 16r80000000. 16r80000000}
		shifts:  { 0.  -8. -16. -24 }.
	bitBlt _ BitBlt new.
	bitBlt
		setDestForm: multiplier;
		sourceForm: self;
		colorMap: map;
		combinationRule: Form over;
		copyBits.
	"Now fix for the fact that using a ColorMap turns pixelValues 0's into 1's"
	bitBlt
		sourceForm: nil;
		colorMap: nil;
		fillBitmap: (Bitmap with: 16rFFFFFFFE);
		combinationRule: Form and;
		copyBits.
	"Apply multiplier to ourselves"
	bitBlt
		fillBitmap: nil;
		setDestForm: self;
		sourceForm: multiplier;
		combinationRule: Form rgbMul;
		copyBits.
	"Now correct for the fact that the multiplier had 0.5's and 0's, not 1's and 0's"
	bitBlt
		sourceForm: self;
		combinationRule: 20; "Form rgbAdd"
		copyBits! !


!DisplayObject methodsFor: 'displaying-generic' stamp: 'jmv 12/14/2014 14:38'!
displayOn: aForm at: aDisplayPoint
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn _ self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule _ Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing this might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn _ self asFormOfDepth: aForm depth ]."
			rule _ Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule
		fillColor: nil! !


!Form methodsFor: 'converting' stamp: 'jmv 12/14/2014 00:25'!
asFormOfDepth: d
	| newForm source |
	d = self depth ifTrue: [ ^self ].
	source _ self depth = 32
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm _ Form extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: (source boundingBox)
		from: 0@0 in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!!)"
	d = 32 ifTrue: [
		newForm fixAlpha ].
	^newForm! !

