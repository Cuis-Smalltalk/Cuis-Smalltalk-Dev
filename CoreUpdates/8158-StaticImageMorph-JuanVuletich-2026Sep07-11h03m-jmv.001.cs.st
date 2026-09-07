'From Cuis7.9 [latest update: #8157] on 7 September 2026 at 11:04:17 am'!

!ImageMorph commentStamp: '<historical>' prior: 0!
ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form.

Use #image: to set my picture.

Structure:
 instance var		Type 		Description
 image			Form		The Form to use when drawing

Code examples:
	ImageMorph new openInWorld; grabFromScreen

	ImageMorph new image: (Form fromFileNamed: 'myGraphicsFileName'); openInWorld.

You may prefer StaticImageMorph!

!classDefinition: #StaticImageMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #StaticImageMorph
	instanceVariableNames: 'image scaledImage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!StaticImageMorph commentStamp: '<historical>' prior: 0!
A StaticImageMorph is very much like an ImageMorph. But it assumes its 'image' (a Form) won't change, and that it is safe to cache a scaled version to sppedup #drawOn:!


!StaticImageMorph methodsFor: 'accessing' stamp: 'jmv 7/Sep/2026, 1:40:23 pm (UTC)'!
color: aColor
	super color: aColor.
	(image depth = 1 and: [ aColor is: #Color ]) ifTrue: [
		image colors: {`Color transparent`. aColor}.
		scaledImage := nil.
		self redrawNeeded ].
! !

!StaticImageMorph methodsFor: 'accessing' stamp: 'jmv 7/Sep/2026, 1:39:05 pm (UTC)'!
image: anImage 

	image := anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ].
	scaledImage := nil.
	self morphExtent: self minimumExtent.
	self redrawNeeded.! !

!StaticImageMorph methodsFor: 'drawing' stamp: 'jmv 7/Sep/2026, 2:01:30 pm (UTC)'!
drawOn: aCanvas

	| ct scale |
	((padding > `0@0`) or: [self morphExtent > self minimumExtent]) 
		ifTrue: [ 	| innerOrigin |
			"Make use of my color in area between border and image"
			innerOrigin := borderWidth @ borderWidth.
			aCanvas
				fillRectangle:  (innerOrigin extent: extent - (2 * innerOrigin ))
				color:  color].
			
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle:  (`0@0` extent: extent)
			color:  borderColor
			borderWidth:  borderWidth 
			borderStyleSymbol:  #simple ].

	ct := aCanvas currentTransformation.
	ct doesNotRotate ifTrue: [
		scale := ct scale.
		(scaledImage isNil or: [ scaledImage extent ~= (image extent * scale) truncated ]) ifTrue: [
			scaledImage := image magnifyBy: scale ].
		aCanvas geometryTransformation: (MorphicTranslation withTranslation: ct translation).
		aCanvas image: scaledImage at: self interiorOrigin.
		aCanvas geometryTransformation: ct.
		^self ].

	scaledImage := nil.
	aCanvas image: image at: self interiorOrigin.
! !

!StaticImageMorph methodsFor: 'drawing' stamp: 'KenD 9/Sep/2024, 8:24:29 pm (UTC)'!
minimumExtent

	^ image extent + (2 * self interiorOrigin)! !

!StaticImageMorph methodsFor: 'initialization' stamp: 'jmv 13/Oct/2020, 12:45:40 am (UTC)'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !

!StaticImageMorph methodsFor: 'menu commands' stamp: 'Squeak1.2 29/Jun/1997, 1:40:55 pm (UTC)'!
grabFromScreen
	self image: Form fromUser! !

!StaticImageMorph methodsFor: 'menu commands' stamp: 'len 20/May/2020, 9:17:35 am (UTC)'!
readFromFile
	self request: 'Enter file name'
		initialAnswer: 'fileName'
		do: [:fileName| self image: (Form fromFileNamed: fileName)]! !


!StaticImageMorph class methodsFor: 'new-morph participation' stamp: 'pb 9/Jun/2017, 2:54:09 am (UTC)'!
categoryInNewMorphMenu
	^ 'Basic'! !

!StaticImageMorph class methodsFor: 'instance creation' stamp: 'jmv 21/Oct/2025, 7:11:44 pm (UTC)'!
fromFileNamed: filename
	^self newWith: (Form fromFileNamed: filename)! !

!StaticImageMorph class methodsFor: 'instance creation' stamp: 'jmv 7/Sep/2026, 1:59:15 pm (UTC)'!
new
	^super new
		image: ImageMorph defaultForm.! !

!StaticImageMorph class methodsFor: 'instance creation' stamp: 'jmv 1/May/2024, 6:52:45 pm (UTC)'!
newWith: aForm
	^super new image: aForm! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 28/Nov/2025, 4:13:00 pm (UTC)'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resampled image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway."

	"We need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			| p bitBlt |
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			bitBlt := BitBltCanvasEngine toForm: form.
			bitBlt
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: 0; sourceY: 0;
				destX: p x destY: p y width: srcForm width height: srcForm height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			"srcBox := srcBox origin extent: srcBox extent - 1."
			srcBox := srcForm boundingBox.
			dstBox := currentTransformation externalBoundingRectOf: srcBox.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) floor ].
			"dstBox := srcBox translatedBy: aPoint."
			dstBox := aPoint extent: srcBox extent.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

