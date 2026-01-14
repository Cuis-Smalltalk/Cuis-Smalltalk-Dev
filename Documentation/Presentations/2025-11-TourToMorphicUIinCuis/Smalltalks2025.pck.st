'From Cuis7.5 [latest update: #7687] on 4 November 2025 at 12:30:00 pm'!
'Description '!
!provides: 'Smalltalks2025' 1 2!
!requires: 'Morphic-AnimatedStar' 1 0 nil!
!requires: 'Morphic-Shapes' 1 0 nil!
SystemOrganization addCategory: #Smalltalks2025!


!classDefinition: #Smalltalks2025PresentationSlide category: #Smalltalks2025!
BorderedBoxMorph subclass: #Smalltalks2025PresentationSlide
	instanceVariableNames: 'prev next'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Smalltalks2025'!
!classDefinition: 'Smalltalks2025PresentationSlide class' category: #Smalltalks2025!
Smalltalks2025PresentationSlide class
	instanceVariableNames: ''!


!Smalltalks2025PresentationSlide commentStamp: 'jmv 11/3/2025 16:52:59' prior: 0!
"
Feature require: 'Smalltalks2025'
"
Smalltalks2025PresentationSlide allInstancesDo: [ :s | s delete ].

| first |
first := Smalltalks2025PresentationSlide slide01.
(((first
	next: Smalltalks2025PresentationSlide slide02 hide openInWorld)
	next: Smalltalks2025PresentationSlide slide03 hide openInWorld)
	next: Smalltalks2025PresentationSlide slide04 hide openInWorld).
first openInWorld getKeyboardFocus.!

!Smalltalks2025PresentationSlide methodsFor: 'initialization' stamp: 'jmv 10/23/2025 14:59:18'!
initialize
	super initialize.
	self
		color: (Color gray: 0.98);
		morphExtent: 1600@900;
		borderWidth: 1! !

!Smalltalks2025PresentationSlide methodsFor: 'initialization' stamp: 'jmv 11/3/2025 17:22:49'!
intoWorld: aWorld
	super intoWorld: aWorld.
	self scaleToFitInto: aWorld localBounds! !

!Smalltalks2025PresentationSlide methodsFor: 'initialization' stamp: 'jmv 11/3/2025 16:35:33'!
next: aSlide
	next := aSlide.
	aSlide prev: self.
	^aSlide! !

!Smalltalks2025PresentationSlide methodsFor: 'initialization' stamp: 'jmv 11/3/2025 16:33:26'!
prev: aSlide
	prev := aSlide.! !

!Smalltalks2025PresentationSlide methodsFor: 'events' stamp: 'jmv 11/4/2025 10:29:45'!
keyStroke: aKeyboardEvent
	aKeyboardEvent isArrowRight ifTrue: [
		next ifNotNil: [ 
			aKeyboardEvent shiftPressed ifFalse: [self hide].
			next show; getKeyboardFocus ] ].
	aKeyboardEvent isArrowLeft ifTrue: [
		prev ifNotNil: [
			aKeyboardEvent shiftPressed ifFalse: [self hide].
			prev show; getKeyboardFocus ] ].! !

!Smalltalks2025PresentationSlide methodsFor: 'geometry testing' stamp: 'jmv 11/4/2025 11:55:22'!
submorphsMightProtrude
	^true! !

!Smalltalks2025PresentationSlide class methodsFor: 'instance creation' stamp: 'jmv 11/4/2025 10:50:16'!
slide01
	"
	 Smalltalks2025PresentationSlide slide01 openInWorld.
	Smalltalks2025PresentationSlide allInstancesDo: [ :s | s delete ].
	"
	| slide |
	slide := self new.
	slide
		addMorph: (LabelMorph contents: 'The Morphic UI framework in Cuis' fontSize: 54 emphasis: 1)
			position: 40@380;
		addMorph: (LabelMorph contents: '...and what do we assume about UIs and how they are built' fontSize: 26 emphasis: 0)
			position: 40@480;
		addMorph: (LabelMorph contents: 'Juan Vuletich, November 2025' fontSize: 18 emphasis: 0)
			position: 60@800.
	^slide! !

!Smalltalks2025PresentationSlide class methodsFor: 'instance creation' stamp: 'jmv 11/3/2025 16:56:37'!
slide02
	"
	 Smalltalks2025PresentationSlide slide02 openInWorld.
	Smalltalks2025PresentationSlide allInstancesDo: [ :s | s delete ].
	"
	| slide |
	slide := self new.
	slide
		addMorph: (LabelMorph contents: 'Standard User Interfaces: Avalonia' fontSize: 44 emphasis: 1)
			position: 220@40;
""		addMorph: ((ImageMorph fromFileNamed: 'Avalonia-01.jpg') scale: 0.67)
			position: 50@128;
		addMorph: ((ImageMorph fromFileNamed: 'Avalonia-03.jpg') scale: 0.9)
			position: 850@310;""
		addMorph: (LabelMorph contents: 'ImageFan Reloaded' fontSize: 18 emphasis: 0)
			position: 130@854.
	^slide! !

!Smalltalks2025PresentationSlide class methodsFor: 'instance creation' stamp: 'jmv 11/4/2025 11:15:09'!
slide03
	"
	 Smalltalks2025PresentationSlide slide03 openInWorld.
	Smalltalks2025PresentationSlide allInstancesDo: [ :s | s delete ].
	"
	| slide |
	slide := self new.
	slide
		addMorph: (LabelMorph contents: 'Custom User Interfaces: Flutter' fontSize: 44 emphasis: 1)
			position: 270@32;
		addMorph: ((ImageMorph fromFileNamed: 'GoogleEarth-Flutter.jpg') scale: 0.95)
			position: 100@120;
		addMorph: (LabelMorph contents: 'Google Earth' fontSize: 18 emphasis: 0)
			position: 114@804;
		addMorph: ((ImageMorph fromFileNamed: 'YoutubeCreate-Flutter.jpg') scale: 0.5)
			position: 1200@120;
		addMorph: (LabelMorph contents: 'Youtube Create' fontSize: 18 emphasis: 0)
			position: 1320@840;
		addMorph: ((ImageMorph fromFileNamed: 'Robert-Felker-images.jpg') scale: 0.75)
			position: 320@520;
		addMorph: (LabelMorph contents: 'Robert Felker: Generative Art' fontSize: 18 emphasis: 0)
			position: 594@853.
	^slide! !

!Smalltalks2025PresentationSlide class methodsFor: 'instance creation' stamp: 'jmv 10/23/2025 15:18:39'!
slide04
	"
	 Smalltalks2025PresentationSlide slide04 openInWorld.
	Smalltalks2025PresentationSlide allInstancesDo: [ :s | s delete ].
	"
	| slide |
	slide := self new.
	slide
		addMorph: (LabelMorph contents: 'The evolution of the Morphic Framework' fontSize: 44 emphasis: 1)
			position: 100@70;
		addMorph: Smalltalks2025PresentationSlide slide4Text1
			position: 340@200;
		addMorph: Smalltalks2025PresentationSlide slide4Text2
			position: 880@310;
		addMorph: Smalltalks2025PresentationSlide slide4Text3
			position: 320@530;
		addMorph: Smalltalks2025PresentationSlide slide4Text4
			position: 980@600;
		addMorph: Smalltalks2025PresentationSlide arrow1
			position: 710@270;
		addMorph: Smalltalks2025PresentationSlide arrow2
			position: 390@450;
		addMorph: Smalltalks2025PresentationSlide arrow3
			position: 1000@500;
		addMorph: Smalltalks2025PresentationSlide arrow4
			position: 720@600.
	^slide! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:16:44'!
arrow1
	^CurvedArrow
		points: {0@0 . 200@0 . 0@110 . 130@110}
		strokeWidth: 4
		dashedStrokeBits: 2r110 dashedBitLength: 10
		color: Color lightBlue! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:16:40'!
arrow2
	^CurvedArrow
		points: {0@0 . 0@10 . 20@80 . 40@100}
		strokeWidth: 4
		dashedStrokeBits: 2r110 dashedBitLength: 10
		color: Color lightBlue! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:16:37'!
arrow3
	^CurvedArrow
		points: {0@0 . -80@80 . -200@ -100 . -400@60}
		strokeWidth: 4
		dashedStrokeBits: 2r110 dashedBitLength: 10
		color: Color lightBlue! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:16:21'!
arrow4
	^CurvedArrow
		points: {0@0 . 100@0 . 140@60 . 220@65}
		strokeWidth: 4
		dashedStrokeBits: 2r110 dashedBitLength: 10
		color: Color lightBlue! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:25:50'!
greenTextColor
	^Color fromHexString: '#5A9F2D'! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:26:00'!
redTextColor
	^Color fromHexString: '#941100'! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 11/4/2025 11:20:49'!
slide4Text1

	^ (TextParagraphMorph contents:
		('1980' pointSize: 24), String lfString centered,
		('MVC / Smalltalk-80' pointSize: 24) bold, String lfString centered, String lfString,
		('    + 100% fully modifiable Smalltalk code' color: self greenTextColor), String lfString,
		('    + View / Model Separation' color: self greenTextColor), String lfString,
		('    - One active window' color: self redTextColor), String lfString,
		('    - Limited composability' color: self redTextColor), String lfString,
		('    - 1 bit per pixel graphics' color: self redTextColor))
			shrink; color: Color transparent; borderWidth: 0! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 11/4/2025 11:28:25'!
slide4Text2

	^ (TextParagraphMorph contents:
		('1993' pointSize: 24), String lfString centered,
		('Morphic 1.0 / Self 4.0' pointSize: 24) bold, String lfString centered,String lfString,
		('    + Morphs: everything is a live object' color: self greenTextColor), String lfString,
		('    + Fully Composable' color: self greenTextColor), String lfString,
		('    - Limited Graphics quality' color: self redTextColor)) shrink; color: Color transparent; borderWidth: 0! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 10/23/2025 15:28:07'!
slide4Text3

	^ (TextParagraphMorph contents:
		('1997' pointSize: 24), String lfString centered,
		('Morphic 2.0 / Squeak' pointSize: 24) bold, String lfString centered, String lfString,
		('    + Richer UI' color: self greenTextColor), String lfString,
		('    - Global Coordinates' color: self redTextColor), String lfString,
		('    - Integer Coordinates' color: self redTextColor), String lfString,
		('    - Limited Graphics quality' color: self redTextColor)) shrink; color: Color transparent; borderWidth: 0! !

!Smalltalks2025PresentationSlide class methodsFor: 'slide 4 parts' stamp: 'jmv 11/3/2025 10:24:41'!
slide4Text4

	^ (TextParagraphMorph contents:
		('2009' pointSize: 24), String lfString centered,
		('Morphic 3.0 / Cuis' pointSize: 24) bold, String lfString centered, String lfString,
		('    + Arbitrary local Coordinate Systems' color: self greenTextColor), String lfString,
		('    + Floating Point Coordinates' color: self greenTextColor), String lfString,
		('    + Top quality Vector Graphics' color: self greenTextColor), String lfString,
		('    + ZUI - Fully Composable' color: self greenTextColor)) shrink; color: Color transparent; borderWidth: 0! !

!VectorCanvas methodsFor: '*Smalltalks2025-drawing' stamp: 'jmv 11/4/2025 10:23:30'!
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
cellSize := 1.
			srcBox := srcForm boundingBox.
			srcBox := srcBox origin extent: srcBox extent - 1.
			dstBox := currentTransformation externalBoundingRectOf: srcBox.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) floor ].
			dstBox := srcBox translatedBy: aPoint.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !
