'From Cuis6.3 [latest update: #6294] on 25 March 2024 at 12:05:03 pm'!

!TextBackgroundColor class methodsFor: 'constants' stamp: 'jmv 3/25/2024 11:42:56'!
gray
	^ self new color: `Color gray`! !


!TextColor class methodsFor: 'constants' stamp: 'jmv 3/25/2024 11:43:18'!
gray
	^ self new color: `Color gray`! !


!Form methodsFor: 'coloring' stamp: 'jmv 3/25/2024 11:43:23'!
fillGray
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: `Color gray`! !

!Form methodsFor: 'coloring' stamp: 'jmv 3/25/2024 11:43:26'!
fillGray: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: `Color gray`! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 3/25/2024 11:43:34'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			Display border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			rect := newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!BoxedMorph methodsFor: 'initialization' stamp: 'jmv 3/25/2024 11:43:51'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color gray`! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 3/25/2024 11:43:54'!
defaultColor
	^ `Color lightGray`! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 3/25/2024 11:49:18'!
defaultColor
	^ `Color veryLightGray`! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 3/25/2024 11:47:35'!
setDismissColor: event with: aHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	event hand obtainHalo: self.
	colorToUse :=  (aHandle includesPixel: event eventPosition)
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	aHandle color: colorToUse! !


!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 3/25/2024 11:48:06'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color lightGray`! !


!ProgressBarMorph methodsFor: 'initialization' stamp: 'jmv 3/25/2024 11:47:02'!
initialize
	super initialize.
	progressColor := `Color gray`.
	value := 0.0! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/25/2024 11:50:07'!
drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height := extent y - (2 * scrollbarThickness).
	top := (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom := (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray`! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/25/2024 11:45:24'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need two of them!!"
	(world notNil and: [currentMorph == world]) ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: 0; sourceY: 0;
			destX: p x destY: p y width: srcForm width height: srcForm height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: 0; sourceY: 0;
				destX: p x destY: p y width: srcForm width height: srcForm height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := srcForm boundingBox.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/25/2024 11:45:20'!
image: srcForm at: aPoint sourceRect: sourceRect
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need two of them!!"
	currentMorph == world ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: sourceRect left; sourceY: sourceRect top;
			destX: p x destY: p y width: sourceRect width height: sourceRect height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (sourceRect translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: sourceRect left; sourceY: sourceRect top;
				destX: p x destY: p y width: sourceRect width height: sourceRect height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := sourceRect.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !


!Theme methodsFor: 'colors' stamp: 'pb 4/24/2019 20:46:25'!
textEmptyDisplayMessage
	^ `Color veryLightGray`! !

!Theme methodsFor: 'colors' stamp: 'jmv 3/25/2024 11:52:02'!
textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ `Color veryLightGray` ].
	Display depth = 2 ifTrue: [^ `Color gray: 0.87` ].
	textHighlight := self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]! !

!Theme methodsFor: 'menu colors' stamp: 'jmv 3/25/2024 11:50:23'!
menuHighlight
	^ Display depth < 8
		ifTrue: [ `Color veryLightGray` ]
		ifFalse: [ self textHighlight ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 3/25/2024 11:44:56'!
autoCompleterDefaultBorderColor
	"Answer the auto comleters default border color."

	^ `Color gray`! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 3/25/2024 11:48:26'!
defaultWindowColor
	^ `Color lightGray`! !

!Theme methodsFor: 'shout' stamp: 'jmv 3/25/2024 11:42:26'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  					-> nil.
		#arguments 					-> #(cyan muchDarker).
		#comment 					-> #(green muchDarker).
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#pseudoVariables 					-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 					-> #(gray veryMuchDarker).
		#blockLevelZero 					-> #black.
		#blockLevelOne 					-> #brown.
		#blockLevelTwo 					-> #magenta.
		#blockLevelThree 					-> #red.
		#blockLevelFour 					-> #(orange darker).
		#blockLevelFive 					-> #(orange muchDarker).
		#blockLevelSix 					-> #(green muchDarker).
		#blockLevelSeven 					-> #blue.
		#defaults 					-> #black.
		#undefined 					-> #red.
		#methodTags 					-> #(green muchDarker).
	}! !


!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/25/2024 11:42:30'!
shout
	"Color symbols as an association list."

	^ {
		#selector  					-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 					-> #(green duller).
		#tempBar 					-> #gray.
		#tempVars 					-> '#767836'.
		#instVar 					-> '#B3695A'.
		#pseudoVariables 					-> '#2070E0'.
		#literals 					-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  					-> '#F08060'.
		#blockLevelZero 					-> '#6FB3BD'.
		#blockLevelOne 					-> '#FFB0B0'.
		#blockLevelTwo 					-> '#B0FFB0'.
		#blockLevelThree 					-> '#B0B0FF'.
		#blockLevelFour 					-> '#00B0B0'.
		#blockLevelFive 					-> '#E03030'.
		#blockLevelSix 					-> '#30E030'.
		#blockLevelSeven 					-> '#3030E0'.
		#defaults 					-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 					-> #green.
	}! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/25/2024 11:52:07'!
text
	^ `Color veryLightGray`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/25/2024 11:42:33'!
textPane
	^ `Color gray`! !

!DarkTheme methodsFor: 'menu colors' stamp: 'jmv 3/25/2024 11:32:47'!
menu
	^ `Color darkGray`! !

