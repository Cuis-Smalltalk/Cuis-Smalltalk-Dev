'From Cuis6.3 [latest update: #6248] on 4 March 2024 at 3:16:12 pm'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 3/4/2024 15:07:09'!
roundedToEven
	"Convenience"
	^self roundTo: 2! !


!Point methodsFor: 'truncation and round off' stamp: 'jmv 3/4/2024 15:07:26'!
roundedToEven
	^ x roundedToEven @ y roundedToEven
! !


!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'jmv 3/4/2024 15:09:17'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 |
	f := FontFamily defaultFamilyAndPointSize.
	h := f lineSpacing * 3//2.
	textWidth := (f widthOfString: progressTitle) + h.
	width := 150 max: textWidth.

	textForm := Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame := Rectangle center: aPoint extent: (width@h) roundedToEven.
	textFrame := `0@0` extent: width@h.
	textFrame := textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame := barFrame merge: textFrame.
	delta := outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame := barFrame translatedBy: delta.
	textFrame := textFrame translatedBy: delta.
	outerFrame := outerFrame translatedBy: delta.
	range := maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame := barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth := 0.
	oldFilledWidth2 := 0.
	result := workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal := barVal ]
			ifNil: [
				currentVal := currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal := minVal ]].
		filledWidth := ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			filledWidth -200 > oldFilledWidth2
				ifFalse: [
					"Usually just request an update, to be done asynchronously."
					DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [
					"Once in a while, force a real screen update (warning: really slow on MacOS if done too often)"
					Display forceToScreen: outerFrame. oldFilledWidth2 := filledWidth ].
			oldFilledWidth := filledWidth ]].
	UISupervisor ui ifNotNil: [ :ui | ui invalidateDisplayRect: outerFrame for: nil ].
	self resume: result! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 3/4/2024 15:08:45'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb := BitBltCanvasEngine toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect := aPoint extent: diameter.
	bb fillOval: rect.
	p := (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: `Color white alpha: 0.3`.
		d := i * diameter // 10.
		rect := Rectangle
			center: p
			extent: d roundedToEven.
		bb fillOval: rect ].
	DisplayScreen screenUpdateRequired: nil.
"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !


!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 3/4/2024 15:10:25'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect := Rectangle center: aPoint extent: aMorph morphExtent.
	delta := trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: (trialRect origin + delta) rounded.! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/4/2024 15:12:33'!
imageUnrotated: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	Scale and position like #image:at:, but don't rotate srcForm:
	Display it in its natural orientation."

	| cellSize srcBox dstBox srcQuad bb |

	"If no rotation or scaling, BitBlt is enough. But there's no need to repeat the code:
	If no rotation, 'unrotated' is the same as 'defaultlyRotated'."
	currentTransformation isPureTranslation ifTrue: [
		^self image: srcForm at: aPoint ].

	"Compute bounds, set morphIds for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.

	cellSize := 4.
	srcBox := srcForm boundingBox.
	srcQuad := srcBox corners.
	dstBox := Rectangle
		center: (currentTransformation transform: srcBox center + aPoint) roundedHAFZ
		extent: (srcBox extent * currentTransformation scale) roundedHAFZ roundedToEven.
	bb := WarpBlt toForm: form.
	bb
		sourceForm: srcForm; combinationRule: Form blend;
		colorMap: (srcForm colormapIfNeededFor: form);
		clipRect: clipRect;
		cellSize: cellSize;
		copyQuad: srcQuad toRect: dstBox.
! !

