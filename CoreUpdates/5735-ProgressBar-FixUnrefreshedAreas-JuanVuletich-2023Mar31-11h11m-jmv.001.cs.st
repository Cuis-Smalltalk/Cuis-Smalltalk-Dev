'From Cuis 6.0 [latest update: #5734] on 31 March 2023 at 11:11:45 am'!

!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'jmv 3/31/2023 11:11:30'!
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
	barFrame := Rectangle center: aPoint extent: width@h.
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

