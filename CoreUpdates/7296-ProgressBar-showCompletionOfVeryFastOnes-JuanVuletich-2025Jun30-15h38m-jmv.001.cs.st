'From Cuis7.5 [latest update: #7295] on 30 June 2025 at 3:39:23 pm'!

!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'jmv 6/30/2025 15:37:07'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame lastDrawTime |
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
	textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
	DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
	DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
	DisplayScreen forceDeferredToScreen: outerFrame.
	lastDrawTime := Time localMillisecondClock.
	result := workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal := barVal ]
			ifNil: [
				currentVal := currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal := minVal ]].
		filledWidth := ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		(Time localMillisecondClock - lastDrawTime > 100 or: [
				currentVal = maxVal ]) ifTrue: [
			textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
			DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
			DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
			DisplayScreen deferredUpdatingDisplay fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			DisplayScreen forceDeferredToScreen: outerFrame.
			lastDrawTime := Time localMillisecondClock ]].
	UISupervisor ui ifNotNil: [ :ui | ui invalidateDisplayRect: outerFrame for: nil ].
	self resume: result! !

