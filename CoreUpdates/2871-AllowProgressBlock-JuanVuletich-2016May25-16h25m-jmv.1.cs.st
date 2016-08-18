'From Cuis 4.2 of 25 July 2013 [latest update: #2769] on 25 May 2016 at 4:26:22.55798 pm'!

!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 5/22/2016 23:01'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 title |
	title _ progressTitle isString ifTrue: [ progressTitle ] ifFalse: [progressTitle value: '---'].
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: title) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	title displayOn: textForm at: (width-textWidth+h//2@4).

	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ 0@0 extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + (0@2).
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + (2@2) extent: filledWidth@17).
			filledWidth + 20 > oldFilledWidth2
				ifFalse: [ DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [ Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	"Not pretty at all!!"
	WorldState addDeferredUIMessage: [
		self runningWorld ifNotNil: [ :w |
			w invalidateDisplayRect: outerFrame from: nil ]].
	self resume: result! !

