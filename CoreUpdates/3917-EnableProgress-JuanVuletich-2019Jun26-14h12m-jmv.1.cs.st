'From Cuis 5.0 of 7 November 2016 [latest update: #3812] on 26 June 2019 at 2:14:05 pm'!

!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 6/26/2019 14:12:51'!
title: argString at: argPoint from: argMinVal to: argMaxVal workBlock: argWorkBlock

	progressTitle _ argString.
	aPoint _ argPoint.
	minVal _ argMinVal.
	maxVal _ argMaxVal.
	currentVal _ minVal.
	workBlock _ argWorkBlock.! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:03:36'!
aPoint
	^aPoint! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:04:37'!
currentVal
	^currentVal! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:05:16'!
currentVal: val
	currentVal _ val! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:03:51'!
maxVal
	^maxVal! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:03:57'!
minVal
	^minVal! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:02:59'!
progressTitle
	^progressTitle! !

!ProgressInitiationException methodsFor: 'instance migration protection' stamp: 'jmv 6/26/2019 14:04:22'!
workBlock
	^workBlock! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 6/26/2019 14:06:27'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 prevContents |
	f _ AbstractFont default.
	h _ f lineSpacing * 3//2.
	textWidth _ (f widthOfString: self progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	self progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame _ Rectangle center: self aPoint extent: width@h.
	textFrame _ `0@0` extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	prevContents _ Form fromDisplay: outerFrame.
	range _ self maxVal = self minVal ifTrue: [1] ifFalse: [self maxVal - self minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ self workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ self currentVal: barVal ]
			ifNil: [
				self currentVal: self currentVal + 1.
				self currentVal >= self maxVal
					ifTrue: [ self currentVal: self minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((self currentVal - self minVal) asFloat / range min: 1.0)) asInteger.
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
					Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	prevContents displayAt: outerFrame topLeft.
	self resume: result! !

!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 6/26/2019 14:13:40'!
display: argString at: argPoint from: argMinVal to: argMaxVal during: argWorkBlock

	self title: argString at: argPoint from: argMinVal to: argMaxVal workBlock: argWorkBlock.
	^self signal! !

