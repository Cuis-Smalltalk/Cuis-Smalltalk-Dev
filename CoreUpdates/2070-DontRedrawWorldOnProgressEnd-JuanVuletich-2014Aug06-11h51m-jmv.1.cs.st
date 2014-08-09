'From Cuis 4.2 of 25 July 2013 [latest update: #2069] on 6 August 2014 at 11:52:07.786875 am'!

!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 8/6/2014 11:49'!
defaultAction

	| delta textFrame barFrame outerFrame result range width w1 f h textWidth textForm innerBarFrame |
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.
	
	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
			
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
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal 
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [		
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		w1 _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		textForm displayAt: textFrame topLeft.
		Display fillBlack: barFrame.
		Display fillWhite: innerBarFrame.
		Display fillGray: (barFrame topLeft + (2@2) extent: w1@17) ].
	"Not pretty at all!!"
	self runningWorld ifNotNil: [ :w |
		w invalidateDisplayRect: outerFrame from: nil ].
	self resume: result! !

