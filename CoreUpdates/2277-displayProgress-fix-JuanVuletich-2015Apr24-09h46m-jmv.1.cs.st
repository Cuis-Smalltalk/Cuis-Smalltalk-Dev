'From Cuis 4.2 of 25 July 2013 [latest update: #2276] on 24 April 2015 at 10:13:12.689589 am'!

!String methodsFor: 'displaying' stamp: 'jmv 4/24/2015 09:49'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:bar |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			bar value: x.]].

['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:bar |
	1 to: 100 do: [:x | bar value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:bar |
	1 to: 30 do: [:x | bar value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:bar |
	1 to: 30 do: [:x | bar value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 4/24/2015 10:11'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFullDrawnWidth |
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
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFullDrawnWidth _ nil.
	oldFilledWidth _ nil.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal 
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [		
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		(oldFilledWidth isNil or: [ filledWidth  > (oldFilledWidth+5) ]) ifTrue: [
			(oldFullDrawnWidth isNil or: [ filledWidth  > (oldFullDrawnWidth+20) ]) ifTrue: [
				textForm displayAt: textFrame topLeft.
				Display fillBlack: barFrame.
				Display fillWhite: innerBarFrame.
				oldFullDrawnWidth _ filledWidth ].
			Display fillGray: (barFrame topLeft + (2@2) extent: filledWidth@17).
			oldFilledWidth _ filledWidth ]].
	"Not pretty at all!!"
	WorldState addDeferredUIMessage: [
		self runningWorld ifNotNil: [ :w |
			w invalidateDisplayRect: outerFrame from: nil ]].
	self resume: result! !

