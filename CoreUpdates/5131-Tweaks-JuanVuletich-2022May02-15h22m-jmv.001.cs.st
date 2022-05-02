'From Cuis 6.0 [latest update: #5130] on 2 May 2022 at 3:23:08 pm'!

!String methodsFor: 'accessing' stamp: 'jmv 4/29/2022 20:21:29'!
findString: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: body startingAt: start matchTable: CaseSensitiveOrder.! !


!BitBlt methodsFor: 'private' stamp: 'jmv 9/2/2016 11:06:18'!
primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta
	| ascii glyph |
	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>
	startIndex to: stopIndex do:[:charIndex|
		ascii _ (aString at: charIndex) numericValue.
		glyph _ glyphMap at: ascii + 1.
		sourceX _ xTable at: glyph + 1.
		width _ (xTable at: glyph + 2) - sourceX.
		self copyBits.
		destX _ destX + width + kernDelta.
	].! !

!methodRemoval: BitBlt #primFaiDisplayString:from:to:map:xTable:kern: stamp: 'jmv 5/2/2022 15:22:22'!
BitBlt removeSelector: #primFaiDisplayString:from:to:map:xTable:kern:!
