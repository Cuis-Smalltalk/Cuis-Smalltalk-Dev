'From Cuis 6.0 [latest update: #5120] on 29 April 2022 at 12:15:53 pm'!

!BitBlt methodsFor: 'private' stamp: 'jmv 4/26/2022 10:21:11'!
primFaiDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta

	| ascii glyph |
	startIndex to: stopIndex do: [ :charIndex |
		ascii _ (aString at: charIndex) numericValue.
		glyph _ glyphMap at: ascii + 1.
		sourceX _ xTable at: glyph + 1.
		width _ (xTable at: glyph + 2) - sourceX.
		self copyBits.
		destX _ destX + width + kernDelta.
	].! !


!BitBlt methodsFor: 'private' stamp: 'jmv 4/26/2022 10:20:59'!
primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta

	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>
	self primFaiDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta! !

