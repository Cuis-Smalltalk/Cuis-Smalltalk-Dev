'From Cuis7.9 [latest update: #8012] on 18 June 2026 at 9:44:46 am'!

!Color methodsFor: 'conversions' stamp: 'jmv 18/Jun/2026, 12:23:52 pm (UTC)'!
closestPixelValue: bitDepth
	"Return the nearest approximation to this color for an bitDepth-bit deep Form."

	^Color 
		colorFromPixelValue: (self bitPatternForDepth: bitDepth) first
		depth: bitDepth! !

!Color methodsFor: 'conversions' stamp: 'jmv 18/Jun/2026, 12:24:22 pm (UTC)'!
closestPixelValue16
	"Return the nearest approximation to this color for an 16-bit deep Form."

	^self closestPixelValue: 16! !

!Color methodsFor: 'conversions' stamp: 'jmv 18/Jun/2026, 12:24:15 pm (UTC)'!
closestPixelValue32
	"Return the nearest approximation to this color for an 32-bit deep Form."

	^self closestPixelValue: 32! !

