'From Cuis 6.0 [latest update: #5527] on 26 October 2022 at 4:07:14 pm'!

!Utf8String methodsFor: 'dispatch' stamp: 'jmv 10/26/2022 16:06:54'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex
		toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!Utf8String methodsFor: 'dispatch' stamp: 'jmv 10/26/2022 16:06:49'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true
! !

