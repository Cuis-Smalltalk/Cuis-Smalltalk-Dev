'From Cuis 4.2 of 25 July 2013 [latest update: #2172] on 27 January 2015 at 4:46:54.769027 pm'!
!methodRemoval: Bitmap #restoreEndianness!
Bitmap removeSelector: #restoreEndianness!
!methodRemoval: Bitmap #writeOn:!
Bitmap removeSelector: #writeOn:!

!Bitmap reorganize!
('printing' printOn:)
('accessing' atAllPut: bitPatternForDepth: byteAt: byteAt:put: byteSize copyFromByteArray2:to: copyFromByteArray: defaultElement integerAt: integerAt:put: pixelValueForDepth: primFill: replaceFrom:to:with:startingAt:)
('testing' isColormap)
('as yet unclassified' asByteArray copy)
!

