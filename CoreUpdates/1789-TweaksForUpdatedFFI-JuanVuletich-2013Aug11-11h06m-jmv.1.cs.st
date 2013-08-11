'From Cuis 4.2 of 25 July 2013 [latest update: #1788] on 11 August 2013 at 11:09:10.435986 am'!

!SystemDictionary methodsFor: 'special objects' stamp: 'eem 10/29/2009 10:52'!
primitiveErrorTable
	"Smalltalk primitiveErrorTable"
	^self specialObjectsArray at: 52! !

!methodRemoval: ByteArray #booleanAt:!
ByteArray removeSelector: #booleanAt:!
!methodRemoval: ByteArray #booleanAt:put:!
ByteArray removeSelector: #booleanAt:put:!
!methodRemoval: ByteArray #doubleAt:!
ByteArray removeSelector: #doubleAt:!
!methodRemoval: ByteArray #doubleAt:put:!
ByteArray removeSelector: #doubleAt:put:!
!methodRemoval: ByteArray #floatAt:!
ByteArray removeSelector: #floatAt:!
!methodRemoval: ByteArray #floatAt:put:!
ByteArray removeSelector: #floatAt:put:!
!methodRemoval: ByteArray #integerAt:put:size:signed:!
ByteArray removeSelector: #integerAt:put:size:signed:!
!methodRemoval: ByteArray #integerAt:size:signed:!
ByteArray removeSelector: #integerAt:size:signed:!
!methodRemoval: ByteArray #isExternalAddress!
ByteArray removeSelector: #isExternalAddress!
!methodRemoval: ByteArray #pointerAt:put:!
ByteArray removeSelector: #pointerAt:put:!
!methodRemoval: ByteArray #signedByteAt:!
ByteArray removeSelector: #signedByteAt:!
!methodRemoval: ByteArray #signedByteAt:put:!
ByteArray removeSelector: #signedByteAt:put:!
!methodRemoval: ByteArray #signedCharAt:!
ByteArray removeSelector: #signedCharAt:!
!methodRemoval: ByteArray #signedCharAt:put:!
ByteArray removeSelector: #signedCharAt:put:!
!methodRemoval: ByteArray #signedLongAt:!
ByteArray removeSelector: #signedLongAt:!
!methodRemoval: ByteArray #signedLongAt:put:!
ByteArray removeSelector: #signedLongAt:put:!
!methodRemoval: ByteArray #signedLongLongAt:!
ByteArray removeSelector: #signedLongLongAt:!
!methodRemoval: ByteArray #signedLongLongAt:put:!
ByteArray removeSelector: #signedLongLongAt:put:!
!methodRemoval: ByteArray #signedShortAt:!
ByteArray removeSelector: #signedShortAt:!
!methodRemoval: ByteArray #signedShortAt:put:!
ByteArray removeSelector: #signedShortAt:put:!
!methodRemoval: ByteArray #structAt:length:!
ByteArray removeSelector: #structAt:length:!
!methodRemoval: ByteArray #structAt:put:length:!
ByteArray removeSelector: #structAt:put:length:!
!methodRemoval: ByteArray #unsignedByteAt:!
ByteArray removeSelector: #unsignedByteAt:!
!methodRemoval: ByteArray #unsignedByteAt:put:!
ByteArray removeSelector: #unsignedByteAt:put:!
!methodRemoval: ByteArray #unsignedCharAt:!
ByteArray removeSelector: #unsignedCharAt:!
!methodRemoval: ByteArray #unsignedCharAt:put:!
ByteArray removeSelector: #unsignedCharAt:put:!
!methodRemoval: ByteArray #unsignedLongAt:!
ByteArray removeSelector: #unsignedLongAt:!
!methodRemoval: ByteArray #unsignedLongAt:put:!
ByteArray removeSelector: #unsignedLongAt:put:!
!methodRemoval: ByteArray #unsignedLongLongAt:!
ByteArray removeSelector: #unsignedLongLongAt:!
!methodRemoval: ByteArray #unsignedLongLongAt:put:!
ByteArray removeSelector: #unsignedLongLongAt:put:!
!methodRemoval: ByteArray #unsignedShortAt:!
ByteArray removeSelector: #unsignedShortAt:!
!methodRemoval: ByteArray #unsignedShortAt:put:!
ByteArray removeSelector: #unsignedShortAt:put:!
!methodRemoval: ByteArray #voidAt:!
ByteArray removeSelector: #voidAt:!
!methodRemoval: ByteArray #voidAt:put:!
ByteArray removeSelector: #voidAt:put:!

!ByteArray reorganize!
('accessing' atAllPut: byteAt: byteAt:put: byteSize bytesPerElement indexOf:startingAt:)
('platform independent access' doubleAt:bigEndian: doubleAt:put:bigEndian: longAt:bigEndian: longAt:put:bigEndian: shortAt:bigEndian: shortAt:put:bigEndian: unsignedLongAt:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:bigEndian: unsignedShortAt:put:bigEndian:)
('converting' asByteArray asString base64Encoded hex)
('private' asByteArrayPointer defaultElement replaceFrom:to:with:startingAt:)
('comparing' hash)
('initialize' readHexFrom:)
('printing' printOn: storeOn:)
('testing' isLiteral)
!

