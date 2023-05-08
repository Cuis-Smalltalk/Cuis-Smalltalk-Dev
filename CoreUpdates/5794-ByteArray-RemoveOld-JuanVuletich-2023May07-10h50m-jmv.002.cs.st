'From Cuis 6.0 [latest update: #5792] on 7 May 2023 at 11:09:35 am'!
!methodRemoval: ByteArray #long64At:bigEndian: stamp: 'jmv 5/7/2023 10:50:31'!
ByteArray removeSelector: #long64At:bigEndian:!
!methodRemoval: ByteArray #doubleAt:bigEndian: stamp: 'jmv 5/7/2023 11:03:51'!
ByteArray removeSelector: #doubleAt:bigEndian:!
!methodRemoval: ByteArray #floatAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:03:53'!
ByteArray removeSelector: #floatAt:put:bigEndian:!
!methodRemoval: ByteArray #unsignedLongAt:bigEndian: stamp: 'jmv 5/7/2023 11:07:50'!
ByteArray removeSelector: #unsignedLongAt:bigEndian:!
!methodRemoval: ByteArray #unsignedLongAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:07:47'!
ByteArray removeSelector: #unsignedLongAt:put:bigEndian:!
!methodRemoval: ByteArray #longAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:08:55'!
ByteArray removeSelector: #longAt:put:bigEndian:!
!methodRemoval: ByteArray #shortAt:bigEndian: stamp: 'jmv 5/7/2023 11:08:56'!
ByteArray removeSelector: #shortAt:bigEndian:!
!methodRemoval: ByteArray #shortAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:08:57'!
ByteArray removeSelector: #shortAt:put:bigEndian:!
!methodRemoval: ByteArray #unsignedShortAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:07:43'!
ByteArray removeSelector: #unsignedShortAt:put:bigEndian:!
!methodRemoval: ByteArray #unsignedShortAt:bigEndian: stamp: 'jmv 5/7/2023 11:07:44'!
ByteArray removeSelector: #unsignedShortAt:bigEndian:!
!methodRemoval: ByteArray #doubleAt:put:bigEndian: stamp: 'jmv 5/7/2023 11:03:52'!
ByteArray removeSelector: #doubleAt:put:bigEndian:!
!methodRemoval: ByteArray #unsignedLong64At:put:bigEndian: stamp: 'jmv 5/7/2023 10:50:08'!
ByteArray removeSelector: #unsignedLong64At:put:bigEndian:!
!methodRemoval: ByteArray #long64At:put:bigEndian: stamp: 'jmv 5/7/2023 10:50:32'!
ByteArray removeSelector: #long64At:put:bigEndian:!
!methodRemoval: ByteArray #unsignedLong64At:bigEndian: stamp: 'jmv 5/7/2023 10:50:06'!
ByteArray removeSelector: #unsignedLong64At:bigEndian:!
!methodRemoval: ByteArray #floatAt:bigEndian: stamp: 'jmv 5/7/2023 11:03:52'!
ByteArray removeSelector: #floatAt:bigEndian:!
!methodRemoval: ByteArray #longAt:bigEndian: stamp: 'jmv 5/7/2023 11:08:54'!
ByteArray removeSelector: #longAt:bigEndian:!

!ByteArray reorganize!
('accessing' atAllPut: bitSize bytesPerElement indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent:)
('access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At: int8At:put: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At: uint8At:put:)
('converting' asByteArray asHex asString asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement replaceFrom:to:with:startingAt:)
('comparing' = hash)
('printing' printOn: storeOn:)
('testing' is: isLiteral)
('initialization' readHexFrom:)
!

