'From Cuis 6.0 [latest update: #5544] on 31 October 2022 at 11:51:22 am'!
'Description '!
!provides: 'Tests-Compression' 1 2!
!requires: 'Compression' 1 22 nil!
SystemOrganization addCategory: 'Tests-Compression'!


!classDefinition: #ZipCrcTests category: 'Tests-Compression'!
TestCase subclass: #ZipCrcTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Compression'!
!classDefinition: 'ZipCrcTests class' category: 'Tests-Compression'!
ZipCrcTests class
	instanceVariableNames: ''!


!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:47:11'!
testInvalidGZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := GZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes at: bytes size-5. "before the length"
	bytes at: bytes size-5 put: (crcByte + 1 bitAnd: 255).

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:47:14'!
testInvalidZLibCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := ZLibWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes at: bytes size-2.
	bytes at: bytes size-2 put: (crcByte + 1 bitAnd: 255).

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 10:44:31'!
testInvalidZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader contents] raise: CRCError.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc - 1.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 10:43:33'!
testMissingGZipCrc
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := GZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	bytes := bytes copyFrom: 1 to: bytes size-6.

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 10:42:47'!
testMissingZLibCrc
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZLibWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	bytes := bytes copyFrom: 1 to: bytes size-2.

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:05:45'!
testMissingZipCrc
	"See that the lack of a CRC does not raise an error"
	| reader writer bytes readBytes |
	writer := ZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	self shouldnt:[readBytes _ reader upToEnd] raise: CRCError.
	self assert: readBytes = 'Hello World' asUtf8Bytes.

	reader := ZipReadStream on: bytes.
	self shouldnt:[reader contents] raise: CRCError.

	reader := ZipReadStream on: bytes.
	self shouldnt:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:07:05'!
testValidGZipCrc
	| reader writer bytes |
	writer := GZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := GZipReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World' asUtf8Bytes.! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:06:59'!
testValidZLibCrc
	| reader writer bytes |
	writer := ZLibWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World' asUtf8Bytes.
	
	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: (reader next: 100) = 'Hello World' asUtf8Bytes.! !

!ZipCrcTests methodsFor: 'tests' stamp: 'jmv 10/31/2022 11:05:59'!
testValidZipCrc
	"See that a correct CRC does not raise an error and that we can read what we wrote."
	| reader writer bytes readBytes |
	writer := ZipWriteStream on: ByteArray new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader upToEnd] raise: CRCError.
	self assert: readBytes = 'Hello World' asUtf8Bytes.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader contents] raise: CRCError.
	self assert: readBytes = 'Hello World' asUtf8Bytes.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader next: 11 ] raise: CRCError.
	self assert: readBytes = 'Hello World' asUtf8Bytes.
	
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader next: 100 ] raise: CRCError.
	self assert: readBytes = 'Hello World' asUtf8Bytes.! !
