'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 5:17:32 pm'!
'Description '!
!provides: 'Tests-Compression' 1 1!
!requires: 'Compression' 1 22 nil!
SystemOrganization addCategory: #'Tests-Compression'!


!classDefinition: #ZipCrcTests category: #'Tests-Compression'!
TestCase subclass: #ZipCrcTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Compression'!
!classDefinition: 'ZipCrcTests class' category: #'Tests-Compression'!
ZipCrcTests class
	instanceVariableNames: ''!


!ZipCrcTests methodsFor: 'tests' stamp: 'ar 2/29/2004 04:43'!
testInvalidGZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-5. "before the length"
	bytes byteAt: bytes size-5 put: (crcByte + 1 bitAnd: 255).

	reader := GZipReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := GZipReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'ar 2/29/2004 04:43'!
testInvalidZLibCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes crcByte |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	crcByte := bytes byteAt: bytes size-2.
	bytes byteAt: bytes size-2 put: (crcByte + 1 bitAnd: 255).

	reader := ZLibReadStream on: bytes.
	self should:[reader upToEnd] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader contents] raise: CRCError.

	reader := ZLibReadStream on: bytes.
	self should:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'nk 3/7/2004 18:37'!
testInvalidZipCrc
	"See that a wrong CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZipWriteStream on: String new.
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

!ZipCrcTests methodsFor: 'tests' stamp: 'ar 2/29/2004 04:42'!
testMissingGZipCrc
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := GZipWriteStream on: String new.
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

!ZipCrcTests methodsFor: 'tests' stamp: 'ar 2/29/2004 04:42'!
testMissingZLibCrc
	"See that the lack of a CRC raises an appropriate error"
	| reader writer bytes |
	writer := ZLibWriteStream on: String new.
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

!ZipCrcTests methodsFor: 'tests' stamp: 'nk 3/7/2004 18:49'!
testMissingZipCrc
	"See that the lack of a CRC does not raise an error"
	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	self shouldnt:[readBytes _ reader upToEnd] raise: CRCError.
	self assert: readBytes = 'Hello World'.

	reader := ZipReadStream on: bytes.
	self shouldnt:[reader contents] raise: CRCError.

	reader := ZipReadStream on: bytes.
	self shouldnt:[reader next: 100] raise: CRCError.
! !

!ZipCrcTests methodsFor: 'tests' stamp: 'ar 2/29/2004 04:42'!
testValidGZipCrc
	| reader writer bytes |
	writer := GZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := GZipReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World'.! !

!ZipCrcTests methodsFor: 'tests' stamp: 'nk 3/7/2004 18:46'!
testValidZLibCrc
	| reader writer bytes |
	writer := ZLibWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: reader upToEnd = 'Hello World'.
	
	bytes := writer encodedStream contents.
	reader := ZLibReadStream on: bytes.
	self assert: (reader next: 100) = 'Hello World'.! !

!ZipCrcTests methodsFor: 'tests' stamp: 'nk 3/7/2004 18:43'!
testValidZipCrc
	"See that a correct CRC does not raise an error and that we can read what we wrote."
	| reader writer bytes readBytes |
	writer := ZipWriteStream on: String new.
	writer nextPutAll: 'Hello World'.
	writer close.

	bytes := writer encodedStream contents.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader upToEnd] raise: CRCError.
	self assert: readBytes = 'Hello World'.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader contents] raise: CRCError.
	self assert: readBytes = 'Hello World'.

	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader next: 11 ] raise: CRCError.
	self assert: readBytes = 'Hello World'.
	
	reader := ZipReadStream on: bytes.
	reader expectedCrc: writer crc.
	self shouldnt:[ readBytes _ reader next: 100 ] raise: CRCError.
	self assert: readBytes = 'Hello World'.! !
