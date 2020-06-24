'From Cuis 5.0 [latest update: #4236] on 24 June 2020 at 4:08:04 pm'!
'Description '!
!provides: 'Compression' 1 27!
SystemOrganization addCategory: #'Compression-System'!
SystemOrganization addCategory: #'Compression-Archives'!


!classDefinition: #GZipConstants category: #'Compression-System'!
SharedPool subclass: #GZipConstants
	instanceVariableNames: ''
	classVariableNames: 'GZipAsciiFlag GZipCommentFlag GZipContinueFlag GZipDeflated GZipEncryptFlag GZipExtraField GZipMagic GZipNameFlag GZipReservedFlags'
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'GZipConstants class' category: #'Compression-System'!
GZipConstants class
	instanceVariableNames: ''!

!classDefinition: #ZipConstants category: #'Compression-System'!
SharedPool subclass: #ZipConstants
	instanceVariableNames: ''
	classVariableNames: 'BaseDistance BaseLength BitLengthOrder DistanceCodes DynamicBlock EndBlock ExtraBitLengthBits ExtraDistanceBits ExtraLengthBits FixedBlock FixedDistanceTree FixedLiteralTree HashBits HashMask HashShift MatchLengthCodes MaxBitLengthBits MaxBitLengthCodes MaxBits MaxDistCodes MaxDistance MaxLengthCodes MaxLiteralCodes MaxMatch MinMatch NumLiterals Repeat11To138 Repeat3To10 Repeat3To6 StoredBlock WindowMask WindowSize'
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZipConstants class' category: #'Compression-System'!
ZipConstants class
	instanceVariableNames: ''!

!classDefinition: #ZipFileConstants category: #'Compression-Archives'!
SharedPool subclass: #ZipFileConstants
	instanceVariableNames: ''
	classVariableNames: 'CentralDirectoryFileHeaderSignature CompressionDeflated CompressionLevelDefault CompressionLevelNone CompressionStored DataDescriptorLength DefaultDirectoryPermissions DefaultFilePermissions DeflatingCompressionFast DeflatingCompressionMaximum DeflatingCompressionNormal DeflatingCompressionSuperFast DirectoryAttrib EndOfCentralDirectorySignature FaMsdos FaUnix FileAttrib IfaBinaryFile IfaTextFile LocalFileHeaderSignature'
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ZipFileConstants class' category: #'Compression-Archives'!
ZipFileConstants class
	instanceVariableNames: ''!

!classDefinition: #InflateStream category: #'Compression-System'!
ReadStream subclass: #InflateStream
	instanceVariableNames: 'state bitBuf bitPos source sourcePos sourceLimit litTable distTable sourceStream crc'
	classVariableNames: 'BlockProceedBit BlockTypes FixedDistCodes FixedLitCodes MaxBits StateNewBlock StateNoMoreData'
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'InflateStream class' category: #'Compression-System'!
InflateStream class
	instanceVariableNames: ''!

!classDefinition: #FastInflateStream category: #'Compression-System'!
InflateStream subclass: #FastInflateStream
	instanceVariableNames: ''
	classVariableNames: 'DistanceMap FixedDistTable FixedLitTable LiteralLengthMap'
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'FastInflateStream class' category: #'Compression-System'!
FastInflateStream class
	instanceVariableNames: ''!

!classDefinition: #GZipReadStream category: #'Compression-System'!
FastInflateStream subclass: #GZipReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GZipConstants'
	category: 'Compression-System'!
!classDefinition: 'GZipReadStream class' category: #'Compression-System'!
GZipReadStream class
	instanceVariableNames: ''!

!classDefinition: #ZLibReadStream category: #'Compression-System'!
FastInflateStream subclass: #ZLibReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZLibReadStream class' category: #'Compression-System'!
ZLibReadStream class
	instanceVariableNames: ''!

!classDefinition: #ZipReadStream category: #'Compression-System'!
FastInflateStream subclass: #ZipReadStream
	instanceVariableNames: 'expectedCrc'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZipReadStream class' category: #'Compression-System'!
ZipReadStream class
	instanceVariableNames: ''!

!classDefinition: #DeflateStream category: #'Compression-System'!
WriteStream subclass: #DeflateStream
	instanceVariableNames: 'hashHead hashTail hashValue blockPosition blockStart'
	classVariableNames: ''
	poolDictionaries: 'ZipConstants'
	category: 'Compression-System'!
!classDefinition: 'DeflateStream class' category: #'Compression-System'!
DeflateStream class
	instanceVariableNames: ''!

!classDefinition: #ZipWriteStream category: #'Compression-System'!
DeflateStream subclass: #ZipWriteStream
	instanceVariableNames: 'literals distances literalFreq distanceFreq litCount matchCount encoder crc crcPosition bytesWritten'
	classVariableNames: 'CrcTable VerboseLevel'
	poolDictionaries: 'ZipConstants'
	category: 'Compression-System'!
!classDefinition: 'ZipWriteStream class' category: #'Compression-System'!
ZipWriteStream class
	instanceVariableNames: ''!

!classDefinition: #GZipWriteStream category: #'Compression-System'!
ZipWriteStream subclass: #GZipWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GZipConstants'
	category: 'Compression-System'!
!classDefinition: 'GZipWriteStream class' category: #'Compression-System'!
GZipWriteStream class
	instanceVariableNames: ''!

!classDefinition: #ZLibWriteStream category: #'Compression-System'!
ZipWriteStream subclass: #ZLibWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZLibWriteStream class' category: #'Compression-System'!
ZLibWriteStream class
	instanceVariableNames: ''!

!classDefinition: #ZipEncoder category: #'Compression-System'!
WriteStream subclass: #ZipEncoder
	instanceVariableNames: 'bitBuffer bitPosition encodedStream'
	classVariableNames: ''
	poolDictionaries: 'ZipConstants'
	category: 'Compression-System'!
!classDefinition: 'ZipEncoder class' category: #'Compression-System'!
ZipEncoder class
	instanceVariableNames: ''!

!classDefinition: #CRCError category: #'Compression-System'!
Error subclass: #CRCError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'CRCError class' category: #'Compression-System'!
CRCError class
	instanceVariableNames: ''!

!classDefinition: #GZipSurrogateStream category: #'Compression-System'!
Object subclass: #GZipSurrogateStream
	instanceVariableNames: 'gZipStream zippedFileStream bufferStream positionThusFar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'GZipSurrogateStream class' category: #'Compression-System'!
GZipSurrogateStream class
	instanceVariableNames: ''!

!classDefinition: #ZipEncoderNode category: #'Compression-System'!
Object subclass: #ZipEncoderNode
	instanceVariableNames: 'value frequency height bitLength code parent left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZipEncoderNode class' category: #'Compression-System'!
ZipEncoderNode class
	instanceVariableNames: ''!

!classDefinition: #ZipEncoderTree category: #'Compression-System'!
Object subclass: #ZipEncoderTree
	instanceVariableNames: 'bitLengths codes maxCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-System'!
!classDefinition: 'ZipEncoderTree class' category: #'Compression-System'!
ZipEncoderTree class
	instanceVariableNames: ''!

!classDefinition: #Archive category: #'Compression-Archives'!
Object subclass: #Archive
	instanceVariableNames: 'members'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'Archive class' category: #'Compression-Archives'!
Archive class
	instanceVariableNames: ''!

!classDefinition: #ZipArchive category: #'Compression-Archives'!
Archive subclass: #ZipArchive
	instanceVariableNames: 'centralDirectorySize centralDirectoryOffsetWRTStartingDiskNumber zipFileComment writeCentralDirectoryOffset writeEOCDOffset'
	classVariableNames: ''
	poolDictionaries: 'ZipFileConstants'
	category: 'Compression-Archives'!
!classDefinition: 'ZipArchive class' category: #'Compression-Archives'!
ZipArchive class
	instanceVariableNames: ''!

!classDefinition: #ArchiveMember category: #'Compression-Archives'!
Object subclass: #ArchiveMember
	instanceVariableNames: 'fileName isCorrupt'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ArchiveMember class' category: #'Compression-Archives'!
ArchiveMember class
	instanceVariableNames: ''!

!classDefinition: #ZipArchiveMember category: #'Compression-Archives'!
ArchiveMember subclass: #ZipArchiveMember
	instanceVariableNames: 'lastModFileDateTime fileAttributeFormat versionMadeBy versionNeededToExtract bitFlag compressionMethod desiredCompressionMethod desiredCompressionLevel internalFileAttributes externalFileAttributes cdExtraField localExtraField fileComment crc32 compressedSize uncompressedSize writeLocalHeaderRelativeOffset readDataRemaining'
	classVariableNames: ''
	poolDictionaries: 'ZipFileConstants'
	category: 'Compression-Archives'!
!classDefinition: 'ZipArchiveMember class' category: #'Compression-Archives'!
ZipArchiveMember class
	instanceVariableNames: ''!

!classDefinition: #ZipFileMember category: #'Compression-Archives'!
ZipArchiveMember subclass: #ZipFileMember
	instanceVariableNames: 'externalFileName stream localHeaderRelativeOffset dataOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ZipFileMember class' category: #'Compression-Archives'!
ZipFileMember class
	instanceVariableNames: ''!

!classDefinition: #ZipDirectoryMember category: #'Compression-Archives'!
ZipFileMember subclass: #ZipDirectoryMember
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ZipDirectoryMember class' category: #'Compression-Archives'!
ZipDirectoryMember class
	instanceVariableNames: ''!

!classDefinition: #ZipNewFileMember category: #'Compression-Archives'!
ZipArchiveMember subclass: #ZipNewFileMember
	instanceVariableNames: 'externalFileName stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ZipNewFileMember class' category: #'Compression-Archives'!
ZipNewFileMember class
	instanceVariableNames: ''!

!classDefinition: #ZipStringMember category: #'Compression-Archives'!
ZipArchiveMember subclass: #ZipStringMember
	instanceVariableNames: 'contents stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compression-Archives'!
!classDefinition: 'ZipStringMember class' category: #'Compression-Archives'!
ZipStringMember class
	instanceVariableNames: ''!


!InflateStream commentStamp: '<historical>' prior: 0!
This class implements the Inflate decompression algorithm as defined by RFC1951 and used in PKZip, GZip and ZLib (and many, many more). It is a variant of the LZ77 compression algorithm described in

[LZ77] Ziv J., Lempel A., "A Universal Algorithm for Sequential Data Compression", IEEE Transactions on Information Theory", Vol. 23, No. 3, pp. 337-343.

[RFC1951] Deutsch. P, "DEFLATE Compressed Data Format Specification version 1.3"

For more information see the above mentioned RFC 1951 which can for instance be found at

	http://www.leo.org/pub/comp/doc/standards/rfc/index.html

Huffman Tree Implementation Notes:
===========================================
The huffman tree used for decoding literal, distance and length codes in the inflate algorithm has been encoded in a single Array. The tree is made up of subsequent tables storing all entries at the current bit depth. Each entry in the table (e.g., a 32bit Integer value) is either a leaf or a non-leaf node. Leaf nodes store the immediate value in its low 16 bits whereas non-leaf nodes store the offset of the subtable in its low 16bits. The high 8 bits of non-leaf nodes contain the number of additional bits needed for the sub table (the high 8 bits of leaf-nodes are always zero). The first entry in each table is always a non-leaf node indicating how many bits we need to fetch initially. We can thus travel down the tree as follows (written in sort-of-pseudocode the actual implementation can be seen in InflateStream>>decodeValueFrom:):

	table _ initialTable.
	bitsNeeded _ high 8 bits of (table at: 1).		"Determine initial bits"
	table _ initialTable + (low 16 bits of (table at: 1)). "Determine start of first real table"
	[bits _ fetch next bitsNeeded bits.			"Grab the bits"
	value _ table at: bits.						"Lookup the value"
	value has high 8 bit set] whileTrue:[		"Check if it's leaf"
		table _ initialTable + (low 16 bits of value).	"No - compute new sub table start"
		bitsNeeded _ high 8 bit of value].		"Compute additional number of bits needed"
	^value
!

!FastInflateStream commentStamp: '<historical>' prior: 0!
This class adds the following optimizations to the basic Inflate decompression:

a) Bit reversed access
If we want to fetch the bits efficiently then we have them in the wrong bit order (e.g., when we should fetch 2r100 we would get 2r001). But since the huffman tree lookup determines the efficiency of the decompression, reversing the bits before traversal is expensive. Therefore the entries in each table are stored in REVERSE BIT ORDER. This is achieved by a reverse increment of the current table index in the huffman table construction phase (see method increment:bits:). According to my measures this speeds up the implementation by about 30-40%.

b) Inplace storage of code meanings and extra bits
Rather than looking up the meaning for each code during decompression of blocks we store the appropriate values directly in the huffman tables, using a pre-defined mapping. Even though this does not make a big difference in speed, it cleans up the code and allows easier translation into primitive code (which is clearly one goal of this implementation).

c) Precomputed huffman tables for fixed blocks
So we don't have to compute the huffman tables from scratch. The precomputed tables are not in our superclass to avoid double storage (and my superclass is more intended for documentation anyways).!

!ZipReadStream commentStamp: 'nk 3/7/2004 18:54' prior: 0!
ZipReadStream is intended for uncompressing the compressed contents of Zip archive members.

Since Zip archive members keep their expected CRC value separately in Zip headers, this class does not attempt to read the CRC from its input stream.

Instead, if you want the CRC verification to work you have to call #expectedCrc: with the expected CRC-32 value from the Zip member header.!

!GZipSurrogateStream commentStamp: 'jmv 6/17/2015 11:47' prior: 0!
A pseudo stream that allows SmartRefStream to write directly to a gzipped file. There are some peculiarities of the project exporting process that require:

1. We ignore #close since the file is closed and may be reopened to continue writing. We implement #reallyClose for when we know that all writing is over.

2. We use a BitBlt to write WordArrayForSegment objects. Bit of a hack, but there it is.

| fileStream wa |

wa _ WordArrayForSegment new: 30000.
1 to: wa size do: [ :i | wa at: i put: i].
fileStream _ GZipSurrogateStream newFileNamed: 'xxx3.gz' inDirectory: FileDirectory smalltalkImageDirectory.
fileStream nextPutAll: 'this is a test'.
fileStream nextPutAll: wa.
fileStream reallyClose.
!

!ZipEncoderNode commentStamp: '<historical>' prior: 0!
ZipEncoderNode represents a node in a huffman tree for encoding ZipStreams.

Instance variables:
	value 		<Integer>	- Encoded value
	frequency	<Integer>	- Number of occurences of the encoded value
	height 		<Integer>	- Height of the node in the tree
	bitLength 	<Integer>	- bit length of the code
	code		<Integer>	- Assigned code for this node
	parent		<ZipEncoderNode>		- Parent of this node
	left			<ZipEncoderNode>		- First child of this node
	right		<ZipEncoderNode>		- Second child of this node
!

!ZipEncoderTree commentStamp: '<historical>' prior: 0!
ZipEncoderTree represents a huffman tree for encoding ZipStreams.

Instance variables:
	bitLengths	<WordArray>	 - Bit lengths of each generated code
	codes		<WordArray>	 - Codes for each value
	maxCode		<Integer>	- Maximum value with non-zero frequency!

!Archive commentStamp: '<historical>' prior: 0!
This is the abstract superclass for file archives. Archives can be read from or written to files, and contain members that represent files and directories.!

!ZipArchive commentStamp: '<historical>' prior: 0!
A ZipArchive represents an archive that is read and/or written using the PKZIP file format.

ZipArchive instances know how to read and write such archives; their members are subinstances of ZipArchiveMember.!

!ArchiveMember commentStamp: '<historical>' prior: 0!
This is the abstract superclass for archive members, which are files or directories stored in archives.!

!ZipArchiveMember commentStamp: '<historical>' prior: 0!
Subinstances of me are members in a ZipArchive.
They represent different data sources:
	* ZipDirectoryMember -- a directory to be added to a zip file
	* ZipFileMember -- a file or directory that is already in a zip file
	* ZipNewFilemember -- a file that is to be added to a zip file
	* ZipStringMember -- a string that is to be added to a zip file

They can write their data to another stream either copying, compressing,
or decompressing as desired.!

!ZipFileMember commentStamp: '<historical>' prior: 0!
ZipNewFileMember instances are used to represent files that have been read from a ZipArchive.
Their data stays in the file on disk, so the original Zip file cannot be directly overwritten.!

!ZipDirectoryMember commentStamp: '<historical>' prior: 0!
ZipFileMember instances represent directories inside ZipArchives.
They don't do much other than hold names and permissions (and extra fields).

You can add files in subdirectories to a ZipArchive without using any ZipDirectoryMembers.!

!ZipNewFileMember commentStamp: '<historical>' prior: 0!
ZipNewFileMember instances are used to represent files that are going to be written to a ZipArchive.
Their data comes from external file streams.!

!ZipStringMember commentStamp: '<historical>' prior: 0!
ZipStringMember instances are used to represent files that are going to be written to a ZipArchive.
Their data comes from in-image strings, though.!

!ZipEncoderNode methodsFor: 'printing' stamp: 'ar 12/26/1999 10:46'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut:$(;
		nextPutAll:'value = '; print: value;
		nextPutAll:', freq = '; print: frequency;
		nextPutAll:', bitLength = '; print: bitLength;
		nextPutAll:', code = '; print: code;
		nextPutAll:', height = '; print: height; 
	nextPut:$).! !

!ArchiveMember methodsFor: 'printing' stamp: 'nk 12/20/2002 15:11'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(;
		nextPutAll: self fileName;
		nextPut: $)! !

!GZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:00'!
gzipMagic
	^GZipMagic! !

!GZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:00'!
initialize
	"GZipConstants initialize"
	GZipMagic := 16r8B1F.		"GZIP magic number"
	GZipDeflated := 8.			"Compression method"

	GZipAsciiFlag := 16r01.		"Contents is ASCII"
	GZipContinueFlag := 16r02.	"Part of a multi-part archive"
	GZipExtraField := 16r04.		"Archive has extra fields"
	GZipNameFlag := 16r08.		"Archive has original file name"
	GZipCommentFlag := 16r10.	"Archive has comment"
	GZipEncryptFlag := 16r20.	"Archive is encrypted"
	GZipReservedFlags := 16rC0.	"Reserved" ! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:09'!
initialize
	"ZipConstants initialize"
	self initializeDeflateConstants.
	self initializeWriteStreamConstants.! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:06'!
initializeDeflateConstants

	WindowSize _ 16r8000.
	WindowMask _ WindowSize - 1.
	MaxDistance _ WindowSize.

	MinMatch _ 3.
	MaxMatch _ 258.

	HashBits _ 15.
	HashMask _ (1 << HashBits) - 1.
	HashShift _ (HashBits + MinMatch - 1) // MinMatch.
! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:08'!
initializeDistanceCodes
	| dist |
	BaseDistance _ WordArray new: MaxDistCodes.
	DistanceCodes _ WordArray new: 512.
	dist _ 0.
	1 to: 16 do:[:code|
		BaseDistance at: code put: dist.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)) do:[:n|
			dist _ dist + 1.
			DistanceCodes at: dist put: code-1]].
	dist = 256 ifFalse:[self error:'Whoops?!!'].
	dist _ dist >> 7.
	17 to: MaxDistCodes do:[:code|
		BaseDistance at: code put: dist << 7.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)-7) do:[:n|
			dist _ dist + 1.
			DistanceCodes at: 256 + dist put: code-1]].
! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:07'!
initializeExtraBits
	ExtraLengthBits _ 
		WordArray withAll: #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0).
	ExtraDistanceBits _ 
		WordArray withAll: #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	ExtraBitLengthBits _ 
		WordArray withAll: #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 7).
	BitLengthOrder _
		WordArray withAll: #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15).
! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:08'!
initializeFixedTrees
	"ZipWriteStream initializeFixedTrees"
	| counts nodes |
	FixedLiteralTree _ ZipEncoderTree new.
	FixedLiteralTree maxCode: 287.
	counts _ WordArray new: MaxBits+1.
	counts at: 7+1 put: 24.
	counts at: 8+1 put: 144+8.
	counts at: 9+1 put: 112.
	nodes _ Array new: 288.
	1 to: 288 do:[:i| nodes at: i put: (ZipEncoderNode value: i-1 frequency: 0 height: 0)].
	0 to: 143 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	144 to: 255 do:[:i| (nodes at: i+1) setBitLengthTo: 9].
	256 to: 279 do:[:i| (nodes at: i+1) setBitLengthTo: 7].
	280 to: 287 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	FixedLiteralTree buildCodes: nodes counts: counts maxDepth: MaxBits.
	FixedLiteralTree setValuesFrom: nodes.

	FixedDistanceTree _ ZipEncoderTree new.
	FixedDistanceTree maxCode: MaxDistCodes.
	FixedDistanceTree
		bitLengths: ((WordArray new: MaxDistCodes+1) atAllPut: 5)
		codes: ((0 to: MaxDistCodes) collect:[:i| FixedDistanceTree reverseBits: i length: 5]).! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:07'!
initializeLengthCodes
	| length |
	BaseLength _ WordArray new: MaxLengthCodes.
	MatchLengthCodes _ WordArray new: MaxMatch - MinMatch + 1.
	length _ 0.
	1 to: MaxLengthCodes - 1 do:[:code|
		BaseLength at: code put: length.
		1 to: (1 bitShift: (ExtraLengthBits at: code)) do:[:n|
			length _ length + 1.
			MatchLengthCodes at: length put: NumLiterals + code]].
! !

!ZipConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:09'!
initializeWriteStreamConstants

	MaxBits _ 15.
	MaxBitLengthBits _ 7.
	EndBlock _ 256.

	StoredBlock _ 0.
	FixedBlock _ 1.
	DynamicBlock _ 2.

	NumLiterals _ 256.
	MaxLengthCodes _ 29.
	MaxDistCodes _ 30.
	MaxBitLengthCodes _ 19.
	MaxLiteralCodes _ NumLiterals + MaxLengthCodes + 1. "+ End of Block"

	Repeat3To6 _ 16. "Repeat previous bit length 3-6 times (2 bits repeat count)"
	Repeat3To10 _ 17. "Repeat previous bit length 3-10 times (3 bits repeat count)"
	Repeat11To138 _ 18. "Repeat previous bit length 11-138 times (7 bits repeat count)"

	self initializeExtraBits.
	self initializeLengthCodes.
	self initializeDistanceCodes.
	self initializeFixedTrees.
! !

!ZipFileConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 19:20'!
initialize
	"ZipFileConstants initialize"
	FaMsdos		:= 0.
	FaUnix 		:= 3.
	DeflatingCompressionNormal		:= 0.
	DeflatingCompressionMaximum	:= 2.
	DeflatingCompressionFast		:= 4.
	DeflatingCompressionSuperFast	:= 6.
	CompressionStored				:= 0.
	CompressionDeflated				:= 8.
	CompressionLevelNone			:= 0.
	CompressionLevelDefault			:= 6.
	IfaTextFile						:= 1.
	IfaBinaryFile					:= 0.
	CentralDirectoryFileHeaderSignature	:= 0.
	LocalFileHeaderSignature 			:= 0.
	EndOfCentralDirectorySignature 		:= 0.
	DataDescriptorLength 				:= 12.

	"Unix permission bits"
	DefaultDirectoryPermissions		:= 8r040755.
	DefaultFilePermissions			:= 8r0100666.
	DirectoryAttrib 					:= 8r040000.
	FileAttrib 						:= 8r0100000.

	CentralDirectoryFileHeaderSignature _ 
		(ByteArray with: 16r50 with: 16r4B with: 16r01 with: 16r02) asString.
	LocalFileHeaderSignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r03 with: 16r04) asString.
	EndOfCentralDirectorySignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r05 with: 16r06) asString.! !

!InflateStream methodsFor: 'testing' stamp: 'jmv 3/1/2010 11:28'!
atEnd
	"Note: It is possible that we have a few bits left,
	representing just the EOB marker. To check for
	this we must force decompression of the next
	block if at end of data."
	super atEnd ifFalse:[^false]. "Primitive test"
	(position >= readLimit and:[state = StateNoMoreData]) ifTrue:[^true].
	"Force decompression, by calling #next. Since #moveContentsToFront
	will never move data to the beginning of the buffer it is safe to
	skip back the read position afterwards"
	self next
		ifNil: [^ true].
	position _ position - 1.
	^false! !

!InflateStream methodsFor: 'bit access' stamp: 'jmv 3/1/2010 11:28'!
bitPosition
	"Return the current bit position of the source"
	^sourceStream
		ifNil: [sourcePos * 8 + bitPos]
		ifNotNil: [sourceStream position + sourcePos * 8 + bitPos]! !

!InflateStream methodsFor: 'bit access' stamp: 'ar 12/4/1998 02:00'!
nextBits: n
	| bits |
	[bitPos < n] whileTrue:[
		bitBuf _ bitBuf + (self nextByte bitShift: bitPos).
		bitPos _ bitPos + 8].
	bits _ bitBuf bitAnd: (1 bitShift: n)-1.
	bitBuf _ bitBuf bitShift: 0 - n.
	bitPos _ bitPos - n.
	^bits! !

!InflateStream methodsFor: 'bit access' stamp: 'ar 12/5/1998 14:54'!
nextByte
	^source byteAt: (sourcePos _ sourcePos + 1)! !

!InflateStream methodsFor: 'bit access' stamp: 'ar 12/4/1998 02:01'!
nextSingleBits: n
	| out |
	out _ 0.
	1 to: n do:[:i| out _ (out bitShift: 1) + (self nextBits: 1)].
	^out! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 15:31'!
close
	sourceStream ifNotNil:[sourceStream close].! !

!InflateStream methodsFor: 'accessing' stamp: 'jmv 7/12/2016 10:11:19'!
contents
	"Use a copy to avoid modifying receiver.
	The copy is shallow, i.e. cheap."
	^ self copy upToEnd! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/22/1999 01:29'!
next
	"Answer the next decompressed object in the Stream represented by the
	receiver."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^self pastEndRead]
		ifFalse: [^collection at: (position _ position + 1)]! !

!InflateStream methodsFor: 'accessing' stamp: 'nk 3/7/2004 18:45'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for simplicity"
	| newArray |

	"try to do it the fast way"
	position + anInteger < readLimit ifTrue: [
		newArray _ collection copyFrom: position + 1 to: position + anInteger.
		position _ position + anInteger.
		^newArray
	].

	"oh, well..."
	newArray _ collection species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: (self next ifNil: [ ^newArray copyFrom: 1 to: index - 1]) ].
	^newArray! !

!InflateStream methodsFor: 'accessing' stamp: 'jmv 3/1/2010 11:29'!
next: n into: buffer startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| c numRead count |
	numRead _ 0.
	["Force decompression if necessary"
	(c _ self next)
		ifNil: [ ^buffer copyFrom: 1 to: startIndex+numRead-1 ].
	"Store the first value which provoked decompression"
	buffer at: startIndex + numRead put: c.
	numRead _ numRead + 1.
	"After collection has been filled copy as many objects as possible"
	count _ (readLimit - position) min: (n - numRead).
	buffer 
		replaceFrom: startIndex + numRead 
		to: startIndex + numRead + count - 1 
		with: collection 
		startingAt: position+1.
	position _ position + count.
	numRead _ numRead + count.
	numRead = n] whileFalse.
	^buffer! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/3/1998 16:19'!
size
	"This is a compressed stream - we don't know the size beforehand"
	^self shouldNotImplement! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/21/1999 23:54'!
sourceLimit
	^sourceLimit! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/21/1999 23:52'!
sourcePosition
	^sourcePos! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 15:31'!
sourceStream
	^sourceStream! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/3/1998 16:19'!
upTo: anObject 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream _ WriteStream on: (collection species new: 100).
	[self atEnd or: [(element _ self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents! !

!InflateStream methodsFor: 'accessing' stamp: 'ar 12/22/1999 02:04'!
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream buffer |
	buffer _ collection species new: 1000.
	newStream _ WriteStream on: (collection species new: 100).
	[self atEnd] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].
	^ newStream contents! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/21/1999 22:59'!
computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits
	"Assign numerical values to all codes.
	Note: The values are stored according to the bit length"
	| offsets values baseOffset codeLength |
	offsets _ Array new: maxBits.
	offsets atAllPut: 0.
	baseOffset _ 1.
	minBits to: maxBits do:[:bits|
		offsets at: bits put: baseOffset.
		baseOffset _ baseOffset + (counts at: bits+1)].
	values _ WordArray new: aCollection size.
	1 to: aCollection size do:[:i|
		codeLength _ aCollection at: i.
		codeLength > 0 ifTrue:[
			baseOffset _ offsets at: codeLength.
			values at: baseOffset put: i-1.
			offsets at: codeLength put: baseOffset + 1]].
	^values! !

!InflateStream methodsFor: 'huffman trees' stamp: 'sma 5/12/2000 10:49'!
createHuffmanTables: values counts: counts from: minBits to: maxBits
	"Create the actual tables"
	| table tableStart tableSize tableEnd 
	valueIndex tableStack numValues deltaBits maxEntries
	lastTable lastTableStart tableIndex lastTableIndex |

	table _ WordArray new: ((4 bitShift: minBits) max: 16).

	"Create the first entry - this is a dummy.
	It gives us information about how many bits to fetch initially."
	table at: 1 put: (minBits bitShift: 24) + 2. "First actual table starts at index 2"

	"Create the first table from scratch."
	tableStart _ 2. "See above"
	tableSize _ 1 bitShift: minBits.
	tableEnd _ tableStart + tableSize.
	"Store the terminal symbols"
	valueIndex _ (counts at: minBits+1).
	tableIndex _ 0.
	1 to: valueIndex do:[:i|
		table at: tableStart + tableIndex put: (values at: i).
		tableIndex _ self increment: tableIndex bits: minBits].
	"Fill up remaining entries with invalid entries"
	tableStack _ OrderedCollection new: 10. "Should be more than enough"
	tableStack addLast: 
		(Array 
			with: minBits	"Number of bits (e.g., depth) for this table"
			with: tableStart	"Start of table"
			with: tableIndex "Next index in table"
			with: minBits	"Number of delta bits encoded in table"
			with: tableSize - valueIndex "Entries remaining in table").
	"Go to next value index"
	valueIndex _ valueIndex + 1.
	"Walk over remaining bit lengths and create new subtables"
	minBits+1 to: maxBits do:[:bits|
		numValues _ counts at: bits+1.
		[numValues > 0] whileTrue:["Create a new subtable"
			lastTable _ tableStack last.
			lastTableStart _ lastTable at: 2.
			lastTableIndex _ lastTable at: 3.
			deltaBits _ bits - (lastTable at: 1).
			"Make up a table of deltaBits size"
			tableSize _ 1 bitShift: deltaBits.
			tableStart _ tableEnd.
			tableEnd _ tableEnd + tableSize.
			[tableEnd > table size ]
				whileTrue:[table _ self growHuffmanTable: table].
			"Connect to last table"
			self assert:[(table at: lastTableStart + lastTableIndex) = 0]."Entry must be unused"
			table at: lastTableStart + lastTableIndex put: (deltaBits bitShift: 24) + tableStart.
			lastTable at: 3 put: (self increment: lastTableIndex bits: (lastTable at: 4)).
			lastTable at: 5 put: (lastTable at: 5) - 1.
			self assert:[(lastTable at: 5) >= 0]. "Don't exceed tableSize"
			"Store terminal values"
			maxEntries _ numValues min: tableSize.
			tableIndex _ 0.
			1 to: maxEntries do:[:i|
				table at: tableStart + tableIndex put: (values at: valueIndex).
				valueIndex _ valueIndex + 1.
				numValues _ numValues - 1.
				tableIndex _ self increment: tableIndex bits: deltaBits].
			"Check if we have filled up the current table completely"
			maxEntries = tableSize ifTrue:[
				"Table has been filled. Back up to the last table with space left."
				[tableStack isEmpty not and:[(tableStack last at: 5) = 0]]
						whileTrue:[tableStack removeLast].
			] ifFalse:[
				"Table not yet filled. Put it back on the stack."
				tableStack addLast:
					(Array
						with: bits		"Nr. of bits in this table"
						with: tableStart	"Start of table"
						with: tableIndex "Index in table"
						with: deltaBits	"delta bits of table"
						with: tableSize - maxEntries "Unused entries in table").
			].
		].
	].
	 ^table copyFrom: 1 to: tableEnd-1! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 02:25'!
decodeDynamicTable: nItems from: aHuffmanTable
	"Decode the code length of the literal/length and distance table
	in a block compressed with dynamic huffman trees"
	| values index value repCount theValue |
	values _ Array new: nItems.
	index _ 1.
	theValue _ 0.
	[index <= nItems] whileTrue:[
		value _ self decodeValueFrom: aHuffmanTable.
		value < 16 ifTrue:[
			"Immediate values"
			theValue _ value.
			values at: index put: value.
			index _ index+1.
		] ifFalse:[
			"Repeated values"
			value = 16 ifTrue:[
				"Repeat last value"
				repCount _ (self nextBits: 2) + 3.
			] ifFalse:[
				"Repeat zero value"
				theValue _ 0.
				value = 17 
					ifTrue:[repCount _ (self nextBits: 3) + 3]
					ifFalse:[value = 18 
								ifTrue:[repCount _ (self nextBits: 7) + 11]
								ifFalse:[^self error:'Invalid bits tree value']]].
			0 to: repCount-1 do:[:i| values at: index+i put: theValue].
			index _ index + repCount].
	].
	^values! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 01:51'!
distanceMap
	"This is used by the fast decompressor"
	^nil! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/3/1998 13:16'!
growHuffmanTable: table
	| newTable |
	newTable _ table species new: table size * 2.
	newTable replaceFrom: 1 to: table size with: table startingAt: 1.
	^newTable! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 02:27'!
huffmanTableFrom: aCollection mappedBy: valueMap
	"Create a new huffman table from the given code lengths.
	Map the actual values by valueMap if it is given.
	See the class comment for a documentation of the huffman
	tables used in this decompressor."
	| counts  values table minBits maxBits |
	minBits _ MaxBits + 1.
	maxBits _ 0.
	"Count the occurences of each code length and compute minBits and maxBits"
	counts _ Array new: MaxBits+1.
	counts atAllPut: 0.
	aCollection do:[:length| 
		length > 0 ifTrue:[
			length < minBits ifTrue:[minBits _ length].
			length > maxBits ifTrue:[maxBits _ length].
			counts at: length+1 put: (counts at: length+1)+1]].
	maxBits = 0 ifTrue:[^nil]. "Empty huffman table"

	"Assign numerical values to all codes."
	values _ self computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits.

	"Map the values if requested"
	self mapValues: values by: valueMap.

	"Create the actual tables"
	table _ self createHuffmanTables: values counts: counts from: minBits to: maxBits.

	^table! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 01:48'!
increment: value bits: nBits
	"Increment a value of nBits length.
	The fast decompressor will do this differently"
	^value+1! !

!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 01:50'!
literalLengthMap
	"This is used by the fast decompressor"
	^nil! !

!InflateStream methodsFor: 'huffman trees' stamp: 'jmv 3/2/2010 16:07'!
mapValues: values by: valueMap
	| oldValue |
	valueMap ifNil:[^values].
	1 to: values size do:[:i|
		oldValue _ values at: i.
		"Note: there may be nil values if not all values are used"
		oldValue
			ifNil: [ ^values]
			ifNotNil: [ values at: i put: (valueMap at: oldValue+1)]]! !

!InflateStream methodsFor: 'crc' stamp: 'ar 2/29/2004 04:04'!
crcError: aString
	^CRCError signal: aString! !

!InflateStream methodsFor: 'crc' stamp: 'ar 2/29/2004 03:49'!
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection.
	Subclasses can implement the appropriate means for the check sum they wish to use."
	^oldCrc! !

!InflateStream methodsFor: 'crc' stamp: 'ar 2/29/2004 04:22'!
verifyCrc
	"Verify the crc checksum in the input"! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/4/1998 02:24'!
decodeValueFrom: table
	"Decode the next value in the receiver using the given huffman table."
	| bits bitsNeeded tableIndex value |
	bitsNeeded _ (table at: 1) bitShift: -24.	"Initial bits needed"
	tableIndex _ 2.							"First real table"
	[bits _ self nextSingleBits: bitsNeeded.	"Get bits"
	value _ table at: (tableIndex + bits).		"Lookup entry in table"
	(value bitAnd: 16r3F000000) = 0] 			"Check if it is a non-leaf node"
		whileFalse:["Fetch sub table"
			tableIndex _ value bitAnd: 16rFFFF.	"Table offset in low 16 bit"
			bitsNeeded _ (value bitShift: -24) bitAnd: 255. "Additional bits in high 8 bit"
			bitsNeeded > MaxBits ifTrue:[^self error:'Invalid huffman table entry']].
	^value! !

!InflateStream methodsFor: 'inflating' stamp: 'yo 5/9/2014 15:19'!
decompressBlock: llTable with: dTable
	"Process the compressed data in the block.
	llTable is the huffman table for literal/length codes
	and dTable is the huffman table for distance codes."
	| value extra length distance oldPos oldBits oldBitPos |
	[readLimit < collection size and: [sourcePos <= sourceLimit]] whileTrue: [
		"Back up stuff if we're running out of space"
		oldBits _ bitBuf.
		oldBitPos _ bitPos.
		oldPos _ sourcePos.
		value _ self decodeValueFrom: llTable.
		value < 256 ifTrue:[ "A literal"
			collection byteAt: (readLimit _ readLimit + 1) put: value.
		] ifFalse: ["length/distance or end of block"
			value = 256 ifTrue:["End of block"
				state _ state bitAnd: StateNoMoreData.
				^self].
			"Compute the actual length value (including possible extra bits)"
			extra _ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0) at: value - 256.
			length _ #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258) at: value - 256.
			extra > 0 ifTrue:[length _ length + (self nextBits: extra)].
			"Compute the distance value"
			value _ self decodeValueFrom: dTable.
			extra _ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13) at: value+1.
			distance _ #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
						1025 1537 2049 3073 4097 6145 8193 12289 16385 24577) at: value+1.
			extra > 0 ifTrue: [distance _ distance + (self nextBits: extra)].
			(readLimit + length >= collection size) ifTrue:[
				bitBuf _ oldBits.
				bitPos _ oldBitPos.
				sourcePos _ oldPos.
				^self].
			collection 
					replaceFrom: readLimit+1 
					to: readLimit + length
					with: collection 
					startingAt: readLimit - distance + 1.
			readLimit _ readLimit + length.
		].
	].! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/3/1998 20:49'!
proceedDynamicBlock
	self decompressBlock: litTable with: distTable! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/3/1998 20:49'!
proceedFixedBlock
	self decompressBlock: litTable with: distTable! !

!InflateStream methodsFor: 'inflating' stamp: 'jmv 3/1/2010 11:29'!
proceedStoredBlock
	"Proceed decompressing a stored (e.g., uncompressed) block"
	| length decoded |
	"Literal table must be nil for a stored block"
	litTable
		ifNotNil: [^ self error: 'Bad state'].
	length _ distTable.
	[length > 0 and:[readLimit < collection size and:[sourcePos < sourceLimit]]] 
		whileTrue:[
			collection at: (readLimit _ readLimit + 1) put: 
				(source at: (sourcePos _ sourcePos + 1)).
			length _ length - 1].
	length = 0 ifTrue:[state _ state bitAnd: StateNoMoreData].
	decoded _ length - distTable.
	distTable _ length.
	^decoded! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/4/1998 01:46'!
processDynamicBlock
	| nLit nDist nLen codeLength lengthTable bits |
	nLit _ (self nextBits: 5) + 257.
	nDist _ (self nextBits: 5) + 1.
	nLen _ (self nextBits: 4) + 4.
	codeLength _ Array new: 19.
	codeLength atAllPut: 0.
	1 to: nLen do:[:i|
		bits _ #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) at: i.
		codeLength at: bits+1 put: (self nextBits: 3).
	].
	lengthTable _ self huffmanTableFrom: codeLength mappedBy: nil.
	"RFC 1951: In other words, all code lengths form a single sequence..."
	codeLength _ self decodeDynamicTable: nLit+nDist from: lengthTable.
	litTable _ self 
				huffmanTableFrom: (codeLength copyFrom: 1 to: nLit)
				mappedBy: self literalLengthMap.
	distTable _ self 
				huffmanTableFrom: (codeLength copyFrom: nLit+1 to: codeLength size)
				mappedBy: self distanceMap.
	state _ state bitOr: BlockProceedBit.
	self proceedDynamicBlock.! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/4/1998 19:13'!
processFixedBlock
	litTable _ self 
				huffmanTableFrom: FixedLitCodes
				mappedBy: self literalLengthMap.
	distTable _ self 
				huffmanTableFrom: FixedDistCodes
				mappedBy: self distanceMap.
	state _ state bitOr: BlockProceedBit.
	self proceedFixedBlock.! !

!InflateStream methodsFor: 'inflating' stamp: 'ar 12/27/1999 13:49'!
processStoredBlock
	| chkSum length |
	"Skip to byte boundary"
	self nextBits: (bitPos bitAnd: 7).
	length _ self nextBits: 16.
	chkSum _ self nextBits: 16.
	(chkSum bitXor: 16rFFFF) = length
		ifFalse:[^self error:'Bad block length'].
	litTable _ nil.
	distTable _ length.
	state _ state bitOr: BlockProceedBit.
	^self proceedStoredBlock! !

!InflateStream methodsFor: 'private' stamp: 'ar 12/4/1998 02:03'!
decompressAll
	"Profile the decompression speed"
	[self atEnd] whileFalse:[
		position _ readLimit.
		self next "Provokes decompression"
	].! !

!InflateStream methodsFor: 'private' stamp: 'jmv 3/1/2010 11:29'!
getFirstBuffer
	"Get the first source buffer after initialization has been done"
	sourceStream
		ifNil: [^ self].
	source _ sourceStream next: 1 << 16. "This is more than enough..."
	sourceLimit _ source size! !

!InflateStream methodsFor: 'private' stamp: 'ar 12/3/1998 17:32'!
getNextBlock
	^self nextBits: 3! !

!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:17'!
moveContentsToFront
	"Move the decoded contents of the receiver to the front so that we have enough space for decoding more data."
	| delta |
	readLimit > 32768 ifTrue:[
		delta _ readLimit - 32767.
		collection 
			replaceFrom: 1 
			to: collection size - delta + 1 
			with: collection 
			startingAt: delta.
		position _ position - delta + 1.
		readLimit _ readLimit - delta + 1].! !

!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:27'!
moveSourceToFront
	"Move the encoded contents of the receiver to the front so that we have enough space for decoding more data."
	(sourceStream == nil or:[sourceStream atEnd]) ifTrue:[^self].
	sourcePos > 10000 ifTrue:[
		source 
			replaceFrom: 1 
			to: source size - sourcePos
			with: source 
			startingAt: sourcePos + 1.
		source _ sourceStream 
			next: sourcePos 
			into: source 
			startingAt: source size - sourcePos + 1.
		sourcePos _ 0.
		sourceLimit _ source size].! !

!InflateStream methodsFor: 'private' stamp: 'ar 2/29/2004 04:18'!
pastEndRead
	"A client has attempted to read beyond the read limit.
	Check in what state we currently are and perform
	the appropriate action"
	| blockType bp oldLimit |
	state = StateNoMoreData ifTrue:[^nil]. "Get out early if possible"
	"Check if we can move decoded data to front"
	self moveContentsToFront.
	"Check if we can fetch more source data"
	self moveSourceToFront.
	state = StateNewBlock ifTrue:[state _ self getNextBlock].
	blockType _ state bitShift: -1.
	bp _ self bitPosition.
	oldLimit := readLimit.
	self perform: (BlockTypes at: blockType+1).
	"Note: if bit position hasn't advanced then nothing has been decoded."
	bp = self bitPosition 
		ifTrue:[^self primitiveFailed].
	"Update crc for the decoded contents"
	readLimit > oldLimit 
		ifTrue:[crc _ self updateCrc: crc from: oldLimit+1 to: readLimit in: collection].
	state = StateNoMoreData ifTrue:[self verifyCrc].
	^self next! !

!InflateStream methodsFor: 'private' stamp: 'ar 12/4/1998 02:03'!
profile
	"Profile the decompression speed"
	MessageTally spyOn:[self decompressAll].! !

!InflateStream methodsFor: 'initialization' stamp: 'jmv 1/12/2010 11:51'!
on: aCollectionOrStream
	(aCollectionOrStream is: #Stream)
		ifTrue: [	aCollectionOrStream binary.
				sourceStream _ aCollectionOrStream.
				self getFirstBuffer]
		ifFalse: [ source _ aCollectionOrStream].
	^self on: source from: 1 to: source size.! !

!InflateStream methodsFor: 'initialization' stamp: 'ar 12/23/1999 15:35'!
on: aCollection from: firstIndex to: lastIndex
	bitBuf _ bitPos _ 0.
	"The decompression buffer has a size of at 64k,
	since we may have distances up to 32k back and
	repetitions of at most 32k length forward"
	collection _ aCollection species new: 1 << 16.
	readLimit _ 0. "Not yet initialized"
	position _ 0.
	source _ aCollection.
	sourceLimit _ lastIndex.
	sourcePos _ firstIndex-1.
	state _ StateNewBlock.! !

!InflateStream methodsFor: 'initialization' stamp: 'ar 12/3/1998 16:32'!
reset
	"Position zero - nothing decoded yet"
	position _ readLimit _ 0.
	sourcePos _ 0.
	bitBuf _ bitPos _ 0.
	state _ 0.! !

!InflateStream class methodsFor: 'class initialization' stamp: 'ar 12/4/1998 19:12'!
initialize
	"InflateStream initialize"
	MaxBits _ 16.
	StateNewBlock _ 0.
	StateNoMoreData _ 1.
	BlockProceedBit _ 8.
	BlockTypes _ #(	processStoredBlock	"New block in stored format"
					processFixedBlock	"New block with fixed huffman tables"
					processDynamicBlock	"New block with dynamic huffman tables"
					errorBadBlock		"Bad block format"
					proceedStoredBlock	"Continue block in stored format"
					proceedFixedBlock	"Continue block in fixed format"
					proceedDynamicBlock	"Continue block in dynamic format"
					errorBadBlock		"Bad block format").
	"Initialize fixed block values"
	FixedLitCodes _ 	((1 to: 144) collect:[:i| 8]),
					((145 to: 256) collect:[:i| 9]),
					((257 to: 280) collect:[:i| 7]),
					((281 to: 288) collect:[:i| 8]).
	FixedDistCodes _ ((1 to: 32) collect:[:i| 5]).! !

!FastInflateStream methodsFor: 'inflating' stamp: 'yo 5/9/2014 15:19'!
decompressBlock: llTable with: dTable
	"Process the compressed data in the block.
	llTable is the huffman table for literal/length codes
	and dTable is the huffman table for distance codes."
	| value extra length distance oldPos oldBits oldBitPos |
	<primitive: 'primitiveInflateDecompressBlock' module: 'ZipPlugin'>
	[readLimit < collection size and:[sourcePos <= sourceLimit]] whileTrue:[
		"Back up stuff if we're running out of space"
		oldBits _ bitBuf.
		oldBitPos _ bitPos.
		oldPos _ sourcePos.
		value _ self decodeValueFrom: llTable.
		value < 256 ifTrue:[ "A literal"
			collection byteAt: (readLimit _ readLimit + 1) put: value.
		] ifFalse: ["length/distance or end of block"
			value = 256 ifTrue:["End of block"
				state _ state bitAnd: StateNoMoreData.
				^self].
			"Compute the actual length value (including possible extra bits)"
			extra _ (value bitShift: -16) - 1.
			length _ value bitAnd: 16rFFFF.
			extra > 0 ifTrue:[length _ length + (self nextBits: extra)].
			"Compute the distance value"
			value _ self decodeValueFrom: dTable.
			extra _ (value bitShift: -16).
			distance _ value bitAnd: 16rFFFF.
			extra > 0 ifTrue: [distance _ distance + (self nextBits: extra)].
			(readLimit + length >= collection size) ifTrue:[
				bitBuf _ oldBits.
				bitPos _ oldBitPos.
				sourcePos _ oldPos.
				^self].
			collection 
					replaceFrom: readLimit+1 
					to: readLimit + length
					with: collection 
					startingAt: readLimit - distance + 1.
			readLimit _ readLimit + length.
		].
	].! !

!FastInflateStream methodsFor: 'inflating' stamp: 'ar 12/4/1998 19:15'!
processFixedBlock
	litTable _ FixedLitTable.
	distTable _ FixedDistTable.
	state _ state bitOr: BlockProceedBit.
	self proceedFixedBlock.! !

!FastInflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 02:26'!
distanceMap
	^DistanceMap! !

!FastInflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 01:48'!
increment: value bits: nBits
	"Increment value in reverse bit order, e.g. 
	for a 3 bit value count as follows:
		000 / 100 / 010 / 110
		001 / 101 / 011 / 111
	See the class comment why we need this."
	| result bit |
	result _ value.
	"Test the lowest bit first"
	bit _ 1 << (nBits - 1).
	"If the currently tested bit is set then we need to
	turn this bit off and test the next bit right to it"
	[(result bitAnd: bit) = 0] whileFalse:[ 
		"Turn off current bit"
		result _ result bitXor: bit.
		"And continue testing the next bit"
		bit _ bit bitShift: -1].
	"Turn on the right-most bit that we haven't touched in the loop above"
	^result bitXor: bit! !

!FastInflateStream methodsFor: 'huffman trees' stamp: 'ar 12/4/1998 02:26'!
literalLengthMap
	^LiteralLengthMap! !

!FastInflateStream methodsFor: 'bit access' stamp: 'ar 12/4/1998 02:02'!
nextSingleBits: n
	"Fetch the bits all at once"
	^self nextBits: n.! !

!FastInflateStream class methodsFor: 'class initialization' stamp: 'ar 12/21/1999 23:00'!
initialize
	"FastInflateStream initialize"
	| low high |

	"Init literal/length map"
	low _ #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 ).
	high _ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0).
	LiteralLengthMap _ WordArray new: 256 + 32.
	1 to: 257 do:[:i| LiteralLengthMap at: i put: i-1].
	1 to: 29 do:[:i| LiteralLengthMap at: 257+i put: (low at:i) + ( (high at: i) + 1 << 16)].

	"Init distance map"
	high _ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	low _ #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
			1025 1537 2049 3073 4097 6145 8193 12289 16385 24577).
	DistanceMap _ WordArray new: 32.
	1 to: 30 do:[:i| DistanceMap at: i put: (low at: i) + ( (high at: i) << 16)].

	"Init fixed block huffman tables"
	FixedLitTable _ self basicNew
				huffmanTableFrom: FixedLitCodes
				mappedBy: LiteralLengthMap.
	FixedDistTable _ self basicNew
				huffmanTableFrom: FixedDistCodes
				mappedBy: DistanceMap.! !

!GZipReadStream methodsFor: 'initialization' stamp: 'ar 2/29/2004 03:32'!
on: aCollection from: firstIndex to: lastIndex
	"Check the header of the GZIP stream."
	| method magic flags length |
	super on: aCollection from: firstIndex to: lastIndex.
	crc _ 16rFFFFFFFF.
	magic _ self nextBits: 16.
	(magic = GZipMagic) 
		ifFalse:[^self error:'Not a GZipped stream'].
	method _ self nextBits: 8.
	(method = GZipDeflated)
		ifFalse:[^self error:'Bad compression method'].
	flags _ self nextBits: 8.
	(flags anyMask: GZipEncryptFlag) 
		ifTrue:[^self error:'Cannot decompress encrypted stream'].
	(flags anyMask: GZipReservedFlags)
		ifTrue:[^self error:'Cannot decompress stream with unknown flags'].
	"Ignore stamp, extra flags, OS type"
	self nextBits: 16; nextBits: 16. "stamp"
	self nextBits: 8. "extra flags"
	self nextBits: 8. "OS type"
	(flags anyMask: GZipContinueFlag) "Number of multi-part archive - ignored"
		ifTrue:[self nextBits: 16]. 
	(flags anyMask: GZipExtraField) "Extra fields - ignored"
		ifTrue:[	length _ self nextBits: 16.
				1 to: length do:[:i| self nextBits: 8]].
	(flags anyMask: GZipNameFlag) "Original file name - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse].
	(flags anyMask: GZipCommentFlag) "Comment - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse].
! !

!GZipReadStream methodsFor: 'crc' stamp: 'ar 2/29/2004 03:30'!
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection"
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection.! !

!GZipReadStream methodsFor: 'crc' stamp: 'ar 2/29/2004 04:20'!
verifyCrc
	| stored |
	stored := 0.
	0 to: 24 by: 8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored := stored bitXor: 16rFFFFFFFF.
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored! !

!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'pb 2/26/2020 13:51:16'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that are serviced by us."

	^suffix = 'gz'
		ifTrue: [ {self serviceViewDecompress. self serviceDecompressToFile} ]
		ifFalse: [ #() ]! !

!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'jmv 6/24/2020 16:07:05'!
saveContents: aFileEntry
	"Save the contents of a gzipped file"
	| zipped buffer newFile |
	newFile := aFileEntry parent // aFileEntry baseName.
	newFile writeStreamDo: [ :unzipped |
		unzipped binary.
		aFileEntry readStreamDo: [ :zipContents |
			zipped _ GZipReadStream on: zipContents.
			buffer _ ByteArray new: 50000.
			'Extracting ' , aFileEntry asString
				displayProgressAt: Sensor mousePoint
				from: 0
				to: zipped sourceStream size
				during: [ :barBlock | 
					[ zipped atEnd ]
						whileFalse: [
							barBlock value: zipped sourceStream position.
							unzipped nextPutAll: (zipped nextInto: buffer) ]]]].
	^ newFile ! !

!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'HAW 1/2/2019 18:36:16'!
serviceDecompressToFile

	^ (SimpleServiceEntry 
		provider: self 
		label: 'decompress to file'
		selector: #saveContents:
		description: 'decompress to file'
		icon: #goTopIcon) triggerFileListChanged! !

!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'HAW 1/2/2019 18:37:36'!
serviceViewDecompress

	^ SimpleServiceEntry 
		provider: self 
		label: 'view decompressed'
		selector: #viewContents:
		description: 'view decompressed'
		icon: #findIcon! !

!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'pb 2/26/2020 13:44:18'!
viewContents: aFileEntry
	"Open the decompressed contents of the .gz file with the given name.  This method is only required for the registering-file-list of Squeak 3.3a and beyond, but does no harm in an earlier system"

	(aFileEntry readStream ) ifNotNil: [ :aStream | 
		aStream viewGZipContents]! !

!ZLibReadStream methodsFor: 'initialization' stamp: 'ar 2/29/2004 03:31'!
on: aCollection from: firstIndex to: lastIndex
	"Check the header of the ZLib stream."
	| method byte |
	super on: aCollection from: firstIndex to: lastIndex.
	crc _ 1.
	method _ self nextBits: 8.
	(method bitAnd: 15) = 8 ifFalse:[^self error:'Unknown compression method'].
	(method bitShift: -4) + 8 > 15 ifTrue:[^self error:'Invalid window size'].
	byte _ self nextBits: 8.
	(method bitShift: 8) + byte \\ 31 = 0 ifFalse:[^self error:'Incorrect header'].
	(byte anyMask: 32) ifTrue:[^self error:'Need preset dictionary'].
! !

!ZLibReadStream methodsFor: 'crc' stamp: 'ar 2/29/2004 03:30'!
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection"
	^ZLibWriteStream updateAdler32: oldCrc from: start to: stop in: aCollection.! !

!ZLibReadStream methodsFor: 'crc' stamp: 'ar 2/29/2004 04:21'!
verifyCrc
	| stored |
	stored := 0.
	24 to: 0 by: -8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored! !

!ZipReadStream methodsFor: 'crc' stamp: 'nk 3/7/2004 18:55'!
expectedCrc: aNumberOrNil
	"If expectedCrc is set, it will be compared against the calculated CRC32 in verifyCrc.
	This number should be the number read from the Zip header (which is the bitwise complement of my crc if all is working correctly)"
	expectedCrc _ aNumberOrNil! !

!ZipReadStream methodsFor: 'crc' stamp: 'nk 3/7/2004 15:32'!
updateCrc: oldCrc from: start to: stop in: aCollection
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection! !

!ZipReadStream methodsFor: 'crc' stamp: 'jmv 2/22/2011 22:55'!
verifyCrc
	"Verify the CRC-32 checksum calculated from the input against the expected CRC-32, if any.
	Answer the calculated CRC-32 in any case.
	Note that the CRC-32 used in Zip files is actually the bit inverse of the calculated value, so that is what is returned."

	| invertedCrc |
	invertedCrc _ crc bitXor: 16rFFFFFFFF.
	(expectedCrc notNil and: [ expectedCrc ~= invertedCrc ])
		ifTrue: [ ^ self crcError: ('Wrong CRC-32 (expected {1} got {2}) (proceed to ignore)' format: { expectedCrc hex. invertedCrc hex }) ].
	^invertedCrc! !

!ZipReadStream methodsFor: 'initialization' stamp: 'nk 3/7/2004 15:31'!
on: aCollection from: firstIndex to: lastIndex
	super on: aCollection from: firstIndex to: lastIndex.
	crc _ 16rFFFFFFFF.
	expectedCrc _ nil.! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 20:24'!
compare: here with: matchPos min: minLength
	"Compare the two strings and return the length of matching characters.
	minLength is a lower bound for match lengths that will be accepted.
	Note: here and matchPos are zero based."
	| length |
	"First test if we can actually get longer than minLength"
	(collection at: here+minLength+1) = (collection at: matchPos+minLength+1)
		ifFalse:[^0].
	(collection at: here+minLength) = (collection at: matchPos+minLength)
		ifFalse:[^0].
	"Then test if we have an initial match at all"
	(collection at: here+1) = (collection at: matchPos+1)
		ifFalse:[^0].
	(collection at: here+2) = (collection at: matchPos+2)
		ifFalse:[^1].
	"Finally do the real comparison"
	length _ 3.
	[length <= MaxMatch and:[
		(collection at: here+length) = (collection at: matchPos+length)]]
			whileTrue:[length _ length + 1].
	^length - 1! !

!DeflateStream methodsFor: 'deflating' stamp: 'jmv 3/1/2010 09:56'!
deflateBlock
	"Deflate the current contents of the stream"
	| flushNeeded lastIndex |
	blockStart ifNil: [
		"One time initialization for the first block"
		1 to: MinMatch-1 do:[:i| self updateHashAt: i].
		blockStart _ 0].

	[blockPosition < position] whileTrue:[
		(position + MaxMatch > writeLimit)
			ifTrue:[lastIndex _ writeLimit - MaxMatch]
			ifFalse:[lastIndex _ position].
		flushNeeded _ self deflateBlock: lastIndex-1
							chainLength: self hashChainLength
							goodMatch: self goodMatchLength.
		flushNeeded ifTrue:[
			self flushBlock.
			blockStart _ blockPosition].
		"Make room for more data"
		self moveContentsToFront].
! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 18:05'!
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
	"Continue deflating the receiver's collection from blockPosition to lastIndex.
	Note that lastIndex must be at least MaxMatch away from the end of collection"
	| here matchResult flushNeeded hereMatch hereLength newMatch newLength hasMatch |
	blockPosition > lastIndex ifTrue:[^false]. "Nothing to deflate"
	hasMatch _ false.
	here _ blockPosition.
	[here <= lastIndex] whileTrue:[
		hasMatch ifFalse:[
			"Find the first match"
			matchResult _ self findMatch: here
								lastLength: MinMatch-1
								lastMatch: here
								chainLength: chainLength
								goodMatch: goodMatch.
			self insertStringAt: here. "update hash table"
			hereMatch _ matchResult bitAnd: 16rFFFF.
			hereLength _ matchResult bitShift: -16].

		"Look ahead if there is a better match at the next position"
		matchResult _ self findMatch: here+1
							lastLength: hereLength
							lastMatch: hereMatch
							chainLength: chainLength
							goodMatch: goodMatch.
		newMatch _ matchResult bitAnd: 16rFFFF.
		newLength _ matchResult bitShift: -16.

		"Now check if the next match is better than the current one.
		If not, output the current match (provided that the current match
		is at least MinMatch long)"
		(hereLength >= newLength and:[hereLength >= MinMatch]) ifTrue:[
			self assert:[self validateMatchAt: here
							from: hereMatch to: hereMatch + hereLength - 1].
			"Encode the current match"
			flushNeeded _ self
				encodeMatch: hereLength
				distance: here - hereMatch.
			"Insert all strings up to the end of the current match.
			Note: The first string has already been inserted."
			1 to: hereLength-1 do:[:i| self insertStringAt: (here _ here + 1)].
			hasMatch _ false.
			here _ here + 1.
		] ifFalse:[
			"Either the next match is better than the current one or we didn't
			have a good match after all (e.g., current match length < MinMatch).
			Output a single literal."
			flushNeeded _ self encodeLiteral: (collection byteAt: (here + 1)).
			here _ here + 1.
			(here <= lastIndex and:[flushNeeded not]) ifTrue:[
				"Cache the results for the next round"
				self insertStringAt: here.
				hasMatch _ true.
				hereMatch _ newMatch.
				hereLength _ newLength].
		].
		flushNeeded ifTrue:[blockPosition _ here. ^true].
	].
	blockPosition _ here.
	^false! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 17:45'!
findMatch: here lastLength: lastLength lastMatch: lastMatch chainLength: maxChainLength goodMatch: goodMatch
	"Find the longest match for the string starting at here.
	If there is no match longer than lastLength return lastMatch/lastLength.
	Traverse at most maxChainLength entries in the hash table.
	Stop if a match of at least goodMatch size has been found."
	| matchResult matchPos distance chainLength limit bestLength length |
	"Compute the default match result"
	matchResult _ (lastLength bitShift: 16) bitOr: lastMatch.

	"There is no way to find a better match than MaxMatch"
	lastLength >= MaxMatch ifTrue:[^matchResult].

	"Start position for searches"
	matchPos _ hashHead at: (self updateHashAt: here + MinMatch) + 1.

	"Compute the distance to the (possible) match"
	distance _ here - matchPos.

	"Note: It is required that 0 < distance < MaxDistance"
	(distance > 0 and:[distance < MaxDistance]) ifFalse:[^matchResult].

	chainLength _ maxChainLength.	"Max. nr of match chain to search"
	here > MaxDistance	"Limit for matches that are too old"
		ifTrue:[limit _ here - MaxDistance]
		ifFalse:[limit _ 0].

	"Best match length so far (current match must be larger to take effect)"
	bestLength _ lastLength.

	["Compare the current string with the string at match position"
	length _ self compare: here with: matchPos min: bestLength.
	"Truncate accidental matches beyound stream position"
	(here + length > position) ifTrue:[length _ position - here].
	"Ignore very small matches if they are too far away"
	(length = MinMatch and:[(here - matchPos) > (MaxDistance // 4)])
		ifTrue:[length _ MinMatch - 1].
	length > bestLength ifTrue:["We have a new (better) match than before"
		"Compute the new match result"
		matchResult _ (length bitShift: 16) bitOr: matchPos.
		bestLength _ length.
		"There is no way to find a better match than MaxMatch"
		bestLength >= MaxMatch ifTrue:[^matchResult].
		"But we may have a good, fast match"
		bestLength > goodMatch ifTrue:[^matchResult].
	].
	(chainLength _ chainLength - 1) > 0] whileTrue:[
		"Compare with previous entry in hash chain"
		matchPos _ hashTail at: (matchPos bitAnd: WindowMask) + 1.
		matchPos <= limit ifTrue:[^matchResult]. "Match position is too old"
	].
	^matchResult! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/28/1999 17:37'!
flushBlock
	"Flush a deflated block"! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 17:46'!
insertStringAt: here
	"Insert the string at the given start position into the hash table.
	Note: The hash value is updated starting at MinMatch-1 since
	all strings before have already been inserted into the hash table
	(and the hash value is updated as well)."
	| prevEntry |
	hashValue _ self updateHashAt: (here + MinMatch).
	prevEntry _ hashHead at: hashValue+1.
	hashHead at: hashValue+1 put: here.
	hashTail at: (here bitAnd: WindowMask)+1 put: prevEntry.! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 17:48'!
updateHash: nextValue
	"Update the running hash value based on the next input byte.
	Return the new updated hash value."
	^((hashValue bitShift: HashShift) bitXor: nextValue) bitAnd: HashMask.! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/29/1999 17:47'!
updateHashAt: here
	"Update the hash value at position here (one based)"
	^self updateHash: (collection byteAt: here)! !

!DeflateStream methodsFor: 'deflating' stamp: 'ar 12/28/1999 17:43'!
validateMatchAt: pos from: startPos to: endPos
	| here |
	here _ pos.
	startPos+1 to: endPos+1 do:[:i|
		(collection at: i) = (collection at: (here _ here + 1))
			ifFalse:[^self error:'Not a match']].
	^true! !

!DeflateStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:04'!
encodeLiteral: literal
	"Encode the given literal.
	Return true if the current block needs to be flushed."
	^false! !

!DeflateStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:04'!
encodeMatch: matchLength distance: matchDistance
	"Encode a match of the given length and distance.
	Return true if the current block should be flushed."
	^false! !

!DeflateStream methodsFor: 'initialization' stamp: 'ar 12/29/1999 17:30'!
flush
	"Force compression"
	self deflateBlock.! !

!DeflateStream methodsFor: 'initialization' stamp: 'ar 12/31/1999 18:00'!
initialize
	blockStart _ nil.
	blockPosition _ 0.
	hashValue _ 0.
	self initializeHashTables.! !

!DeflateStream methodsFor: 'initialization' stamp: 'ar 12/29/1999 17:32'!
initializeHashTables
	hashHead _ WordArray new: 1 << HashBits.
	hashTail _ WordArray new: WindowSize.
! !

!DeflateStream methodsFor: 'initialization' stamp: 'ar 12/29/1999 17:33'!
on: aCollection
	self initialize.
	super on: (aCollection species new: WindowSize * 2).! !

!DeflateStream methodsFor: 'initialization' stamp: 'ar 12/28/1999 17:34'!
on: aCollection from: firstIndex to: lastIndex
	"Not for DeflateStreams please"
	^self shouldNotImplement! !

!DeflateStream methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:00'!
goodMatchLength
	"Return the length that is considered to be a 'good' match.
	Higher values will result in better compression but take more time."
	^MaxMatch "Best compression"! !

!DeflateStream methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:00'!
hashChainLength
	"Return the max. number of hash chains to traverse.
	Higher values will result in better compression but take more time."
	^4096 "Best compression"! !

!DeflateStream methodsFor: 'accessing' stamp: 'ar 2/19/2004 00:34'!
next: bytes putAll: aCollection startingAt: startPos
	(startPos = 1 and:[bytes = aCollection size]) 
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startPos to: startPos + bytes - 1)! !

!DeflateStream methodsFor: 'accessing' stamp: 'ar 12/29/1999 17:33'!
nextPutAll: aCollection
	| start count max |
	aCollection species = collection species
		ifFalse:[
			aCollection do:[:ch| self nextPut: ch].
			^aCollection].
	start _ 1.
	count _ aCollection size.
	[count = 0] whileFalse:[
		position = writeLimit ifTrue:[self deflateBlock].
		max _ writeLimit - position.
		max > count ifTrue:[max _ count].
		collection replaceFrom: position+1
			to: position+max
			with: aCollection
			startingAt: start.
		start _ start + max.
		count _ count - max.
		position _ position + max].
	^aCollection! !

!DeflateStream methodsFor: 'accessing' stamp: 'ar 12/28/1999 17:35'!
pastEndPut: anObject
	self deflateBlock.
	^self nextPut: anObject! !

!DeflateStream methodsFor: 'private' stamp: 'ar 12/29/1999 17:50'!
moveContentsToFront
	"Move the contents of the receiver to the front"
	| delta |
	delta _ (blockPosition - WindowSize).
	delta <= 0 ifTrue:[^self].
	"Move collection"
	collection 
		replaceFrom: 1 
		to: collection size - delta 
		with: collection 
		startingAt: delta+1.
	position _ position - delta.
	"Move hash table entries"
	blockPosition _ blockPosition - delta.
	blockStart _ blockStart - delta.
	self updateHashTable: hashHead delta: delta.
	self updateHashTable: hashTail delta: delta.! !

!DeflateStream methodsFor: 'private' stamp: 'ar 2/2/2001 15:47'!
updateHashTable: table delta: delta
	| pos |
	<primitive: 'primitiveDeflateUpdateHashTable' module: 'ZipPlugin'>
	1 to: table size do:[:i|
		"Discard entries that are out of range"
		(pos _ table at: i) >= delta
			ifTrue:[table at: i put: pos - delta]
			ifFalse:[table at: i put: 0]].! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 2/28/2001 13:39'!
close
	self deflateBlock.
	self flushBlock: true.
	encoder close.! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 2/27/2001 13:23'!
finish
	"Finish pending operation. Do not close output stream."
	self deflateBlock.
	self flushBlock: true.
	encoder flush.! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 12/30/1999 00:40'!
initialize
	super initialize.
	literals _ ByteArray new: WindowSize.
	distances _ WordArray new: WindowSize.
	literalFreq _ WordArray new: MaxLiteralCodes.
	distanceFreq _ WordArray new: MaxDistCodes.
	self initializeNewBlock.
! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 12/29/1999 18:29'!
initializeNewBlock
	"Initialize the encoder for a new block of data"
	literalFreq atAllPut: 0.
	distanceFreq atAllPut: 0.
	literalFreq at: EndBlock+1 put: 1.
	litCount _ 0.
	matchCount _ 0.! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 2/24/2001 19:43'!
on: aCollectionOrStream
	crc _ 16rFFFFFFFF.
	crcPosition _ 1.
	bytesWritten _ 0.
	encoder _ ZipEncoder on: aCollectionOrStream.
	encoder isBinary
		ifTrue:[super on: ByteArray new]
		ifFalse:[super on: String new].
	self writeHeader.
! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'nk 2/17/2004 16:31'!
release
	"We're done with compression. Do some cleanup."
	literals _ distances _ literalFreq _ distanceFreq _ nil.
	self updateCrc.
	encoder flushBits.
	self writeFooter.! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'nk 2/17/2004 16:30'!
writeFooter
	"Write footer information if necessary"
	crc _ crc bitXor: 16rFFFFFFFF.! !

!ZipWriteStream methodsFor: 'initialization' stamp: 'ar 2/24/2001 19:44'!
writeHeader
	"Write header information if necessary"! !

!ZipWriteStream methodsFor: 'accessing' stamp: 'ar 2/24/2001 19:46'!
crc
	^crc! !

!ZipWriteStream methodsFor: 'accessing' stamp: 'ar 12/30/1999 00:37'!
encodedStream
	^encoder encodedStream! !

!ZipWriteStream methodsFor: 'accessing' stamp: 'ar 12/29/1999 18:32'!
forcedMethod
	"Return a symbol describing an enforced method or nil if the method should
	be chosen adaptively. Valid symbols are
		#stored	- store blocks (do not compress)
		#fixed	- use fixed huffman trees
		#dynamic	- use dynamic huffman trees."
	^nil! !

!ZipWriteStream methodsFor: 'deflating' stamp: 'ar 2/2/2001 15:47'!
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
	"^DeflatePlugin doPrimitive:#primitiveDeflateBlock"
	<primitive: 'primitiveDeflateBlock' module: 'ZipPlugin'>
	^super deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 01:55'!
dynamicBlockSizeFor: lTree and: dTree using: blTree and: blFreq
	"Compute the length for the current block using dynamic huffman trees"
	| bits index extra treeBits freq |
	bits _ 3 "block type" + 5 "literal codes length" + 5 "distance codes length".

	"Compute the # of bits for sending the bit length tree"
	treeBits _ 4. "Max index for bit length tree"
	index _ MaxBitLengthCodes.
	[index >= 4] whileTrue:[
		(index = 4 or:[(blFreq at: (BitLengthOrder at: index)+1) > 0])
			ifTrue:[treeBits _ treeBits + (index * 3).
					index _ -1]
			ifFalse:[index _ index - 1]].

	"Compute the # of bits for sending the literal/distance tree.
	Note: The frequency are already stored in the blTree"
	0 to: 15 do:[:i| "First, the non-repeating values"
		freq _ blFreq at: i+1.
		freq > 0 ifTrue:[treeBits _ treeBits + (freq * (blTree bitLengthAt: i))]].
	"Now the repeating values"
	(Repeat3To6 to: Repeat11To138) with: #(2 3 7) do:[:i :addl|
		freq _ blFreq at: i+1.
		freq > 0 ifTrue:[
			treeBits _ treeBits + (freq * ((blTree bitLengthAt: i) + addl "addl bits"))]].
	VerboseLevel > 1 ifTrue:[
		Transcript show:'['; print: treeBits; show:' bits for dynamic tree]'].
	bits _ bits + treeBits.

	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		freq _ literalFreq at: i+1.
		freq > 0 ifTrue:[bits _ bits + (freq * (lTree bitLengthAt: i))]].
	NumLiterals+1 to: lTree maxCode do:[:i| "encoding of match lengths"
		freq _ literalFreq at: i+1.
		extra _ ExtraLengthBits at: i-NumLiterals.
		freq > 0 ifTrue:[bits _ bits + (freq * ((lTree bitLengthAt: i) + extra))]].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		freq _ distanceFreq at: i+1.
		extra _ ExtraDistanceBits at: i+1.
		freq > 0 ifTrue:[bits _ bits + (freq * ((dTree bitLengthAt: i) + extra))]].

	^bits! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:55'!
scanBitLength: bitLength repeatCount: repeatCount into: anArray
	"Update the frequency for the aTree based on the given values"
	| count |
	count _ repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			anArray at: Repeat11To138+1 put: (anArray at: Repeat11To138+1) + 1.
			count _ (count - 138) max: 0].
		[count >= 3] whileTrue:[
			anArray at: Repeat3To10+1 put: (anArray at: Repeat3To10+1) + 1.
			count _ (count - 10) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	] ifFalse:[
		anArray at: bitLength+1 put: (anArray at: bitLength+1) + 1.
		count _ count - 1.
		[count >= 3] whileTrue:[
			anArray at: Repeat3To6+1 put: (anArray at: Repeat3To6+1) + 1.
			count _ (count - 6) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	].! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:55'!
scanBitLengths: bits into: anArray
	"Scan the trees and determine the frequency of the bit lengths.
	For repeating codes, emit a repeat count."
	| lastValue lastCount value |
	bits size = 0 ifTrue:[^self].
	lastValue _ bits at: 1.
	lastCount _ 1.
	2 to: bits size do:[:i|
		value _ bits at: i.
		value = lastValue 
			ifTrue:[lastCount _ lastCount + 1]
			ifFalse:[self scanBitLength: lastValue repeatCount: lastCount into: anArray.
					lastValue _ value.
					lastCount _ 1]].
	self scanBitLength: lastValue repeatCount: lastCount into: anArray.! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:40'!
sendBitLength: bitLength repeatCount: repeatCount tree: aTree
	"Send the given bitLength, repeating repeatCount times"
	| count |
	count _ repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			self sendBitLength: Repeat11To138 tree: aTree.
			encoder nextBits: 7 put: (count min: 138) - 11.
			count _ (count - 138) max: 0].
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To10 tree: aTree.
			encoder nextBits: 3 put: (count min: 10) - 3.
			count _ (count - 10) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	] ifFalse:[
		self sendBitLength: bitLength tree: aTree.
		count _ count - 1.
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To6 tree: aTree.
			encoder nextBits: 2 put: (count min: 6) - 3.
			count _ (count - 6) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	].! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:40'!
sendBitLength: bitLength tree: aTree
	"Send the given bitLength"
	encoder nextBits: (aTree bitLengthAt: bitLength) 
		put: (aTree codeAt: bitLength).! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:40'!
sendBitLengthTree: blTree
	"Send the bit length tree"
	| blIndex bitLength |
	MaxBitLengthCodes to: 4 by: -1 do:[:maxIndex|
		blIndex _ BitLengthOrder at: maxIndex.
		bitLength _ blIndex <= blTree maxCode 
			ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
		(maxIndex = 4 or:[bitLength > 0]) ifTrue:[
			encoder nextBits: 4 put: maxIndex - 4.
			1 to: maxIndex do:[:j|
				blIndex _ BitLengthOrder at: j.
				bitLength _ blIndex <= blTree maxCode 
					ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
				encoder nextBits: 3 put: bitLength].
			^self]].! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 00:48'!
sendCompressedBlock: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| sum |
	sum _ encoder
			sendBlock: (ReadStream on: literals from: 1 to: litCount)
			with: (ReadStream on: distances from: 1 to: litCount)
			with: litTree
			with: distTree.
	sum = (blockPosition - blockStart) ifFalse:[self error:'Wrong number of bytes'].! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/29/1999 18:19'!
sendDynamicBlock: blTree literalTree: lTree distanceTree: dTree bitLengths: bits
	"Send a block using dynamic huffman trees"
	self sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits.
	self sendCompressedBlock: lTree with: dTree.! !

!ZipWriteStream methodsFor: 'dynamic blocks' stamp: 'ar 12/30/1999 11:40'!
sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits
	"Send all the trees needed for dynamic huffman tree encoding"
	| lastValue lastCount value |
	encoder nextBits: 5 put: (lTree maxCode - 256).
	encoder nextBits: 5 put: (dTree maxCode).
	self sendBitLengthTree: blTree.
	bits size = 0 ifTrue:[^self].
	lastValue _ bits at: 1.
	lastCount _ 1.
	2 to: bits size do:[:i|
		value _ bits at: i.
		value = lastValue 
			ifTrue:[lastCount _ lastCount + 1]
			ifFalse:[self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
					lastValue _ value.
					lastCount _ 1]].
	self sendBitLength: lastValue repeatCount: lastCount tree: blTree.! !

!ZipWriteStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:05'!
encodeLiteral: lit
	"Encode the given literal"
	litCount _ litCount + 1.
	literals at: litCount put: lit.
	distances at: litCount put: 0.
	literalFreq at: lit+1 put: (literalFreq at: lit+1) + 1.
	^self shouldFlush! !

!ZipWriteStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:05'!
encodeMatch: length distance: dist
	"Encode the given match of length length starting at dist bytes ahead"
	| literal distance |
	dist > 0 
		ifFalse:[^self error:'Distance must be positive'].
	length < MinMatch 
		ifTrue:[^self error:'Match length must be at least ', MinMatch printString].
	litCount _ litCount + 1.
	matchCount _ matchCount + 1.
	literals at: litCount put: length - MinMatch.
	distances at: litCount put: dist.
	literal _ (MatchLengthCodes at: length - MinMatch + 1).
	literalFreq at: literal+1 put: (literalFreq at: literal+1) + 1.
	dist < 257
		ifTrue:[distance _ DistanceCodes at: dist]
		ifFalse:[distance _ DistanceCodes at: 257 + (dist - 1 bitShift: -7)].
	distanceFreq at: distance+1 put: (distanceFreq at: distance+1) + 1.
	^self shouldFlush! !

!ZipWriteStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:08'!
flushBlock
	^self flushBlock: false! !

!ZipWriteStream methodsFor: 'encoding' stamp: 'jmv 3/13/2012 12:58'!
flushBlock: lastBlock
	"Send the current block"
	| lastFlag bitsRequired method bitsSent
	storedLength fixedLength dynamicLength 
	blTree lTree dTree blBits blFreq |

	lastFlag _ lastBlock ifTrue:[1] ifFalse:[0].

	"Compute the literal/length and distance tree"
	lTree _ ZipEncoderTree buildTreeFrom: literalFreq maxDepth: MaxBits.
	dTree _ ZipEncoderTree buildTreeFrom: distanceFreq maxDepth: MaxBits.

	"Compute the bit length tree"
	blBits _ lTree bitLengths, dTree bitLengths.
	blFreq _ WordArray new: MaxBitLengthCodes.
	self scanBitLengths: blBits into: blFreq.
	blTree _ ZipEncoderTree buildTreeFrom: blFreq maxDepth: MaxBitLengthBits.

	"Compute the bit length for the current block.
	Note: Most of this could be computed on the fly but it's getting
	really ugly in this case so we do it afterwards."
	storedLength _ self storedBlockSize.
	fixedLength _ self fixedBlockSizeFor: lTree and: dTree.
	dynamicLength _ self dynamicBlockSizeFor: lTree and: dTree 
							using: blTree and: blFreq.
	VerboseLevel > 1 ifTrue:[
		Transcript newLine; show:'Block sizes (S/F/D):';
			space; print: storedLength // 8; 
			nextPut:$/; print: fixedLength // 8; 
			nextPut:$/; print: dynamicLength // 8; space; endEntry].

	"Check which method to use"
	method _ self forcedMethod.
	method ifNil: [ 
		method _ (storedLength < fixedLength and:[storedLength < dynamicLength]) 
			ifTrue:[#stored]
			ifFalse:[fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]]].
	(method == #stored and:[blockStart < 0]) ifTrue:[
		"Cannot use #stored if the block is not available"
		method _ fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]].

	bitsSent _ encoder bitPosition. "# of bits sent before this block"
	bitsRequired _ nil.

	(method == #stored) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'S'].
		bitsRequired _ storedLength.
		encoder nextBits: 3 put: StoredBlock << 1 + lastFlag.
		self sendStoredBlock].

	(method == #fixed) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'F'].
		bitsRequired _ fixedLength.
		encoder nextBits: 3 put: FixedBlock << 1 + lastFlag.
		self sendFixedBlock].

	(method == #dynamic) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'D'].
		bitsRequired _ dynamicLength.
		encoder nextBits: 3 put: DynamicBlock << 1 + lastFlag.
		self sendDynamicBlock: blTree 
			literalTree: lTree 
			distanceTree: dTree 
			bitLengths: blBits].

	bitsRequired = (encoder bitPosition - bitsSent)
		ifFalse:[self error:'Bits size mismatch'].

	lastBlock 
		ifTrue:[self release]
		ifFalse:[self initializeNewBlock].! !

!ZipWriteStream methodsFor: 'encoding' stamp: 'ar 12/29/1999 18:08'!
shouldFlush
	"Check if we should flush the current block.
	Flushing can be useful if the input characteristics change."
	| nLits |
	litCount = literals size ifTrue:[^true]. "We *must* flush"
	(litCount bitAnd: 16rFFF) = 0 ifFalse:[^false]. "Only check every N kbytes"
	matchCount * 10 <= litCount ifTrue:[
		"This is basically random data. 
		There is no need to flush early since the overhead
		for encoding the trees will add to the overall size"
		^false].
	"Try to adapt to the input data.
	We flush if the ratio between matches and literals
	changes beyound a certain threshold"
	nLits _ litCount - matchCount.
	nLits <= matchCount ifTrue:[^false]. "whow!! so many matches"
	^nLits * 4 <= matchCount! !

!ZipWriteStream methodsFor: 'fixed blocks' stamp: 'ar 12/29/1999 18:18'!
fixedBlockSizeFor: lTree and: dTree
	"Compute the length for the current block using fixed huffman trees"
	| bits extra |
	bits _ 3 "block type".
	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		bits _ bits + ((literalFreq at: i+1) * (FixedLiteralTree bitLengthAt: i))].
	NumLiterals+1 to: lTree maxCode+1 do:[:i| "Encoding of match lengths"
		extra _ ExtraLengthBits at: i-NumLiterals.
		bits _ bits + ((literalFreq at: i+1) * ((FixedLiteralTree bitLengthAt: i) + extra))].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		extra _ ExtraDistanceBits at: i+1.
		bits _ bits + ((distanceFreq at: i+1) * ((FixedDistanceTree bitLengthAt: i) + extra))].

	^bits! !

!ZipWriteStream methodsFor: 'fixed blocks' stamp: 'ar 12/29/1999 18:18'!
sendFixedBlock
	"Send a block using fixed huffman trees"
	self sendCompressedBlock: FixedLiteralTree with: FixedDistanceTree.! !

!ZipWriteStream methodsFor: 'private' stamp: 'ar 2/24/2001 19:45'!
moveContentsToFront
	"Need to update crc here"
	self updateCrc.
	super moveContentsToFront.
	crcPosition _ position + 1.! !

!ZipWriteStream methodsFor: 'private' stamp: 'ar 2/24/2001 19:45'!
updateCrc
	crcPosition <= position ifTrue:[
		bytesWritten _ bytesWritten + position - crcPosition + 1.
		crc _ self updateCrc: crc from: crcPosition to: position in: collection.
		crcPosition _ position + 1].! !

!ZipWriteStream methodsFor: 'private' stamp: 'nk 2/17/2004 16:51'!
updateCrc: oldCrc from: start to: stop in: aCollection
	^self class updateCrc: oldCrc from: start to: stop in: aCollection! !

!ZipWriteStream methodsFor: 'stored blocks' stamp: 'ar 1/2/2000 16:36'!
sendStoredBlock
	"Send an uncompressed block"
	| inBytes |
	inBytes _ blockPosition - blockStart.
	encoder flushBits. "Skip to byte boundary"
	encoder nextBits: 16 put: inBytes.
	encoder nextBits: 16 put: (inBytes bitXor: 16rFFFF).
	encoder flushBits.
	1 to: inBytes do:[:i|
		encoder nextBytePut: (collection byteAt: blockStart+i)].! !

!ZipWriteStream methodsFor: 'stored blocks' stamp: 'ar 12/30/1999 00:42'!
storedBlockSize
	"Compute the length for the current block when stored as is"
	^3 "block type bits" 
		+ (8 - (encoder bitPosition + 3 bitAnd: 7) bitAnd: 7)"skipped bits to byte boundary"
			+ 32 "byte length + chksum" 
				+ (blockPosition - blockStart * 8) "actual data bits".! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/30/1999 15:55'!
baseDistance
	^BaseDistance! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/30/1999 15:55'!
baseLength
	^BaseLength! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 2/24/2001 19:42'!
crcTable
	^CrcTable! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:42'!
distanceCodes
	^DistanceCodes! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/30/1999 15:55'!
extraDistanceBits
	^ExtraDistanceBits! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/30/1999 15:55'!
extraLengthBits
	^ExtraLengthBits! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:42'!
matchLengthCodes
	^MatchLengthCodes! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:53'!
maxDistanceCodes
	^MaxDistCodes! !

!ZipWriteStream class methodsFor: 'accessing' stamp: 'ar 12/29/1999 20:53'!
maxLiteralCodes
	^MaxLiteralCodes! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 7/17/2017 15:51:45'!
compressAndDecompress: aFile using: tempName stats: stats
	| fileSize tempFile result |
	aFile
		ifNil: [^ nil].
	fileSize _ aFile size.
	(fileSize < 1"00000" "or:[fileSize > 1000000]") ifTrue:[aFile close. ^nil].
	Transcript newLine; show:'Testing ', aFile name,' ... '.
	tempFile _ StandardFileStream new open: tempName forWrite: true.
	'Compressing ', aFile name,'...' displayProgressAt: Sensor mousePoint
		from: 1 to: aFile size during:[:barBlock|
			result _ self regressionCompress: aFile into: tempFile notifiying: barBlock stats: stats].
	result ifTrue:[
		'Validating ', aFile name,'...' displayProgressAt: Sensor mousePoint
			from: 0 to: aFile size during:[:barBlock|
				result _ self regressionDecompress: aFile from: tempFile notifying: barBlock stats: stats]].
	aFile close.
	tempFile close.
	(DirectoryEntry smalltalkImageDirectory // tempName) delete.
	result ~~ false ifTrue:[
		Transcript show:' ok (', (result * 100 truncateTo: 0.01) printString,')'].
	^result! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'pb 5/25/2016 01:30'!
logProblem: reason for: aFile

	'problems.log' asFileEntry appendStreamDo: [ :errFile |
		errFile
			newLine;
			nextPutAll: aFile name;
			newLine;
			nextPutAll: reason ].
	Transcript show:' failed (', reason,')'.
	aFile close.
	^false! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 1/9/2014 23:32'!
printRegressionStats: stats from: fd
	| raw compressed numFiles |
	raw _ stats at: #rawSize ifAbsent:[0].
	raw = 0 ifTrue:[^self].
	compressed _ stats at: #compressedSize ifAbsent:[0].
	numFiles _ stats at: #numFiles ifAbsent:[0].
	Transcript newLine; nextPutAll: fd pathName.
	Transcript newLine; tab; nextPutAll:'Files compressed: ', numFiles printStringWithCommas.
	Transcript newLine; tab; nextPutAll:'Bytes compressed: ', raw printStringWithCommas.
	Transcript newLine; tab; nextPutAll:'Avg. compression ratio: ';
		print: ((compressed / raw asFloat * 100.0) truncateTo: 0.01).
	Transcript endEntry.! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 7/17/2017 15:52:09'!
regressionCompress: aFile into: tempFile notifiying: progressBarBlock stats: stats
	"Compress aFile into tempFile"
	| zip encoded buffer |
	aFile binary.
	aFile position: 0.
	tempFile binary.
	buffer _ ByteArray new: 4096.
	zip _ self on: (ByteArray new: 10000).
	encoded _ zip encodedStream.
	[aFile atEnd] whileFalse:[
		progressBarBlock value: aFile position.
		zip nextPutAll: (aFile nextInto: buffer).
		encoded position > 0 ifTrue:[
			tempFile nextPutAll: encoded contents.
			encoded position: 0]].
	zip close.
	tempFile nextPutAll: encoded contents.
	^true! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 7/17/2017 15:52:23'!
regressionDecompress: aFile from: tempFile notifying: progressBarBlock stats: stats
	"Validate aFile as decompressed from tempFile"
	| unzip rawSize compressedSize buffer1 buffer2 |
	rawSize _ aFile size.
	compressedSize _ tempFile size.
	aFile ascii.
	aFile position: 0.
	tempFile ascii.
	tempFile position: 0.
	buffer1 _ ByteArray new: 4096.
	buffer2 _ buffer1 copy.
	unzip _ FastInflateStream on: tempFile.
	[aFile atEnd] whileFalse:[
		progressBarBlock value: aFile position.
		buffer1 _ aFile nextInto: buffer1.
		buffer2 _ unzip nextInto: buffer2.
		buffer1 = buffer2
			ifFalse:[^self logProblem: 'contents ' for: aFile].
	].
	unzip next ifNotNil: [ ^self logProblem: 'EOF' for: aFile].
	stats at: #rawSize put:
		(stats at: #rawSize ifAbsent:[0]) + rawSize.
	stats at: #compressedSize put:
		(stats at: #compressedSize ifAbsent:[0]) + compressedSize.
	^compressedSize asFloat / rawSize asFloat.! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'pb 5/25/2016 01:16'!
regressionTest 
	"
	ZipWriteStream regressionTest
	"
	"Compress and decompress everything we can 
	find to validate that compression works as expected."
	self regressionTestFrom: DirectoryEntry smalltalkImageDirectory! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'pb 5/25/2016 01:16'!
regressionTestFrom: fd
	"ZipWriteStream regressionTestFrom: FileDirectory currentDirectory"
	"ZipWriteStream regressionTestFrom: (FileDirectory on:'')"
	"ZipWriteStream regressionTestFrom: (FileDirectory on:'C:')"
	| stats entry |
	Transcript clear.
	stats _ Dictionary new.
	entry _ (DirectoryEntry smalltalkImageDirectory // '$$sqcompress$$').
	entry exists ifTrue: [ entry delete ].
	self regressionTestFrom: fd using: entry pathName stats: stats.! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 11/3/2016 10:22:29'!
regressionTestFrom: fd using: tempName stats: stats
	| files file fullName |
	files _ fd fileNames asArray sort.
	files do: [ :fName |
		file _ nil.
		fullName _ (fd // fName) pathName.
		fullName = tempName ifFalse: [
			file _ StandardFileStream new
				open: fullName
				forWrite: false ].
		self
			compressAndDecompress: file
			using: tempName
			stats: stats ].
	stats
		at: #numFiles
		put:
			(stats
				at: #numFiles
				ifAbsent: [ 0 ]) + files size.
	files _ nil.
	self
		printRegressionStats: stats
		from: fd.
	fd directoryNames asArray sort do: [ :dName |
		self
			regressionTestFrom: fd / dName
			using: tempName
			stats: stats ].! !

!ZipWriteStream class methodsFor: 'class initialization' stamp: 'ar 5/18/2003 19:10'!
initialize
	"ZipWriteStream initialize"
	VerboseLevel := 0.
	self initializeCrcTable.! !

!ZipWriteStream class methodsFor: 'class initialization' stamp: 'ar 2/24/2001 19:42'!
initializeCrcTable
	"ZipWriteStream initialize"
	CrcTable _ #(16r00000000 16r77073096 16rEE0E612C 16r990951BA 16r076DC419
  16r706AF48F 16rE963A535 16r9E6495A3 16r0EDB8832 16r79DCB8A4
  16rE0D5E91E 16r97D2D988 16r09B64C2B 16r7EB17CBD 16rE7B82D07
  16r90BF1D91 16r1DB71064 16r6AB020F2 16rF3B97148 16r84BE41DE
  16r1ADAD47D 16r6DDDE4EB 16rF4D4B551 16r83D385C7 16r136C9856
  16r646BA8C0 16rFD62F97A 16r8A65C9EC 16r14015C4F 16r63066CD9
  16rFA0F3D63 16r8D080DF5 16r3B6E20C8 16r4C69105E 16rD56041E4
  16rA2677172 16r3C03E4D1 16r4B04D447 16rD20D85FD 16rA50AB56B
  16r35B5A8FA 16r42B2986C 16rDBBBC9D6 16rACBCF940 16r32D86CE3
  16r45DF5C75 16rDCD60DCF 16rABD13D59 16r26D930AC 16r51DE003A
  16rC8D75180 16rBFD06116 16r21B4F4B5 16r56B3C423 16rCFBA9599
  16rB8BDA50F 16r2802B89E 16r5F058808 16rC60CD9B2 16rB10BE924
  16r2F6F7C87 16r58684C11 16rC1611DAB 16rB6662D3D 16r76DC4190
  16r01DB7106 16r98D220BC 16rEFD5102A 16r71B18589 16r06B6B51F
  16r9FBFE4A5 16rE8B8D433 16r7807C9A2 16r0F00F934 16r9609A88E
  16rE10E9818 16r7F6A0DBB 16r086D3D2D 16r91646C97 16rE6635C01
  16r6B6B51F4 16r1C6C6162 16r856530D8 16rF262004E 16r6C0695ED
  16r1B01A57B 16r8208F4C1 16rF50FC457 16r65B0D9C6 16r12B7E950
  16r8BBEB8EA 16rFCB9887C 16r62DD1DDF 16r15DA2D49 16r8CD37CF3
  16rFBD44C65 16r4DB26158 16r3AB551CE 16rA3BC0074 16rD4BB30E2
  16r4ADFA541 16r3DD895D7 16rA4D1C46D 16rD3D6F4FB 16r4369E96A
  16r346ED9FC 16rAD678846 16rDA60B8D0 16r44042D73 16r33031DE5
  16rAA0A4C5F 16rDD0D7CC9 16r5005713C 16r270241AA 16rBE0B1010
  16rC90C2086 16r5768B525 16r206F85B3 16rB966D409 16rCE61E49F
  16r5EDEF90E 16r29D9C998 16rB0D09822 16rC7D7A8B4 16r59B33D17
  16r2EB40D81 16rB7BD5C3B 16rC0BA6CAD 16rEDB88320 16r9ABFB3B6
  16r03B6E20C 16r74B1D29A 16rEAD54739 16r9DD277AF 16r04DB2615
  16r73DC1683 16rE3630B12 16r94643B84 16r0D6D6A3E 16r7A6A5AA8
  16rE40ECF0B 16r9309FF9D 16r0A00AE27 16r7D079EB1 16rF00F9344
  16r8708A3D2 16r1E01F268 16r6906C2FE 16rF762575D 16r806567CB
  16r196C3671 16r6E6B06E7 16rFED41B76 16r89D32BE0 16r10DA7A5A
  16r67DD4ACC 16rF9B9DF6F 16r8EBEEFF9 16r17B7BE43 16r60B08ED5
  16rD6D6A3E8 16rA1D1937E 16r38D8C2C4 16r4FDFF252 16rD1BB67F1
  16rA6BC5767 16r3FB506DD 16r48B2364B 16rD80D2BDA 16rAF0A1B4C
  16r36034AF6 16r41047A60 16rDF60EFC3 16rA867DF55 16r316E8EEF
  16r4669BE79 16rCB61B38C 16rBC66831A 16r256FD2A0 16r5268E236
  16rCC0C7795 16rBB0B4703 16r220216B9 16r5505262F 16rC5BA3BBE
  16rB2BD0B28 16r2BB45A92 16r5CB36A04 16rC2D7FFA7 16rB5D0CF31
  16r2CD99E8B 16r5BDEAE1D 16r9B64C2B0 16rEC63F226 16r756AA39C
  16r026D930A 16r9C0906A9 16rEB0E363F 16r72076785 16r05005713
  16r95BF4A82 16rE2B87A14 16r7BB12BAE 16r0CB61B38 16r92D28E9B
  16rE5D5BE0D 16r7CDCEFB7 16r0BDBDF21 16r86D3D2D4 16rF1D4E242
  16r68DDB3F8 16r1FDA836E 16r81BE16CD 16rF6B9265B 16r6FB077E1
  16r18B74777 16r88085AE6 16rFF0F6A70 16r66063BCA 16r11010B5C
  16r8F659EFF 16rF862AE69 16r616BFFD3 16r166CCF45 16rA00AE278
  16rD70DD2EE 16r4E048354 16r3903B3C2 16rA7672661 16rD06016F7
  16r4969474D 16r3E6E77DB 16rAED16A4A 16rD9D65ADC 16r40DF0B66
  16r37D83BF0 16rA9BCAE53 16rDEBB9EC5 16r47B2CF7F 16r30B5FFE9
  16rBDBDF21C 16rCABAC28A 16r53B39330 16r24B4A3A6 16rBAD03605
  16rCDD70693 16r54DE5729 16r23D967BF 16rB3667A2E 16rC4614AB8
  16r5D681B02 16r2A6F2B94 16rB40BBE37 16rC30C8EA1 16r5A05DF1B
  16r2D02EF8D
).! !

!ZipWriteStream class methodsFor: 'crc' stamp: 'nk 2/17/2004 16:50'!
updateCrc: oldCrc from: start to: stop in: aCollection
	| newCrc |
	<primitive: 'primitiveUpdateGZipCrc32' module: 'ZipPlugin'>
	newCrc _ oldCrc.
	start to: stop do:[:i|
		newCrc _ (CrcTable at: ((newCrc bitXor: (aCollection byteAt: i)) 
				bitAnd: 255) + 1) bitXor: (newCrc bitShift: -8).
	].
	^newCrc! !

!GZipWriteStream methodsFor: 'initialization' stamp: 'nk 2/19/2004 08:31'!
writeFooter
	"Write some footer information for the crc"
	super writeFooter.
	0 to: 3 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].
	0 to: 3 do:[:i| encoder nextBytePut: (bytesWritten >> (i*8) bitAnd: 255)].! !

!GZipWriteStream methodsFor: 'initialization' stamp: 'ar 12/30/1999 11:41'!
writeHeader
	"Write the GZip header"
	encoder nextBits: 16 put: GZipMagic.
	encoder nextBits: 8 put: GZipDeflated.
	encoder nextBits: 8 put: 0. "No flags"
	encoder nextBits: 32 put: 0. "no time stamp"
	encoder nextBits: 8 put: 0. "No extra flags"
	encoder nextBits: 8 put: 0. "No OS type"
! !

!GZipWriteStream class methodsFor: 'file list services' stamp: 'pb 2/26/2020 13:44:48'!
compressFile: aFileEntry
	"Create a compressed file from the file of the given name"

	(aFileEntry readStream) compressFile! !

!GZipWriteStream class methodsFor: 'file list services' stamp: 'pb 2/26/2020 13:51:06'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that are serviced by us."
	"Don't offer to compress already-compressed files
	sjc 3-May 2003-added jpeg extension"

	^({ 'gz' . 'sar' . 'zip' . 'gif' . 'jpg' . 'jpeg'. 'pr'. 'png' } includes: suffix)
		ifTrue: [ #() ]
		ifFalse: [ { self serviceCompressFile } ]
! !

!GZipWriteStream class methodsFor: 'file list services' stamp: 'HAW 1/2/2019 18:38:24'!
serviceCompressFile

	^ (SimpleServiceEntry 
		provider: self 
		label: 'compress file'
		selector: #compressFile:
		description: 'compress file using gzip compression, making a new file'
		icon: #goBottomIcon) triggerFileListChanged! !

!ZLibWriteStream methodsFor: 'initialization' stamp: 'nk 2/17/2004 16:28'!
on: aCollectionOrStream
	super on: aCollectionOrStream.
	crc _ 1.! !

!ZLibWriteStream methodsFor: 'initialization' stamp: 'nk 2/17/2004 16:53'!
updateCrc: adler from: start to: stop in: aCollection
	"Update crc using the Adler32 checksum technique from RFC1950"
	^self class updateAdler32: adler from:  start to:  stop in: aCollection! !

!ZLibWriteStream methodsFor: 'initialization' stamp: 'ar 2/29/2004 04:40'!
writeFooter
	"Store the Adler32 checksum as the last 4 bytes."
	3 to: 0 by: -1 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].! !

!ZLibWriteStream methodsFor: 'initialization' stamp: 'nk 2/17/2004 18:23'!
writeHeader
	"Write header information"
	encoder nextBits: 8 put: 120. "deflate method with 15bit window size"
	encoder nextBits: 8 put: 94. "checksum; no preset; fast (flevel=1) compression"! !

!ZLibWriteStream class methodsFor: 'crc' stamp: 'ar 4/14/2010 19:50'!
updateAdler32: adler from: start to: stop in: aCollection
	"Update crc using the Adler32 checksum technique from RFC1950"
"
        unsigned long s1 = adler & 0xffff;
        unsigned long s2 = (adler >> 16) & 0xffff;
        int n;

        for (n = 0; n < len; n++) {
          s1 = (s1 + buf[n]) % BASE;
          s2 = (s2 + s1)     % BASE;
        }
        return (s2 << 16) + s1;
"
	| s1 s2 |
	<primitive: 'primitiveUpdateAdler32' module: 'ZipPlugin'>
	s1 := adler bitAnd: 16rFFFF.
	s2 := (adler bitShift: -16) bitAnd: 16rFFFF.
	start to: stop do: [ :n | | b |
		b := aCollection byteAt: n.
		s1 := (s1 + b) \\ 65521.
		s2 := (s2 + s1) \\ 65521. ].
	^(s2 bitShift: 16) + s1! !

!ZipEncoder methodsFor: 'accessing' stamp: 'ar 12/30/1999 00:45'!
bitPosition
	^encodedStream position + position * 8 + bitPosition.! !

!ZipEncoder methodsFor: 'accessing' stamp: 'ar 12/30/1999 00:37'!
encodedStream
	^encodedStream! !

!ZipEncoder methodsFor: 'accessing' stamp: 'ar 1/2/2000 16:34'!
nextBits: nBits put: value
	"Store a value of nBits"
	"self assert:[value >= 0 and:[(1 bitShift: nBits) > value]]."
	bitBuffer _ bitBuffer bitOr: (value bitShift: bitPosition).
	bitPosition _ bitPosition + nBits.
	[bitPosition >= 8] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer _ bitBuffer bitShift: -8.
		bitPosition _ bitPosition - 8].! !

!ZipEncoder methodsFor: 'accessing' stamp: 'ar 1/2/2000 16:34'!
nextBytePut: anObject 
	"Primitive. Insert the argument at the next position in the Stream
	represented by the receiver. Fail if the collection of this stream is not an
	Array or a String. Fail if the stream is positioned at its end, or if the
	position is out of bounds in the collection. Fail if the argument is not
	of the right type for the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 66>
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: 
			[position _ position + 1.
			^collection byteAt: position put: anObject]! !

!ZipEncoder methodsFor: 'initialization' stamp: 'sd 1/30/2004 15:24'!
close
	self flush.
	encodedStream close.! !

!ZipEncoder methodsFor: 'initialization' stamp: 'ar 12/30/1999 15:51'!
commit
	encodedStream next: position putAll: collection.
	position _ readLimit _ 0.! !

!ZipEncoder methodsFor: 'initialization' stamp: 'ar 12/30/1999 15:51'!
flush
	self flushBits.
	self commit.! !

!ZipEncoder methodsFor: 'initialization' stamp: 'ar 1/2/2000 16:35'!
flushBits
	"Flush currently unsent bits"
	[bitPosition > 0] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer _ bitBuffer bitShift: -8.
		bitPosition _ bitPosition - 8].
	bitPosition _ 0.! !

!ZipEncoder methodsFor: 'initialization' stamp: 'jmv 2/17/2010 22:09'!
on: aCollectionOrStream
	(aCollectionOrStream is: #Stream)
		ifTrue:[encodedStream _ aCollectionOrStream]
		ifFalse:[	encodedStream _ WriteStream on: aCollectionOrStream].
	encodedStream isBinary
		ifTrue:[super on: (ByteArray new: 4096)]
		ifFalse:[super on: (String new: 4096)].
	bitPosition _ bitBuffer _ 0.! !

!ZipEncoder methodsFor: 'private' stamp: 'ar 1/2/2000 16:38'!
pastEndPut: anObject
	"Flush the current buffer and store the new object at the beginning"
	self commit.
	^self nextBytePut: anObject asInteger! !

!ZipEncoder methodsFor: 'private' stamp: 'ar 2/2/2001 15:47'!
privateSendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| lit dist code extra sum |
	<primitive: 'primitiveZipSendBlock' module: 'ZipPlugin'>
	sum _ 0.
	[lit _ literalStream next.
	dist _ distanceStream next.
	lit == nil] whileFalse:[
		dist = 0 ifTrue:["lit is a literal"
			sum _ sum + 1.
			self nextBits: (litTree bitLengthAt: lit)
				put: (litTree codeAt: lit).
		] ifFalse:["lit is match length"
			sum _ sum + lit + MinMatch.
			code _ (MatchLengthCodes at: lit + 1).
			self nextBits: (litTree bitLengthAt: code)
				put: (litTree codeAt: code).
			extra _ ExtraLengthBits at: code-NumLiterals.
			extra = 0 ifFalse:[
				lit _ lit - (BaseLength at: code-NumLiterals).
				self nextBits: extra put: lit.
			].
			dist _ dist - 1.
			dist < 256
				ifTrue:[code _ DistanceCodes at: dist + 1]
				ifFalse:[code _ DistanceCodes at: 257 + (dist bitShift: -7)].
			"self assert:[code < MaxDistCodes]."
			self nextBits: (distTree bitLengthAt: code)
				put: (distTree codeAt: code).
			extra _ ExtraDistanceBits at: code+1.
			extra = 0 ifFalse:[
				dist _ dist - (BaseDistance at: code+1).
				self nextBits: extra put: dist.
			].
		].
	].
	^sum! !

!ZipEncoder methodsFor: 'block encoding' stamp: 'ar 12/30/1999 18:39'!
sendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| result |
	result _ 0.
	[literalStream atEnd] whileFalse:[
		result _ result + (self privateSendBlock: literalStream
						with: distanceStream with: litTree with: distTree).
		self commit.
	].
	self nextBits: (litTree bitLengthAt: EndBlock) put: (litTree codeAt: EndBlock).
	^result! !

!CRCError methodsFor: 'as yet unclassified' stamp: 'nk 3/7/2004 15:56'!
isResumable
	^true! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:41'!
ascii

	self bufferStream ascii! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:42'!
binary

	self bufferStream binary! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:43'!
bufferStream

	^bufferStream ifNil: [bufferStream _ RWBinaryOrTextStream on: (ByteArray new: 5000)].
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 09:26'!
close
	
	"we don't want to until user is really done"
	

! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:26'!
closed

	^false! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 19:18'!
fileOutClass: extraClass andObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| class srefStream |

	self timeStamp.

	extraClass ifNotNil: [
		class _ extraClass.	"A specific class the user wants written"
		class sharedPools size > 0 ifTrue: [
			class shouldFileOutPools ifTrue: [class fileOutSharedPoolsOn: self]
		].
		class fileOutOn: self moveSource: false toFile: 0
	].

	"Append the object's raw data"
	srefStream _ SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me - well it thinks it does, anyway"
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 19:16'!
flushBuffer

	| data |
	bufferStream ifNil: [^self].
	data _ bufferStream contents asByteArray.
	gZipStream nextPutAll: data.
	positionThusFar _ positionThusFar + data size.
	bufferStream _ nil.
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:12'!
newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self bufferStream newLine! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 09:23'!
next

	^self bufferStream next! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:42'!
nextChunkPut: aString

	self bufferStream nextChunkPut: aString! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:41'!
nextPut: aByte

	^self bufferStream nextPut: aByte
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:24'!
nextPutAll: aString

	^aString writeOnGZIPByteStream: self
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:41'!
nextPutAllBytes: aString

	^self bufferStream nextPutAll: aString
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/2000 11:36'!
nextPutAllWordArray: aWordArray

	| ba hackwa hackba blt rowsAtATime sourceOrigin rowsRemaining |

	self flag: #bob.		"do we need to be concerned by bytesPerElement??"
	ba _ nil.
	rowsAtATime _ 2000.		"or 8000 bytes"
	hackwa _ Form new hackBits: aWordArray.
	sourceOrigin _ 0@0.
	[(rowsRemaining _ hackwa height - sourceOrigin y) > 0] whileTrue: [
		rowsAtATime _ rowsAtATime min: rowsRemaining.
		(ba isNil or: [ba size ~= (rowsAtATime * 4)]) ifTrue: [
			ba _ ByteArray new: rowsAtATime * 4.
			hackba _ Form new hackBits: ba.
			blt _ (BitBlt toForm: hackba) sourceForm: hackwa.
		].
		blt 
			combinationRule: Form over;
			sourceOrigin: sourceOrigin;
			destX: 0 destY: 0 width: 4 height: rowsAtATime;
			copyBits.
		self bufferStream nextPutAll: ba.
		self flushBuffer.
		sourceOrigin _ sourceOrigin x @ (sourceOrigin y + rowsAtATime).
	].
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'jmv 2/26/2016 16:19'!
nextSignedInt32Put: int32 bigEndian: bigEndian

	^self bufferStream nextSignedInt32Put: int32 bigEndian: bigEndian! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/22/2000 18:42'!
nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length _ s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s.
	^s! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'ar 5/17/2001 19:08'!
nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	^self nextPutAllWordArray: aCollection! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/2000 11:22'!
originalContents

	^''		"used only to determine if we are byte-structured"! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 19:12'!
position

	^self bufferStream position + positionThusFar! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 19:13'!
reallyClose

	self flushBuffer.
	gZipStream close.
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:30'!
reopen

	"ignore"! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 08:30'!
setToEnd

	"ignore"! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 09:21'!
skip: aNumber

	^self bufferStream skip: aNumber
! !

!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:35'!
timeStamp
	"Append the current time to the receiver as a String."
	self bufferStream nextChunkPut:	"double string quotes and !!s"
		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.
	self bufferStream newLine! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/25/1999 19:41'!
bitLength
	^bitLength ifNil:[0]! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/30/1999 14:28'!
code
	^code ifNil:[0]! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/25/1999 21:51'!
code: aCode
	self assert:[aCode >= 0 and:[(1 bitShift: bitLength) > aCode]].
	code _ aCode.! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:36'!
frequency
	^frequency! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/28/1999 00:56'!
frequency: aNumber
	frequency _ aNumber! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/26/1999 10:44'!
height
	^height! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:43'!
left
	^left! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/25/1999 20:06'!
left: aNode
	aNode parent: self.
	left _ aNode.! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:43'!
parent
	^parent! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:43'!
parent: aNode
	parent _ aNode! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:43'!
right
	^right! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/25/1999 20:06'!
right: aNode
	aNode parent: self.
	right _ aNode.! !

!ZipEncoderNode methodsFor: 'accessing' stamp: 'ar 12/24/1999 23:43'!
value
	^value! !

!ZipEncoderNode methodsFor: 'private' stamp: 'ar 12/26/1999 10:45'!
computeHeight
	^self isLeaf
		ifTrue:[height _ 0]
		ifFalse:[height _ (left computeHeight max: right computeHeight) + 1].! !

!ZipEncoderNode methodsFor: 'private' stamp: 'ar 12/25/1999 18:14'!
leafNodes
	self isLeaf
		ifTrue:[^Array with: self]
		ifFalse:[^left leafNodes, right leafNodes]! !

!ZipEncoderNode methodsFor: 'private' stamp: 'ar 12/26/1999 12:05'!
setBitLengthTo: bl
	bitLength _ bl! !

!ZipEncoderNode methodsFor: 'private' stamp: 'ar 12/26/1999 10:46'!
setValue: v frequency: f height: h
	value _ v.
	frequency _ f.
	height _ h.! !

!ZipEncoderNode methodsFor: 'encoding' stamp: 'jmv 3/2/2010 11:31'!
encodeBitLength: blCounts from: aTree
	| index |
	"Note: If bitLength is not nil then the tree must be broken"
	bitLength ifNotNil: [ self error:'Huffman tree is broken'].
	bitLength _ parent
		ifNil: [0]
		ifNotNil: [ parent bitLength + 1].
	self isLeaf ifTrue:[
		index _ bitLength + 1.
		blCounts at: index put: (blCounts at: index) + 1.
	] ifFalse:[
		left encodeBitLength: blCounts from: aTree.
		right encodeBitLength: blCounts from: aTree.
	]! !

!ZipEncoderNode methodsFor: 'encoding' stamp: 'ar 12/27/1999 14:27'!
rotateToHeight: maxHeight
	"Rotate the tree to achieve maxHeight depth"
	| newParent |
	height < 4 ifTrue:[^self].
	self left: (left rotateToHeight: maxHeight-1).
	self right: (right rotateToHeight: maxHeight-1).
	height _ (left height max: right height) + 1.
	height <= maxHeight ifTrue:[^self].
	(left height - right height) abs <= 2 ifTrue:[^self].
	left height < right height ifTrue:[
		right right height >= right left height ifTrue:[
			newParent _ right.
			self right: newParent left.
			newParent left: self.
		] ifFalse:[
			newParent _ right left.
			right left: newParent right.
			newParent right: right.
			self right: newParent left.
			newParent left: self.
		].
	] ifFalse:[
		left left height >= left right height ifTrue:[
			newParent _ left.
			self left: newParent right.
			newParent right: self.
		] ifFalse:[
			newParent _ left right.
			left right: newParent left.
			newParent left: left.
			self left: newParent right.
			newParent right: self.
		].
	].
	parent computeHeight.
	^parent! !

!ZipEncoderNode methodsFor: 'testing' stamp: 'ar 12/24/1999 23:17'!
isLeaf
	^left == nil! !

!ZipEncoderNode class methodsFor: 'instance creation' stamp: 'ar 12/26/1999 10:47'!
value: v frequency: f height: h
	^self new setValue: v frequency: f height: h! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/30/1999 01:43'!
bitLengthAt: index
	^bitLengths at: index+1! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/30/1999 01:32'!
bitLengths
	"Return an array of all bitLength values for valid codes"
	^bitLengths! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/30/1999 01:04'!
codeAt: index
	^codes at: index+1! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/30/1999 01:24'!
codes
	"Return an array of all valid codes"
	^codes! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/25/1999 17:15'!
maxCode
	^maxCode! !

!ZipEncoderTree methodsFor: 'accessing' stamp: 'ar 12/25/1999 21:45'!
maxCode: aNumber
	maxCode _ aNumber.! !

!ZipEncoderTree methodsFor: 'private' stamp: 'ar 12/30/1999 14:26'!
bitLengths: blArray codes: codeArray
	bitLengths _ blArray as: WordArray.
	codes _ codeArray as: WordArray.
	self assert:[(self bitLengthAt: maxCode) > 0].! !

!ZipEncoderTree methodsFor: 'private' stamp: 'ar 12/26/1999 11:02'!
reverseBits: code length: length
	"Bit reverse the given code"
	| result bit bits |
	result _ 0.
	bits _ code.
	1 to: length do:[:i|
		bit _ bits bitAnd: 1.
		result _ result << 1 bitOr: bit.
		bits _ bits >> 1].
	^result! !

!ZipEncoderTree methodsFor: 'private' stamp: 'ar 12/30/1999 14:26'!
setValuesFrom: nodeList
	self bitLengths: (nodeList collect:[:n| n bitLength] from: 1 to: maxCode+1)
		codes: (nodeList collect:[:n| n code] from: 1 to: maxCode+1).! !

!ZipEncoderTree methodsFor: 'encoding' stamp: 'ar 12/30/1999 01:34'!
buildCodes: nodeList counts: blCounts maxDepth: depth
	"Build the codes for all nodes"
	| nextCode code node length |
	nextCode _ WordArray new: depth+1.
	code _ 0.
	1 to: depth do:[:bits|
		code _ (code + (blCounts at: bits)) << 1.
		nextCode at: bits+1 put: code].
	self assert:[(code + (blCounts at: depth+1) - 1) = (1 << depth - 1)].
	0 to: maxCode do:[:n|
		node _ nodeList at: n+1.
		length _ node bitLength.
		length = 0 ifFalse:[
			code _ nextCode at: length+1.
			node code: (self reverseBits: code length: length).
			nextCode at: length+1 put: code+1.
		].
	].! !

!ZipEncoderTree methodsFor: 'encoding' stamp: 'ar 12/26/1999 10:42'!
buildHierarchyFrom: aHeap
	"Build the node hierarchy based on the leafs in aHeap"
	| left right parent |
	[aHeap size > 1] whileTrue:[
		left _ aHeap removeFirst.
		right _ aHeap removeFirst.
		parent _ ZipEncoderNode value: -1 
			frequency: (left frequency + right frequency)
			height: (left height max: right height) + 1.
		left parent: parent.
		right parent: parent.
		parent left: left.
		parent right: right.
		aHeap add: parent].
	^aHeap removeFirst
! !

!ZipEncoderTree methodsFor: 'encoding' stamp: 'ar 12/30/1999 14:19'!
buildTree: nodeList maxDepth: depth
	"Build either the literal or the distance tree"
	| heap rootNode blCounts |
	heap _ Heap new: nodeList size // 3.
	heap sortBlock: self nodeSortBlock.
	"Find all nodes with non-zero frequency and add to heap"
	maxCode _ 0.
	nodeList do:[:dNode|
		dNode frequency = 0 ifFalse:[
			maxCode _ dNode value.
			heap add: dNode]].
	"The pkzip format requires that at least one distance code exists,
	and that at least one bit should be sent even if there is only one
	possible code. So to avoid special checks later on we force at least
	two codes of non zero frequency."
	heap size = 0 ifTrue:[
		self assert:[maxCode = 0].
		heap add: nodeList first.
		heap add: nodeList second.
		maxCode _ 1].
	heap size = 1 ifTrue:[
		nodeList first frequency = 0
			ifTrue:[heap add: nodeList first]
			ifFalse:[heap add: nodeList second].
		maxCode _ maxCode max: 1].
	rootNode _ self buildHierarchyFrom: heap.
	rootNode height > depth ifTrue:[
		rootNode _ rootNode rotateToHeight: depth.
		rootNode height > depth ifTrue:[self error:'Cannot encode tree']].
	blCounts _ WordArray new: depth+1.
	rootNode encodeBitLength: blCounts from: self.
	self buildCodes: nodeList counts: blCounts maxDepth: depth.
	self setValuesFrom: nodeList.! !

!ZipEncoderTree methodsFor: 'encoding' stamp: 'ar 12/30/1999 01:24'!
buildTreeFrom: frequencies maxDepth: depth
	"Build the receiver from the given frequency values"
	| nodeList |
	nodeList _ Array new: frequencies size.
	1 to: frequencies size do:[:i|
		nodeList at: i put: (ZipEncoderNode value: i-1 frequency: (frequencies at: i) height: 0)
	].
	self buildTree: nodeList maxDepth: depth.! !

!ZipEncoderTree methodsFor: 'encoding' stamp: 'ar 12/26/1999 10:42'!
nodeSortBlock
	^[:n1 :n2|
		n1 frequency = n2 frequency
			ifTrue:[n1 height <= n2 height]
			ifFalse:[n1 frequency <= n2 frequency]].! !

!ZipEncoderTree class methodsFor: 'instance creation' stamp: 'ar 12/30/1999 01:25'!
buildTreeFrom: frequencies maxDepth: depth
	^self new buildTreeFrom: frequencies maxDepth: depth! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 19:09'!
addDirectory: aFileName
	^self addDirectory: aFileName as: aFileName
! !

!Archive methodsFor: 'archive operations' stamp: 'nk 12/20/2002 14:57'!
addDirectory: aFileName as: anotherFileName
	| newMember |
	newMember _ self memberClass newFromDirectory: aFileName.
	self addMember: newMember.
	newMember localFileName: anotherFileName.
	^newMember! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/21/2001 18:29'!
addFile: aFileName
	^self addFile: aFileName as: aFileName! !

!Archive methodsFor: 'archive operations' stamp: 'nk 12/20/2002 15:03'!
addFile: aFileName as: anotherFileName
	| newMember |
	newMember _ self memberClass newFromFile: aFileName.
	self addMember: newMember.
	newMember localFileName: anotherFileName.
	^newMember! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 19:09'!
addMember: aMember
	^members addLast: aMember! !

!Archive methodsFor: 'archive operations' stamp: 'nk 12/20/2002 15:03'!
addString: aString as: aFileName
	| newMember |
	newMember _ self memberClass newFromString: aString named: aFileName.
	self addMember: newMember.
	newMember localFileName: aFileName.
	^newMember! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/24/2001 14:12'!
canWriteToFileNamed: aFileName
	"Catch attempts to overwrite existing zip file"
	^(members anySatisfy: [ :ea | ea usesFileNamed: aFileName ]) not.
! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 07:57'!
contentsOf: aMemberOrName
	| member |
	member _ self member: aMemberOrName.
	member ifNil: [ ^nil ].
	^member contents! !

!Archive methodsFor: 'archive operations' stamp: 'pb 5/25/2016 01:06'!
extractMember: aMemberOrName
	| member |
	member _ self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: member localFileName inDirectory: DirectoryEntry currentDirectory! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 07:57'!
extractMember: aMemberOrName toFileNamed: aFileName
	| member |
	member _ self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: aFileName! !

!Archive methodsFor: 'archive operations' stamp: 'pb 5/25/2016 01:06'!
extractMemberWithoutPath: aMemberOrName
	self extractMemberWithoutPath: aMemberOrName inDirectory: DirectoryEntry currentDirectory! !

!Archive methodsFor: 'archive operations' stamp: 'jmv 5/31/2016 10:35'!
extractMemberWithoutPath: aMemberOrName inDirectory: aDirectoryEntry
	| member |
	member _ self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: member localFileName asFileEntry name inDirectory: aDirectoryEntry! !

!Archive methodsFor: 'archive operations' stamp: 'jmv 3/2/2010 08:56'!
memberNamed: aString
	"Return the first member whose zip name or local file name matches aString, or nil"
	^members detect: [ :ea | ea fileName = aString or: [ ea localFileName = aString ]] ifNone: nil! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/21/2001 18:00'!
memberNames
	^members collect: [ :ea | ea fileName ]! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/21/2001 17:58'!
members
	^members! !

!Archive methodsFor: 'archive operations' stamp: 'nk 12/20/2002 14:50'!
membersMatching: aString
	^members select: [ :ea | (aString match: ea fileName) or: [ aString match: ea localFileName ] ]! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/21/2001 17:59'!
numberOfMembers
	^members size! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 07:57'!
removeMember: aMemberOrName
	| member |
	member _ self member: aMemberOrName.
	member ifNotNil: [ members remove: member ].
	^member! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 07:57'!
replaceMember: aMemberOrName with: newMember
	| member |
	member _ self member: aMemberOrName.
	member ifNotNil: [ members replaceAll: member with: newMember ].
	^member! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/22/2001 17:24'!
setContentsOf: aMemberOrName to: aString
	| newMember oldMember |
	oldMember _ self member: aMemberOrName.
	newMember _ (self memberClass newFromString: aString named: oldMember fileName)
		copyFrom: oldMember.
	self replaceMember: oldMember with: newMember.! !

!Archive methodsFor: 'archive operations' stamp: 'nk 2/21/2001 20:58'!
writeTo: aStream
	self subclassResponsibility! !

!Archive methodsFor: 'archive operations' stamp: 'pb 5/25/2016 01:35'!
writeToFileNamed: aFileName

	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^self error: (aFileName, ' is needed by one or more members in this archive') ].
	aFileName asFileEntry writeStreamDo: [ :stream |
		self writeTo: stream ]! !

!Archive methodsFor: 'initialization' stamp: 'nk 2/21/2001 17:58'!
initialize
	members _ OrderedCollection new.! !

!Archive methodsFor: 'private' stamp: 'nk 2/22/2001 07:56'!
member: aMemberOrName
	^(members includes: aMemberOrName)
		ifTrue: [ aMemberOrName ]
		ifFalse: [ self memberNamed: aMemberOrName ].! !

!Archive methodsFor: 'private' stamp: 'nk 2/21/2001 18:14'!
memberClass
	self subclassResponsibility! !

!ZipArchive methodsFor: 'initialization' stamp: 'ar 3/2/2001 18:47'!
close
	self members do:[:m| m close].! !

!ZipArchive methodsFor: 'initialization' stamp: 'nk 2/22/2001 17:20'!
initialize
	super initialize.
	writeEOCDOffset _ writeCentralDirectoryOffset _ 0.
	zipFileComment _ ''.
! !

!ZipArchive methodsFor: 'archive operations' stamp: 'jmv 7/17/2017 15:47:50'!
extractAllTo: aDirectoryEntry
	"Extract all elements to the given directory"
	Utilities informUserDuring: [ :barBlock | self extractAllTo: aDirectoryEntry informing: barBlock]! !

!ZipArchive methodsFor: 'archive operations' stamp: 'jmv 7/17/2017 15:47:41'!
extractAllTo: aDirectoryEntry informing: barBlock
	"Extract all elements to the given directory"
	^self extractAllTo: aDirectoryEntry informing: barBlock overwrite: false! !

!ZipArchive methodsFor: 'archive operations' stamp: 'jmv 7/17/2017 15:50:56'!
extractAllTo: aDirectory informing: barBlock overwrite: allOverwrite
	"Extract all elements to the given directory"
	| dir overwriteAll response |
	overwriteAll := allOverwrite.
	self members do:[:entry|
		entry isDirectory ifTrue:[
			barBlock ifNotNil:[barBlock value: 'Creating ', entry fileName].
			dir := (entry fileName findTokens:'/') 
					inject: aDirectory into:[:base :part| base / part].
			dir assureExistence.
		].
	].
	self members do:[:entry|
		entry isDirectory ifFalse:[
			barBlock ifNotNil:[barBlock value: 'Extracting ', entry fileName].
			response := entry extractInDirectory: aDirectory overwrite: overwriteAll.
			response == #retryWithOverwrite ifTrue:[
				overwriteAll := true.
				response := entry extractInDirectory: aDirectory overwrite: overwriteAll.
			].
			response == #abort ifTrue:[^self].
			response == #failed ifTrue:[
				(self confirm: 'Failed to extract ', entry fileName, '. Proceed?') ifFalse:[^self].
			].
		].
	].
! !

!ZipArchive methodsFor: 'private' stamp: 'nk 2/21/2001 18:26'!
memberClass
	^ZipArchiveMember! !

!ZipArchive methodsFor: 'private' stamp: 'jmv 2/26/2016 16:28'!
readEndOfCentralDirectoryFrom: aStream
	"Read EOCD, starting from position before signature."
	| signature zipFileCommentLength |
	signature _ self readSignatureFrom: aStream.
	signature = EndOfCentralDirectorySignature ifFalse: [ ^self error: 'bad signature at ', aStream position printString ].

	aStream nextUnsignedInt16BigEndian: false. "# of this disk"
	aStream nextUnsignedInt16BigEndian: false. "# of disk with central dir start"
	aStream nextUnsignedInt16BigEndian: false. "# of entries in central dir on this disk"
	aStream nextUnsignedInt16BigEndian: false. "total # of entries in central dir"
	centralDirectorySize _ aStream nextUnsignedInt32BigEndian: false. "size of central directory"
	centralDirectoryOffsetWRTStartingDiskNumber _ aStream nextUnsignedInt32BigEndian: false. "offset of start of central directory"
	zipFileCommentLength _ aStream nextUnsignedInt16BigEndian: false. "zip file comment"
	zipFileComment _ aStream next: zipFileCommentLength.
! !

!ZipArchive methodsFor: 'private' stamp: 'nk 2/23/2001 09:19'!
readMembersFrom: stream named: fileName
	| newMember signature |
	[
		newMember _ self memberClass newFromZipFile: stream named: fileName.
		signature _ self readSignatureFrom: stream.
		signature = EndOfCentralDirectorySignature ifTrue: [ ^self ].
		signature = CentralDirectoryFileHeaderSignature
			ifFalse: [ self error: 'bad CD signature at ', (stream position - 4) hex ].
		newMember readFrom: stream.
		newMember looksLikeDirectory ifTrue: [ newMember _ newMember asDirectory ].
		self addMember: newMember.
	] repeat.! !

!ZipArchive methodsFor: 'private' stamp: 'nk 2/21/2001 20:09'!
readSignatureFrom: stream
	"Returns next signature from given stream, leaves stream positioned afterwards."

	| signatureData | 
	signatureData _ String new: 4.
	stream next: 4 into: signatureData.
	({ CentralDirectoryFileHeaderSignature . LocalFileHeaderSignature . EndOfCentralDirectorySignature }
		includes: signatureData)
			ifFalse: [ ^self error: 'bad signature ', signatureData asHex, ' at position ', (stream position - 4) asString ].
	^signatureData
! !

!ZipArchive methodsFor: 'private' stamp: 'nk 2/21/2001 20:19'!
writeCentralDirectoryTo: aStream
	| offset |
	offset _ writeCentralDirectoryOffset.
	members do: [ :member |
		member writeCentralDirectoryFileHeaderTo: aStream.
		offset _ offset + member centralDirectoryHeaderSize.
	].
	writeEOCDOffset _ offset.
	self writeEndOfCentralDirectoryTo: aStream.

! !

!ZipArchive methodsFor: 'private' stamp: 'jmv 2/26/2016 16:37'!
writeEndOfCentralDirectoryTo: aStream

	aStream nextPutAll: EndOfCentralDirectorySignature.
	aStream nextUnsignedInt16Put: 0 bigEndian: false. "diskNumber"
	aStream nextUnsignedInt16Put: 0 bigEndian: false. "diskNumberWithStartOfCentralDirectory"
	aStream nextUnsignedInt16Put: members size bigEndian: false. "numberOfCentralDirectoriesOnThisDisk"
	aStream nextUnsignedInt16Put: members size bigEndian: false. "numberOfCentralDirectories"
	aStream nextUnsignedInt32Put: writeEOCDOffset - writeCentralDirectoryOffset bigEndian: false. "size of central dir"
	aStream nextUnsignedInt32Put: writeCentralDirectoryOffset bigEndian: false. "offset of central dir"
	aStream nextUnsignedInt16Put: zipFileComment size bigEndian: false. "zip file comment"
	zipFileComment isEmpty ifFalse: [ aStream nextPutAll: zipFileComment ].

! !

!ZipArchive methodsFor: 'accessing' stamp: 'nk 3/27/2002 11:23'!
prependedDataSize
	"Answer the size of whatever data exists before my first member.
	Assumes that I was read from a file or stream (i.e. the first member is a ZipFileMember)"
	^members isEmpty
		ifFalse: [ members first localHeaderRelativeOffset ]
		ifTrue: [ centralDirectoryOffsetWRTStartingDiskNumber ]! !

!ZipArchive methodsFor: 'accessing' stamp: 'nk 2/24/2001 13:44'!
zipFileComment
	^zipFileComment asString! !

!ZipArchive methodsFor: 'accessing' stamp: 'nk 2/24/2001 13:43'!
zipFileComment: aString
	zipFileComment _ aString! !

!ZipArchive methodsFor: 'reading' stamp: 'pb 5/25/2016 01:51'!
readFrom: aStreamOrFileName
	| stream name eocdPosition |
	stream _ (aStreamOrFileName is: #Stream)
		ifTrue: [name _ aStreamOrFileName name. aStreamOrFileName]
		ifFalse: [(name _ aStreamOrFileName) asFileEntry readStream].
	stream binary.
	eocdPosition _ self class findEndOfCentralDirectoryFrom: stream.
	eocdPosition <= 0 ifTrue: [self error: 'can''t find EOCD position'].
	self readEndOfCentralDirectoryFrom: stream.
	stream position: eocdPosition - centralDirectorySize.
	self readMembersFrom: stream named: name! !

!ZipArchive methodsFor: 'writing' stamp: 'nk 2/23/2001 10:29'!
writeTo: stream
	stream binary.
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset _ stream position.
	self writeCentralDirectoryTo: stream.
	! !

!ZipArchive methodsFor: 'writing' stamp: 'nk 3/27/2002 10:42'!
writeTo: stream prepending: aString
	stream binary.
	stream nextPutAll: aString.
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset _ stream position.
	self writeCentralDirectoryTo: stream.
	! !

!ZipArchive methodsFor: 'writing' stamp: 'pb 5/25/2016 01:51'!
writeTo: stream prependingFileNamed: aFileName
	| prepended buffer |
	stream binary.
	prepended _ aFileName asFileEntry readStream.
	prepended binary.
	buffer _ ByteArray new: (prepended size min: 32768).
	[ prepended atEnd ] whileFalse: [ | bytesRead |
		bytesRead _ prepended readInto: buffer startingAt: 1 count: buffer size.
		stream next: bytesRead putAll: buffer startingAt: 1
	].
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset _ stream position.
	self writeCentralDirectoryTo: stream.
	! !

!ZipArchive methodsFor: 'writing' stamp: 'jmv 5/31/2016 11:24'!
writeToFileNamed: aFileName prepending: aString

	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^self error: (aFileName, ' is needed by one or more members in this archive') ].
	aFileName asFileEntry forceWriteStreamDo: [ :stream |
		self writeTo: stream prepending: aString ]! !

!ZipArchive methodsFor: 'writing' stamp: 'jmv 5/31/2016 11:24'!
writeToFileNamed: aFileName prependingFileNamed: anotherFileName

	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^self error: (aFileName, ' is needed by one or more members in this archive') ].
	aFileName asFileEntry forceWriteStreamDo: [ :stream |
		self writeTo: stream prependingFileNamed: anotherFileName ]! !

!ZipArchive class methodsFor: 'constants' stamp: 'nk 2/22/2001 14:13'!
compressionDeflated
	^CompressionDeflated! !

!ZipArchive class methodsFor: 'constants' stamp: 'nk 2/22/2001 14:12'!
compressionLevelDefault
	^CompressionLevelDefault! !

!ZipArchive class methodsFor: 'constants' stamp: 'nk 2/22/2001 14:12'!
compressionLevelNone
	^CompressionLevelNone ! !

!ZipArchive class methodsFor: 'constants' stamp: 'nk 2/22/2001 14:13'!
compressionStored
	^CompressionStored! !

!ZipArchive class methodsFor: 'constants' stamp: 'jmv 11/2/2010 12:40'!
findEndOfCentralDirectoryFrom: stream
	"Seek in the given stream to the end, then read backwards until we find the
	signature of the central directory record. Leave the file positioned right
	before the signature.

	Answers the file position of the EOCD, or 0 if not found."

	| data fileLength seekOffset pos maxOffset |
	stream setToEnd.
	fileLength _ stream position.
	"If the file length is less than 18 for the EOCD length plus 4 for the signature, we have a problem"
	fileLength < 22 ifTrue: [^ self error: 'file is too short'].
	
	seekOffset _ 0.
	pos _ 0.
	data _ String new: 4100.
	maxOffset _ 40960 min: fileLength.	"limit search range to 40K"

	[
		seekOffset _ (seekOffset + 4096) min: fileLength.
		stream position: fileLength - seekOffset.
		data _ stream next: (4100 min: seekOffset) into: data startingAt: 1.
		pos _ self
			lastIndexOfPKSignature: EndOfCentralDirectorySignature
			in: data.
		pos = 0 and: [seekOffset < maxOffset]
	] whileTrue.

	^ pos > 0
		ifTrue: [ | newPos | stream position: (newPos _ (stream position + pos - seekOffset - 1)). newPos]
		ifFalse: [0]! !

!ZipArchive class methodsFor: 'constants' stamp: 'ar 9/6/2010 15:28'!
lastIndexOfPKSignature: aSignature in: data
	"Answer the last index in data where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(data size - 3) to: 1 by: -1 do: [ :i |
		(((data at: i) = a)
			and: [ ((data at: i + 1) = b)
				and: [ ((data at: i + 2) = c)
					and: [ ((data at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0! !

!ZipArchive class methodsFor: 'constants' stamp: 'ar 2/27/2001 13:38'!
validSignatures
	"Return the valid signatures for a zip file"
	^Array 
		with: LocalFileHeaderSignature
		with: CentralDirectoryFileHeaderSignature
		with: EndOfCentralDirectorySignature! !

!ZipArchive class methodsFor: 'file list services' stamp: 'pb 2/26/2020 13:51:11'!
fileReaderServicesForFileEntry: aFileEntry suffix: suffix
	"FileList buttons that are swe know about."

	suffix = 'mcz'
		ifTrue: [ ^ {
			CodePackageFile serviceInstallMonticelloPackage .
			CodeFileBrowser serviceBrowseMCZCode
		} ].
	^#()! !

!ZipArchive class methodsFor: 'file format' stamp: 'pb 5/25/2016 01:51'!
isZipArchive: aStreamOrFileName
	"Answer whether the given filename represents a valid zip file."

	| stream eocdPosition |
	stream _ (aStreamOrFileName is: #Stream)
		ifTrue: [aStreamOrFileName]
		ifFalse: [aStreamOrFileName asFileEntry readStream ].
	stream ifNil: [^ false].
	"nil happens sometimes somehow"
	stream size < 22 ifTrue: [^ false].
	stream binary.
	eocdPosition _ self findEndOfCentralDirectoryFrom: stream.
	stream ~= aStreamOrFileName ifTrue: [stream close].
	^ eocdPosition > 0! !

!ArchiveMember methodsFor: 'initialization' stamp: 'ar 3/2/2001 18:46'!
close
! !

!ArchiveMember methodsFor: 'initialization' stamp: 'nk 3/7/2004 16:05'!
initialize
	fileName _ ''.
	isCorrupt _ false.! !

!ArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 16:00'!
fileName
	^fileName! !

!ArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 16:00'!
fileName: aName
	fileName _ aName! !

!ArchiveMember methodsFor: 'accessing' stamp: 'nk 3/7/2004 16:16'!
isCorrupt
	^isCorrupt ifNil: [ isCorrupt _ false ]! !

!ArchiveMember methodsFor: 'accessing' stamp: 'nk 3/7/2004 16:06'!
isCorrupt: aBoolean
	"Mark this member as being corrupt."
	isCorrupt := aBoolean! !

!ArchiveMember methodsFor: 'accessing' stamp: 'jmv 10/27/2015 18:12'!
localFileName: aString
	"Set my internal filename.
	Returns the (possibly new) filename.
	aString will be translated from local FS format into Unix format."

	^fileName _ aString copyReplaceAll: '\' with: '/'.! !

!ArchiveMember methodsFor: 'testing' stamp: 'nk 2/21/2001 19:43'!
usesFileNamed: aFileName
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^false! !

!ArchiveMember class methodsFor: 'as yet unclassified' stamp: 'nk 2/21/2001 18:33'!
newDirectoryNamed: aString
	self subclassResponsibility! !

!ArchiveMember class methodsFor: 'as yet unclassified' stamp: 'nk 2/21/2001 18:32'!
newFromFile: aFileName
	self subclassResponsibility! !

!ArchiveMember class methodsFor: 'as yet unclassified' stamp: 'nk 2/21/2001 18:32'!
newFromString: aString
	self subclassResponsibility! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 2/21/2001 21:55'!
asDirectory
	^ZipDirectoryMember new copyFrom: self! !

!ZipArchiveMember methodsFor: 'private' stamp: 'jmv 2/16/2011 18:29'!
dosToSqueakTime: dt
	"DOS years start at 1980, so add 1980."
	| year mon mday hour min sec date time |

	year := (( dt bitShift: -25 ) bitAnd: 16r7F ).
	mon := (( dt bitShift: -21 ) bitAnd: 16r0F ).
	mday := (( dt bitShift: -16 ) bitAnd: 16r1F ).
	date := Date newDay: mday month: mon year: year+1980.

	hour := (( dt bitShift: -11 ) bitAnd: 16r1F ).
	min := (( dt bitShift: -5 ) bitAnd: 16r3F ).
	sec := (( dt bitShift: 1 ) bitAnd: 16r3E ).
	time := ((( hour * 60 ) + min ) * 60 ) + sec.

	^date secondsSinceSqueakEpoch + time

	! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 2/23/2001 08:24'!
endRead
	readDataRemaining _ 0.! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 2/21/2001 23:57'!
mapPermissionsFromUnix: unixPerms
	^ unixPerms bitShift: 16.! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 2/21/2001 23:58'!
mapPermissionsToUnix: dosPerms
	^ dosPerms bitShift: -16.! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 2/22/2001 20:42'!
readRawChunk: n
	self subclassResponsibility! !

!ZipArchiveMember methodsFor: 'private' stamp: 'nk 4/28/2002 21:53'!
rewindData
	readDataRemaining _  (desiredCompressionMethod = CompressionDeflated
		and: [ compressionMethod = CompressionDeflated ])
			ifTrue: [ compressedSize ]
			ifFalse: [ uncompressedSize ].
! !

!ZipArchiveMember methodsFor: 'private' stamp: 'jdr 8/11/2010 11:00'!
squeakToDosTime: secs
	| dosTime dateTime |

	dateTime := Time dateAndTimeFromSeconds: secs.
	dosTime := (dateTime second seconds) bitShift: -1.
	dosTime := dosTime + ((dateTime second minutes) bitShift: 5).
	dosTime := dosTime + ((dateTime second hours) bitShift: 11).
	dosTime := dosTime + ((dateTime first dayOfMonth) bitShift: 16).
	dosTime := dosTime + ((dateTime first monthIndex) bitShift: 21).
	dosTime := dosTime + (((dateTime first year) - 1980) bitShift: 25).
	^dosTime
! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 09:44'!
centralDirectoryHeaderSize
	^ 46 + fileName size + cdExtraField size + fileComment size! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/23/2001 08:00'!
clearExtraFields
	cdExtraField _ ''.
	localExtraField _ ''.! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:38'!
compressedSize
	"Return the compressed size for this member.
	This will not be set for members that were constructed from strings
	or external files until after the member has been written."
	^compressedSize! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/21/2001 22:02'!
compressionMethod
	"Returns my compression method. This is the method that is
	currently being used to compress my data.

	This will be CompressionStored for added string or file members,
	or CompressionStored or CompressionDeflated (others are possible but not handled)"

	^compressionMethod! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:28'!
crc32
	^crc32! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:35'!
crc32String
	| hexString |
	hexString _ crc32 hex.
	^('00000000' copyFrom: 1 to: 11 - (hexString size)) , (hexString copyFrom: 4 to: hexString size)! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/21/2001 22:10'!
desiredCompressionLevel
	^desiredCompressionLevel! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/21/2001 22:14'!
desiredCompressionLevel: aNumber
	"Set my desiredCompressionLevel
	This is the method that will be used to write.
	Returns prior desiredCompressionLevel.

	Valid arguments are 0 (CompressionLevelNone) through 9,
	including 6 (CompressionLevelDefault).

	0 (CompressionLevelNone) will change the desiredCompressionMethod
	to CompressionStored. All other arguments will change the
	desiredCompressionMethod to CompressionDeflated."

	| old |
	old _ desiredCompressionLevel.
	desiredCompressionLevel _ aNumber.
	desiredCompressionMethod _ (aNumber > 0)
		ifTrue: [ CompressionDeflated ]
		ifFalse: [ CompressionStored ].
	^old! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/21/2001 22:03'!
desiredCompressionMethod
	"Get my desiredCompressionMethod.
	This is the method that will be used to write"

	^desiredCompressionMethod! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/23/2001 11:25'!
desiredCompressionMethod: aNumber
	"Set my desiredCompressionMethod
	This is the method that will be used to write.
	Answers prior desiredCompressionMethod.

	Only CompressionDeflated or CompressionStored are valid arguments.

	Changing to CompressionStored will change my desiredCompressionLevel
	to CompressionLevelNone; changing to CompressionDeflated will change my
	desiredCompressionLevel to CompressionLevelDefault."

	| old |
	old _ desiredCompressionMethod.
	desiredCompressionMethod _ aNumber.
	desiredCompressionLevel _ (aNumber = CompressionDeflated)
			ifTrue: [ CompressionLevelDefault ]
			ifFalse: [ CompressionLevelNone ].
	compressionMethod = CompressionStored ifTrue: [ compressedSize _ uncompressedSize ].
	^old.! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:25'!
fileComment
	^fileComment! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:25'!
fileComment: aString
	fileComment _ aString! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'jdr 8/11/2010 10:38'!
lastModTime
	"Return my last modification date/time stamp,
	converted to Squeak seconds"

	^self dosToSqueakTime: lastModFileDateTime! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'jmv 10/27/2015 18:16'!
localFileName
	"Answer my fileName in terms of the local directory naming convention"
	^fileName! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'jdr 8/11/2010 11:08'!
setLastModFileDateTimeFrom: aSmalltalkTime

	lastModFileDateTime := self squeakToDosTime: aSmalltalkTime! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 11/11/2002 21:03'!
splitFileName
	"Answer my name split on slash boundaries. A directory will have a trailing empty string."
	^ fileName findTokens: '/'.! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:38'!
uncompressedSize
	"Return the uncompressed size for this member."
	^uncompressedSize! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:18'!
unixFileAttributes
	^self mapPermissionsToUnix: externalFileAttributes.! !

!ZipArchiveMember methodsFor: 'accessing' stamp: 'nk 2/22/2001 00:24'!
unixFileAttributes: perms
	| oldPerms newPerms |
	oldPerms _ self mapPermissionsToUnix: externalFileAttributes.
	newPerms _  self isDirectory
			ifTrue: [ (perms bitAnd: FileAttrib bitInvert) bitOr: DirectoryAttrib ]
			ifFalse: [ (perms bitAnd: DirectoryAttrib bitInvert) bitOr: FileAttrib ].
	externalFileAttributes _ self mapPermissionsFromUnix: newPerms.
	^oldPerms.! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'ar 2/28/2001 14:01'!
compressDataTo: aStream
	"Copy my deflated data to the given stream."
	| encoder startPos endPos |

	encoder _ ZipWriteStream on: aStream.
	startPos _ aStream position.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data _ self readRawChunk: (4096 min: readDataRemaining).
		encoder nextPutAll: data asByteArray.
		readDataRemaining _ readDataRemaining - data size.
	].
	encoder finish. "not close!!"
	endPos _ aStream position.
	compressedSize _ endPos - startPos.
	crc32 _ encoder crc.
! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'nk 2/23/2001 11:04'!
copyDataTo: aStream

	compressionMethod = CompressionStored ifTrue: [ ^self copyDataWithCRCTo: aStream ].

	self copyRawDataTo: aStream.! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'nk 3/7/2004 15:42'!
copyDataWithCRCTo: aStream
	"Copy my data to aStream. Also set the CRC-32.
	Only used when compressionMethod = desiredCompressionMethod = CompressionStored"

	uncompressedSize _ compressedSize _ readDataRemaining.

	crc32 _ 16rFFFFFFFF.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data _ self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		crc32 _ ZipWriteStream updateCrc: crc32 from: 1 to: data size in: data.
		readDataRemaining _ readDataRemaining - data size.
	].

	crc32 _ crc32 bitXor: 16rFFFFFFFF.
! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'nk 2/23/2001 11:04'!
copyRawDataTo: aStream

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data _ self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining _ readDataRemaining - data size.
	].
! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'nk 2/24/2001 17:57'!
copyRawDataTo: aStream from: start to: finish

	readDataRemaining _ readDataRemaining min: finish - start + 1.

	self readRawChunk: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data _ self readRawChunk: (32768 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining _ readDataRemaining - data size.
	].
! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'jmv 2/26/2016 16:51'!
refreshLocalFileHeaderTo: aStream
	"Re-writes my local header to the given stream.
	To be called after writing the data stream.
	Assumes that fileName and localExtraField sizes didn't change since last written."

	| here |
	here _ aStream position.
	aStream position: writeLocalHeaderRelativeOffset.

	aStream nextPutAll: LocalFileHeaderSignature.
	aStream nextUnsignedInt16Put: versionNeededToExtract bigEndian: false.
	aStream nextUnsignedInt16Put: bitFlag bigEndian: false.
	aStream nextUnsignedInt16Put: desiredCompressionMethod bigEndian: false.
	aStream nextUnsignedInt32Put: lastModFileDateTime bigEndian: false.
	aStream nextUnsignedInt32Put: crc32 bigEndian: false.
	aStream nextUnsignedInt32Put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]) bigEndian: false.
	aStream nextUnsignedInt32Put: uncompressedSize bigEndian: false.
	aStream nextUnsignedInt16Put: fileName size bigEndian: false.
	aStream nextUnsignedInt16Put: localExtraField size bigEndian: false.

	aStream position: here! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'jmv 2/26/2016 16:53'!
writeCentralDirectoryFileHeaderTo: aStream
	"C2 v3 V4 v5 V2"

	aStream nextPutAll: CentralDirectoryFileHeaderSignature.
	aStream nextPut: versionMadeBy.
	aStream nextPut: fileAttributeFormat.

	aStream nextUnsignedInt16Put: versionNeededToExtract bigEndian: false.
	aStream nextUnsignedInt16Put: bitFlag bigEndian: false.
	aStream nextUnsignedInt16Put: desiredCompressionMethod bigEndian: false.

	aStream nextUnsignedInt32Put: lastModFileDateTime bigEndian: false.

	"These next 3 should have been updated during the write of the data"
	aStream nextUnsignedInt32Put: crc32 bigEndian: false.
	aStream nextUnsignedInt32Put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]) bigEndian: false.
	aStream nextUnsignedInt32Put: uncompressedSize bigEndian: false.

	aStream nextUnsignedInt16Put: fileName size bigEndian: false.
	aStream nextUnsignedInt16Put: cdExtraField size bigEndian: false.
	aStream nextUnsignedInt16Put: fileComment size bigEndian: false.
	aStream nextUnsignedInt16Put: 0 bigEndian: false.		"diskNumberStart"
	aStream nextUnsignedInt16Put: internalFileAttributes bigEndian: false.

	aStream nextUnsignedInt32Put: externalFileAttributes bigEndian: false.
	aStream nextUnsignedInt32Put: writeLocalHeaderRelativeOffset bigEndian: false.

	aStream nextPutAll: fileName asByteArray.
	aStream nextPutAll: cdExtraField asByteArray.
	aStream nextPutAll: fileComment asByteArray.! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'jmv 2/26/2016 16:54'!
writeDataDescriptorTo: aStream
	"This writes a data descriptor to the given stream.
	Assumes that crc32, writeOffset, and uncompressedSize are
	set correctly (they should be after a write).
	Further, the local file header should have the
	GPBF_HAS_DATA_DESCRIPTOR_MASK (8) bit set."

	aStream nextUnsignedInt32Put: crc32 bigEndian: false.
	aStream nextUnsignedInt32Put: compressedSize bigEndian: false.
	aStream nextUnsignedInt32Put: uncompressedSize bigEndian: false! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'ar 8/10/2010 13:15'!
writeDataTo: aStream
	"Copy my (possibly inflated or deflated) data to the given stream.
	This might do compression, decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	"Note: Do not shortcut this method if uncompressedSize = 0. Even in this case
	data may be produced by the compressor (i.e., '' zipped size > 0) and must
	be stored in the file or else other utilities will treat the zip file as corrupt."

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self compressDataTo: aStream ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream ].

	self copyDataTo: aStream.! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'nk 2/24/2001 18:01'!
writeDataTo: aStream from: start to: finish
	"Copy my (possibly inflated or deflated) data to the given stream.
	But only the specified byte range.
	This might do decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	uncompressedSize = 0 ifTrue: [ ^self ].	"nothing to do because no data"
	start > finish ifTrue: [ ^self ].
	start > uncompressedSize ifTrue: [ ^self ].

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self error: 'only supports uncompression or copying right now' ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream from: start to: finish ].

	self copyRawDataTo: aStream from: start to: finish.! !

!ZipArchiveMember methodsFor: 'private-writing' stamp: 'jmv 2/26/2016 16:55'!
writeLocalFileHeaderTo: aStream
	"Write my local header to a file handle.
	Stores the offset to the start of the header in my
	writeLocalHeaderRelativeOffset member."

	aStream nextPutAll: LocalFileHeaderSignature.
	aStream nextUnsignedInt16Put: versionNeededToExtract bigEndian: false.
	aStream nextUnsignedInt16Put: bitFlag bigEndian: false.
	aStream nextUnsignedInt16Put: desiredCompressionMethod bigEndian: false.

	aStream nextUnsignedInt32Put: lastModFileDateTime bigEndian: false.
	aStream nextUnsignedInt32Put: crc32 bigEndian: false.
	aStream nextUnsignedInt32Put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]) bigEndian: false.
	aStream nextUnsignedInt32Put: uncompressedSize bigEndian: false.

	aStream nextUnsignedInt16Put: fileName size bigEndian: false.
	aStream nextUnsignedInt16Put: localExtraField size bigEndian: false.

	aStream nextPutAll: fileName asByteArray.
	aStream nextPutAll: localExtraField asByteArray! !

!ZipArchiveMember methodsFor: 'reading' stamp: 'ar 2/27/2001 14:30'!
contentStream
	"Answer my contents as a string."
	| s |
	s _ RWBinaryOrTextStream on: (String new: self uncompressedSize).
	self extractTo: s.
	^s reset! !

!ZipArchiveMember methodsFor: 'reading' stamp: 'jmv 10/8/2017 17:07:26'!
contents
	"Answer my contents as a string."
	| s |
	s _ RWBinaryOrTextStream on: (String new: self uncompressedSize).
	self extractTo: s.
	s useCharacters.
	^s contents! !

!ZipArchiveMember methodsFor: 'reading' stamp: 'jmv 10/8/2017 17:07:29'!
contentsFrom: start to: finish
	"Answer my contents as a string."
	| s |
	s _ RWBinaryOrTextStream on: (String new: finish - start + 1).
	self extractTo: s from: start to: finish.
	s useCharacters.
	^s contents! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'jmv 5/31/2016 10:38'!
extractInDirectory: aDirectoryEntry

	self extractToFileNamed: self localFileName inDirectory: aDirectoryEntry
! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'jmv 11/3/2016 11:52:24'!
extractInDirectory: aDirectory overwrite: overwriteAll
	"Extract this entry into the given directory. Answer #okay, #failed, #abort, or #retryWithOverwrite."
	| path fileDir file index localName |
	path := fileName findTokens:'/'.
	localName := path last.
	fileDir := path allButLast inject: aDirectory into:[:base :part| base / part].
	fileDir assureExistence.
	file := [fileDir privateNewFile: localName] on: FileExistsException do:[:ex| ex return: nil].
	file ifNil:[
		overwriteAll ifFalse:[
			[index := (PopUpMenu labelArray:{
						'Yes, overwrite'. 
						'No, don''t overwrite'. 
						'Overwrite ALL files'.
						'Cancel operation'
					} lines: #(2)) startUpWithCaption: fileName, ' already exists. Overwrite?'.
			index == nil] whileTrue.
			index = 4 ifTrue:[^#abort].
			index = 3 ifTrue:[^#retryWithOverwrite].
			index = 2 ifTrue:[^#okay].
		].
		file := [fileDir privateForceNewFile: localName] on: Error do:[:ex| ex return].
		file ifNil:[^#failed].
	].
	self extractTo: file.
	file close.
	^#okay! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'nk 2/22/2001 18:03'!
extractTo: aStream
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression _ self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead.! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'nk 2/24/2001 18:03'!
extractTo: aStream from: start to: finish
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression _ self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream from: start to: finish.
	self desiredCompressionMethod: oldCompression.
	self endRead.! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'pb 5/25/2016 01:16'!
extractToFileNamed: aFileName

	self extractToFileNamed: aFileName inDirectory: DirectoryEntry currentDirectory! !

!ZipArchiveMember methodsFor: 'extraction' stamp: 'jmv 5/31/2016 11:24'!
extractToFileNamed: aLocalFileName inDirectory: aDirectoryEntry

	self isEncrypted ifTrue: [ ^self error: 'encryption unsupported' ].
	self isDirectory
		ifFalse: [
			aDirectoryEntry / aLocalFileName forceWriteStreamDo: [ :stream |
				self extractTo: stream ] ]
		ifTrue: [
			(aDirectoryEntry / aLocalFileName) asDirectoryEntry assureExistence ]! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:40'!
hasDataDescriptor
	^ (bitFlag bitAnd: 8)	~= 0 "GPBF_HAS_DATA_DESCRIPTOR_MASK"! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:00'!
isDirectory
	^false! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:39'!
isEncrypted
	"Return true if this member is encrypted (this is unsupported)"
	^ (bitFlag bitAnd: 1) ~= 0! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:41'!
isTextFile
	"Returns true if I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	^ (internalFileAttributes bitAnd: 1) ~= 0
! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:43'!
isTextFile: aBoolean
	"Set whether I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	internalFileAttributes _ aBoolean
		ifTrue: [ internalFileAttributes bitOr: 1 ]
		ifFalse: [ internalFileAttributes bitAnd: 1 bitInvert ]
! !

!ZipArchiveMember methodsFor: 'testing' stamp: 'nk 2/21/2001 20:38'!
looksLikeDirectory
	^false! !

!ZipArchiveMember methodsFor: 'initialization' stamp: 'nk 2/24/2001 16:16'!
initialize
	super initialize.
	lastModFileDateTime _ 0.
	fileAttributeFormat _ FaUnix.
	versionMadeBy _ 20.
	versionNeededToExtract _ 20.
	bitFlag _ 0.
	compressionMethod _ CompressionStored.
	desiredCompressionMethod _ CompressionDeflated.
	desiredCompressionLevel _ CompressionLevelDefault.
	internalFileAttributes _ 0.
	externalFileAttributes _ 0.
	fileName _ ''.
	cdExtraField _ ''.
	localExtraField _ ''.
	fileComment _ ''.
	crc32 _ 0.
	compressedSize _ 0.
	uncompressedSize _ 0.
	self unixFileAttributes: DefaultFilePermissions.! !

!ZipArchiveMember methodsFor: 'writing' stamp: 'nk 2/23/2001 11:28'!
writeTo: aStream
	self rewindData.
	writeLocalHeaderRelativeOffset _ aStream position.
	self writeLocalFileHeaderTo: aStream.
	self writeDataTo: aStream.
	self refreshLocalFileHeaderTo: aStream.! !

!ZipArchiveMember class methodsFor: 'instance creation' stamp: 'nk 2/22/2001 17:27'!
newFromDirectory: aFileName
	^ZipDirectoryMember newNamed: aFileName! !

!ZipArchiveMember class methodsFor: 'instance creation' stamp: 'nk 2/22/2001 17:27'!
newFromFile: aFileName
	^ZipNewFileMember newNamed: aFileName! !

!ZipArchiveMember class methodsFor: 'instance creation' stamp: 'nk 2/22/2001 17:25'!
newFromString: aString named: aFileName
	^ZipStringMember newFrom: aString named: aFileName! !

!ZipArchiveMember class methodsFor: 'instance creation' stamp: 'nk 2/21/2001 20:40'!
newFromZipFile: stream named: fileName
	^ZipFileMember newFrom: stream named: fileName! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'nk 11/11/2002 21:46'!
canonicalizeFileName
	"For security reasons, make all paths relative and remove any ../ portions"

	[fileName beginsWith: '/'] whileTrue: [fileName := fileName allButFirst].
	fileName := fileName copyReplaceAll: '../' with: ''! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'jmv 2/26/2016 16:30'!
readCentralDirectoryFileHeaderFrom: aStream
	"Assumes aStream positioned after signature"

	| fileNameLength extraFieldLength fileCommentLength |

	versionMadeBy _ aStream next.
	fileAttributeFormat _ aStream next.

	versionNeededToExtract _ aStream nextUnsignedInt16BigEndian: false.
	bitFlag _ aStream nextUnsignedInt16BigEndian: false.
	compressionMethod _ aStream nextUnsignedInt16BigEndian: false.

	lastModFileDateTime _ aStream nextUnsignedInt32BigEndian: false.
	crc32 _ aStream nextUnsignedInt32BigEndian: false.
	compressedSize _ aStream nextUnsignedInt32BigEndian: false.
	uncompressedSize _ aStream nextUnsignedInt32BigEndian: false.

	fileNameLength _ aStream nextUnsignedInt16BigEndian: false.
	extraFieldLength _ aStream nextUnsignedInt16BigEndian: false.
	fileCommentLength _ aStream nextUnsignedInt16BigEndian: false.
	aStream nextUnsignedInt16BigEndian: false. 	"disk number start"
	internalFileAttributes _ aStream nextUnsignedInt16BigEndian: false.

	externalFileAttributes _ aStream nextUnsignedInt32BigEndian: false.
	localHeaderRelativeOffset _ aStream nextUnsignedInt32BigEndian: false.

	fileName _ (aStream next: fileNameLength) asString.
	cdExtraField _ (aStream next: extraFieldLength) asByteArray.
	fileComment _ (aStream next: fileCommentLength) asString.

	self desiredCompressionMethod: compressionMethod! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'nk 11/11/2002 21:48'!
readFrom: aStream 
	"assumes aStream positioned after CD header; leaves stream positioned after my CD entry"

	self readCentralDirectoryFileHeaderFrom: aStream.
	self readLocalDirectoryFileHeaderFrom: aStream.
	self endRead.
	self canonicalizeFileName.
! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'jmv 2/26/2016 16:31'!
readLocalDirectoryFileHeaderFrom: aStream 
	"Positions stream as necessary. Will return stream to its original position"

	| fileNameLength extraFieldLength xcrc32 xcompressedSize xuncompressedSize sig oldPos |

	oldPos _ aStream position.

	aStream position: localHeaderRelativeOffset.

	sig _ aStream next: 4.
	sig = LocalFileHeaderSignature asByteArray
		ifFalse: [ aStream position: oldPos.
				^self error: 'bad LH signature at ', localHeaderRelativeOffset hex ].

	versionNeededToExtract _ aStream nextUnsignedInt16BigEndian: false.
	bitFlag _ aStream nextUnsignedInt16BigEndian: false.
	compressionMethod _ aStream nextUnsignedInt16BigEndian: false.

	lastModFileDateTime _ aStream nextUnsignedInt32BigEndian: false.
	xcrc32 _ aStream nextUnsignedInt32BigEndian: false.
	xcompressedSize _ aStream nextUnsignedInt32BigEndian: false.
	xuncompressedSize _ aStream nextUnsignedInt32BigEndian: false.

	fileNameLength _ aStream nextUnsignedInt16BigEndian: false.
	extraFieldLength _ aStream nextUnsignedInt16BigEndian: false.

	fileName _ (aStream next: fileNameLength) asString.
	localExtraField _ (aStream next: extraFieldLength) asByteArray.

	dataOffset _ aStream position.

	"Don't trash these fields if we already got them from the central directory"
	self hasDataDescriptor ifFalse: [
		crc32 _ xcrc32.
		compressedSize _ xcompressedSize.
		uncompressedSize _ xuncompressedSize.
	].

	aStream position: oldPos.! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'nk 2/22/2001 20:46'!
readRawChunk: n
	^stream next: n! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'nk 2/23/2001 09:56'!
rewindData
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ self error: 'stream missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4).
	self skipLocalDirectoryFileHeaderFrom: stream.! !

!ZipFileMember methodsFor: 'private-reading' stamp: 'jmv 2/26/2016 16:31'!
skipLocalDirectoryFileHeaderFrom: aStream 
	"Assumes that stream is positioned after signature."

	|  extraFieldLength fileNameLength |
	aStream next: 22.
	fileNameLength _ aStream nextUnsignedInt16BigEndian: false.
	extraFieldLength _ aStream nextUnsignedInt16BigEndian: false.
	aStream next: fileNameLength.
	aStream next: extraFieldLength.
	dataOffset _ aStream position! !

!ZipFileMember methodsFor: 'initialization' stamp: 'ar 3/2/2001 18:46'!
close
	stream ifNotNil:[stream close].! !

!ZipFileMember methodsFor: 'initialization' stamp: 'nk 2/22/2001 16:52'!
initialize
	super initialize.
	crc32 _ 0.
	localHeaderRelativeOffset _ 0.
	dataOffset _ 0.! !

!ZipFileMember methodsFor: 'initialization' stamp: 'nk 2/22/2001 16:51'!
stream: aStream externalFileName: aFileName
	stream _ aStream.
	externalFileName _ aFileName.! !

!ZipFileMember methodsFor: 'private-writing' stamp: 'nk 2/23/2001 11:04'!
copyDataTo: aStream

	self copyRawDataTo: aStream.! !

!ZipFileMember methodsFor: 'private-writing' stamp: 'nk 3/27/2002 11:20'!
localHeaderRelativeOffset
	^localHeaderRelativeOffset! !

!ZipFileMember methodsFor: 'private-writing' stamp: 'nk 3/7/2004 16:08'!
uncompressDataTo: aStream

	| decoder buffer chunkSize crcErrorMessage |
	decoder _ ZipReadStream on: stream.
	decoder expectedCrc: self crc32.
	buffer _ ByteArray new: (32768 min: readDataRemaining).
	crcErrorMessage _ nil.

	[[ readDataRemaining > 0 ] whileTrue: [
		chunkSize _ 32768 min: readDataRemaining.
		buffer _ decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining _ readDataRemaining - chunkSize.
	]] on: CRCError do: [ :ex | crcErrorMessage _ ex messageText. ex proceed ].

	crcErrorMessage ifNotNil: [ self isCorrupt: true. CRCError signal: crcErrorMessage ]

! !

!ZipFileMember methodsFor: 'private-writing' stamp: 'nk 2/24/2001 17:52'!
uncompressDataTo: aStream from: start to: finish

	| decoder buffer chunkSize |
	decoder _ FastInflateStream on: stream.
	readDataRemaining _ readDataRemaining min: finish - start + 1.
	buffer _ ByteArray new: (32768 min: readDataRemaining).
	decoder next: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [
		chunkSize _ 32768 min: readDataRemaining.
		buffer _ decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining _ readDataRemaining - chunkSize.
	].
! !

!ZipFileMember methodsFor: 'testing' stamp: 'nk 2/21/2001 21:52'!
looksLikeDirectory
	^fileName last = $/
		and: [ uncompressedSize = 0 ]! !

!ZipFileMember methodsFor: 'testing' stamp: 'jmv 10/14/2015 17:28'!
usesFileNamed: aFileName
	"Do I require aFileName? That is, do I care if it's clobbered?"

	^externalFileName asFileEntry = aFileName asFileEntry! !

!ZipFileMember class methodsFor: 'as yet unclassified' stamp: 'nk 2/22/2001 17:31'!
newFrom: stream named: fileName
	^(self new) stream: stream externalFileName: fileName! !

!ZipDirectoryMember methodsFor: 'private' stamp: 'nk 2/21/2001 21:55'!
asDirectory
	^self! !

!ZipDirectoryMember methodsFor: 'private' stamp: 'nk 3/27/2002 11:30'!
rewindData! !

!ZipDirectoryMember methodsFor: 'private' stamp: 'cmm 9/16/2010 18:59'!
writeDataTo: aStream
       "Write nothing. Directories have no contents to write."! !

!ZipDirectoryMember methodsFor: 'accessing' stamp: 'nk 2/23/2001 10:00'!
desiredCompressionMethod: aNumber! !

!ZipDirectoryMember methodsFor: 'accessing' stamp: 'jmv 10/14/2015 17:34'!
localFileName: aString
	| fe |
	super localFileName: aString.
	fileName last = $/ ifFalse: [ fileName _ fileName, '/' ].
	fe _ fileName asDirectoryEntry.
	fe exists ifTrue: [
		self setLastModFileDateTimeFrom: fe modificationTime ]! !

!ZipDirectoryMember methodsFor: 'initialization' stamp: 'nk 2/23/2001 10:01'!
initialize
	super initialize.
	super desiredCompressionMethod: CompressionStored.! !

!ZipDirectoryMember methodsFor: 'testing' stamp: 'nk 2/22/2001 00:00'!
isDirectory
	^true! !

!ZipDirectoryMember methodsFor: 'testing' stamp: 'nk 3/27/2002 11:29'!
usesFileNamed: aName
	^false! !

!ZipDirectoryMember class methodsFor: 'as yet unclassified' stamp: 'nk 12/20/2002 14:57'!
newNamed: aFileName
	^(self new) localFileName: aFileName; yourself! !

!ZipNewFileMember methodsFor: 'initialization' stamp: 'ar 3/2/2001 18:50'!
close
	stream ifNotNil:[stream close].! !

!ZipNewFileMember methodsFor: 'initialization' stamp: 'pb 5/25/2016 01:51'!
from: aFileName
	| entry |
	compressionMethod _ CompressionStored.
	"Now get the size, attributes, and timestamps, and see if the file exists"
	stream _ aFileName asFileEntry readStream.
	self localFileName: (externalFileName _ stream name).
	entry _ aFileName asFileEntry.
	compressedSize _ uncompressedSize _ entry fileSize.
	desiredCompressionMethod _ compressedSize > 0 ifTrue: [ CompressionDeflated ] ifFalse: [ CompressionStored ].
	self setLastModFileDateTimeFrom: entry modificationTime
! !

!ZipNewFileMember methodsFor: 'initialization' stamp: 'nk 2/22/2001 16:56'!
initialize
	super initialize.
	externalFileName _ ''.! !

!ZipNewFileMember methodsFor: 'private' stamp: 'nk 2/22/2001 20:48'!
readRawChunk: n
	^stream next: n! !

!ZipNewFileMember methodsFor: 'private-writing' stamp: 'nk 2/23/2001 09:58'!
rewindData
	super rewindData.
	readDataRemaining _ stream size.
	stream position: 0.! !

!ZipNewFileMember methodsFor: 'testing' stamp: 'jmv 10/14/2015 17:28'!
usesFileNamed: aFileName
	"Do I require aFileName? That is, do I care if it's clobbered?"

	^externalFileName asFileEntry = aFileName asFileEntry! !

!ZipNewFileMember class methodsFor: 'instance creation' stamp: 'nk 2/22/2001 17:27'!
newNamed: aFileName
	^(self new) from: aFileName! !

!ZipStringMember methodsFor: 'initialization' stamp: 'nk 2/22/2001 16:47'!
contents
	^contents! !

!ZipStringMember methodsFor: 'initialization' stamp: 'jmv 4/17/2013 11:57'!
contents: aString
	contents _ aString.
	compressedSize _ uncompressedSize _ aString size.
	"set the file date to now"
	self setLastModFileDateTimeFrom: Time localSecondClock! !

!ZipStringMember methodsFor: 'initialization' stamp: 'nk 2/22/2001 20:50'!
initialize
	super initialize.
	self contents: ''.
	compressionMethod _ desiredCompressionMethod _ CompressionStored.
! !

!ZipStringMember methodsFor: 'private' stamp: 'nk 2/22/2001 20:51'!
readRawChunk: n
	^stream next: n! !

!ZipStringMember methodsFor: 'private-writing' stamp: 'nk 2/23/2001 10:28'!
rewindData
	super rewindData.
	stream _ ReadStream on: contents.
	readDataRemaining _ contents size.! !

!ZipStringMember class methodsFor: 'as yet unclassified' stamp: 'nk 12/20/2002 15:06'!
newFrom: aString named: aFileName
	^(self new) contents: aString; localFileName: aFileName; yourself! !

!CodeFileBrowser class methodsFor: '*Compression' stamp: 'jmv 10/2/2015 16:55'!
browseMCZCode: aFileEntry
	"Browse the selected file."

	CodeFileBrowserWindow browseMCZFile: aFileEntry! !

!CodeFileBrowser class methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:26:10'!
serviceBrowseMCZCode
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'browse code'
		selector: #browseMCZCode:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			argumentGetter: [ :fileList |
				fileList selectedFileEntry ]! !

!ChangeList class methodsFor: '*Compression' stamp: 'jmv 4/4/2020 12:59:40'!
browseMCZContents: aFileEntry
	"Browse the selected file."
	|  unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck.st files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aFileEntry pathName.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	aFileEntry readStreamDo: [ :stream |
		unzipped _ stream asUnZippedStream: 'snapshot/source.st'.
		unzipped ascii.
		changeList _ self new scanFile: unzipped from: 0 to: unzipped size.
		stream reset.
		packageFile _ CodePackageFile new.
		packageFile
			buildFileStream: unzipped
			packageName: pkName
			fullName: fullName.
	].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new className: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry pathName! !

!ChangeList class methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:27:25'!
serviceMCZContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'see file contents'
		selector: #browseMCZContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents'
		icon: #changesIcon)
			argumentGetter: [ :fileList |
				fileList selectedFileEntry ]! !

!FileList methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:55:02'!
compressFile
	"Compress the currently selected file"

	self fullName asFileEntry readStream compressFile.
	self updateFileList! !

!FileList methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:38:30'!
serviceCompressFile
	"Answer a service for compressing a file"

	^ SimpleServiceEntry
		provider: self
		label: 'compress'
		selector: #compressFile
		description: 'compress file'
		buttonLabel: 'compress'
		icon: #goBottomIcon! !

!ArrayedCollection methodsFor: '*Compression' stamp: 'tk 3/7/2001 18:07'!
writeOnGZIPByteStream: aStream 
	"We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^ super writeOnGZIPByteStream: aStream].
		"super may cause an error, but will not be called."
	
	aStream nextPutAllWordArray: self! !

!String methodsFor: '*Compression' stamp: 'jmv 9/5/2016 22:13:54'!
unzipped
	|  magic1 magic2 |
	magic1 _ (self at: 1) numericValue.
	magic2 _ (self at: 2) numericValue.
	(magic1 = 16r1F and:[magic2 = 16r8B]) ifFalse: [ ^self ].
	^ (GZipReadStream on: self) upToEnd! !

!PositionableStream methodsFor: '*Compression' stamp: 'jmv 6/10/2013 20:40'!
asZLibReadStream
	^ ZLibReadStream on: collection from: position+1 to: readLimit! !

!ReadWriteStream methodsFor: '*Compression' stamp: 'jmv 6/10/2013 20:39'!
asUnZippedStream
	|  isGZip outputStream first strm |
	"Decompress this file if needed, and return a stream.  No file is written.  File extension may be .gz or anything else."

	strm _ self binary.

	first _ strm next.
	isGZip _ (strm next * 256 + first) = (GZipConstants gzipMagic).
	strm skip: -2.
	isGZip 
		ifTrue: [ outputStream _ (RWBinaryOrTextStream with:
									(GZipReadStream on: strm) upToEnd) reset.
				strm close]
		ifFalse: [ outputStream _ strm].
	^ outputStream! !

!ReadWriteStream methodsFor: '*Compression' stamp: 'jmv 6/10/2013 20:40'!
asUnZippedStream: memberFileName
	|  strm archive which |
	"Decompress this file if needed, and return a stream.  No file is written.  File extension may be .gz or anything else.  Also works on archives (.zip, .gZip)."

	strm _ self binary.
	strm isZipArchive ifFalse: [
		^self error: 'Not a Zip file' ].
	
	archive _ ZipArchive new readFrom: strm.
	which _ archive members 
		detect: [:any | any fileName  = memberFileName] 
		ifNone: nil.
	which ifNil: [
		archive close.
		^ self error: 'Can''t find requested file in archive'].
	strm _ which contentStream.
	archive close.
	^ strm! !

!ReadWriteStream methodsFor: '*Compression' stamp: 'jmv 6/10/2013 20:40'!
isZipArchive
	"Determine if this appears to be a valid Zip archive"
	|  sig |
	self binary.
	sig _ (self next: 4) asString.
	self position: self position - 4. "rewind"
	^ (ZipArchive validSignatures includes: sig)! !

!FileStream methodsFor: '*Compression' stamp: 'jmv 6/10/2013 20:39'!
viewGZipContents
	"View the contents of a gzipped file"

	|  stringContents |
	self binary.
	stringContents _ self contentsOfEntireFile.
	stringContents _ (GZipReadStream on: stringContents) upToEnd.
	stringContents _ stringContents asString withCuisLineEndings.

	Workspace new
		contents: stringContents;
		openLabel: 'Decompressed contents of: ', self localName! !

!StandardFileStream methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:57:48'!
compressFile
	"Write a new file that has the data in me compressed in GZip format."
	|  zipped buffer |

	self readOnly; binary.
	zipped _ (self name, '.gz') asFileEntry writeStream.
	zipped binary.
	zipped _ GZipWriteStream on: zipped.
	buffer _ ByteArray new: 50000.
	'Compressing ', self fullName displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			[self atEnd] whileFalse: [
				barBlock value: self position.
				zipped nextPutAll: (self nextInto: buffer)].
			zipped close.
			self close ].
	 ^zipped! !

!CodeFileBrowserWindow class methodsFor: '*Compression' stamp: 'jmv 3/24/2020 11:20:46'!
browseMCZFile: aFileEntry

	| codeFile unzipped |
	aFileEntry readStreamDo: [ :stream |
		unzipped _ stream asUnZippedStream: 'snapshot/source.st'.
		unzipped ascii.
		codeFile _ (CodeFile new fullName: aFileEntry pathName; buildFrom: unzipped) ].
	CodeFileBrowser browseCodeFileEntry: codeFile! !

!CodePackageFile class methodsFor: '*Compression' stamp: 'jmv 10/2/2015 16:55'!
browseMCZCode: aFileEntry
	"Browse the selected file."

	CodeFileBrowserWindow browseMCZFile: aFileEntry! !

!CodePackageFile class methodsFor: '*Compression' stamp: 'jmv 4/4/2020 12:57:16'!
installMonticelloPackage: aFileEntry
	
	| fullName pkName unzip codePackageFile |
	fullName _ aFileEntry pathName.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	aFileEntry readStreamDo: [ :stream |
		unzip _ stream asUnZippedStream: 'snapshot/source.st'.
		unzip ascii.
		codePackageFile _ CodePackageFile new.
		codePackageFile
			buildFileStream: unzip
			packageName: pkName
			fullName: fullName.
		codePackageFile install ]! !

!CodePackageFile class methodsFor: '*Compression' stamp: 'jmv 11/3/2016 11:38:49'!
monticelloPackageNameFrom: fullName
	| localName |
	localName _ fullName asFileEntry name.
	^(localName prefixAndSuffix: $-)
		ifNotNil: [ :ary | ary first ]
		ifNil: [ localName withoutSuffix: '.mcz' ]! !

!CodePackageFile class methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:28:36'!
serviceBrowseMCZCode
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'browse code'
		selector: #browseMCZCode:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code'
		icon: #editFindReplaceIcon)
			argumentGetter: [ :fileList | fileList selectedFileEntry ] ! !

!CodePackageFile class methodsFor: '*Compression' stamp: 'HAW 1/2/2019 18:29:13'!
serviceInstallMonticelloPackage
	"Answer the service of installing a Monticello package file (Squeak's compressed chunk format file for a package)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install Monticello package'
		selector: #installMonticelloPackage:
		description: 'install the Monticello package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package'
		icon: #saveIcon)
			argumentGetter: [ :fileList | fileList selectedFileEntry ] ! !
InflateStream initialize!
FastInflateStream initialize!
ZipWriteStream initialize!
GZipConstants initialize!
ZipConstants initialize!
ZipFileConstants initialize!
