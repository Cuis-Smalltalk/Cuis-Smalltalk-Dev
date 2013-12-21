'From Cuis 4.2 of 25 July 2013 [latest update: #1880] on 21 December 2013 at 9:46:43.792112 am'!

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 12/21/2013 09:42'!
writeUncompressedOn: binaryStream
	"Write the receiver on the file in the format depth, extent, offset, bits.  Warning:  Caller must put header info on file!!  Use writeUncompressedOnFileNamed: instead."
	self unhibernate.
	binaryStream nextPut: depth.
	binaryStream nextWordPut: width.
	binaryStream nextWordPut: height.
	binaryStream nextWordPut: ((self offset x) >=0
					ifTrue: [self offset x]
					ifFalse: [self offset x + 65536]).
	binaryStream nextWordPut: ((self offset y) >=0
					ifTrue: [self offset y]
					ifFalse: [self offset y + 65536]).
	bits writeUncompressedOn: binaryStream! !

!methodRemoval: ImageReadWriter class #on:!
ImageReadWriter class removeSelector: #on:!
