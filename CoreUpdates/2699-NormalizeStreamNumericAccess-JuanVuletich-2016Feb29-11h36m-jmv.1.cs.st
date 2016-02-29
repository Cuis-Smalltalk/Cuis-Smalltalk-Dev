'From Cuis 4.2 of 25 July 2013 [latest update: #2692] on 29 February 2016 at 11:39:41.661671 am'!

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 2/29/2016 11:19'!
nextUnsignedInt32
	"Read a 32-bit unsigned quantity from the stream.
	Big Endian"

	^ stream nextUnsignedInt32BigEndian: true! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 2/29/2016 11:19'!
nextUnsignedInt32Put: aNumber
	"Write out a 32-bit integer as 32 bits.
	Big Endian"

	stream nextUnsignedInt32Put: aNumber bigEndian: true.
	^aNumber! !


!DataStream methodsFor: 'other' stamp: 'jmv 2/29/2016 10:59'!
errorRead
    "PRIVATE -- Raise an error because this case of next's perform:
     shouldn't be called."

    self error: 'This should never be called'! !


!ArrayedCollection methodsFor: 'objects from disk' stamp: 'jmv 2/26/2016 16:18'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	self class isPointers | self class isWords not ifTrue: [^ super writeOn: aStream].
				"super may cause an error, but will not be called."
	aStream nextSignedInt32Put: self basicSize bigEndian: true.
	aStream nextWordsPutAll: self.! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 2/26/2016 16:10'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	self isPointers | self isWords not ifTrue: [^ super newFromStream: s].
		"super may cause an error, but will not be called."

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skip: -1.
	len _ s nextSignedInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !


!Bitmap class methodsFor: 'instance creation' stamp: 'jmv 2/26/2016 16:11'!
newFromStream: s
	| len |
	len _ s nextSignedInt32BigEndian: true.
	^ s nextWordsInto: (self new: len)! !


!Float64Array methodsFor: 'objects from disk' stamp: 'jmv 2/26/2016 16:14'!
writeOn: aStream
	"Store self on the argument, aStream.
	Write bigEndian / PowerPC order."

	aStream nextUnsignedInt32Put: self size bigEndian: true.
	self do: [ :f |
		aStream nextDouble64Put: f bigEndian: true ]! !


!Float64Array class methodsFor: 'objects from disk' stamp: 'jmv 2/26/2016 16:14'!
newFromStream: aStream
	"Read bigEndian / PowerPC order."

	| len answer |
	len _ aStream nextUnsignedInt32BigEndian: true.
	answer _ self new: len.
	1 to: len do: [ :i |
		answer at: i put: (aStream nextDouble64BigEndian: true) ].
	^ answer! !


!RunArray methodsFor: 'printing' stamp: 'jmv 2/26/2016 16:23'!
writeOn: aStream

	aStream nextUnsignedInt16Put: runs size bigEndian: true.
	1 to: runs size do: [ :x |
		aStream nextUnsignedInt16Put: (runs at: x) bigEndian: true.
		aStream nextUnsignedInt16Put: (values at: x) bigEndian: true ]! !


!RunArray class methodsFor: 'instance creation' stamp: 'jmv 2/26/2016 16:22'!
readFrom: aStream
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size _ aStream nextUnsignedInt16BigEndian: true.
	runs _ Array new: size.
	values _ Array new: size.
	1 to: size do: [ :x |
		runs at: x put: (aStream nextUnsignedInt16BigEndian: true).
		values at: x put: (aStream nextUnsignedInt16BigEndian: true) ].
	^ self runs: runs values: values! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 2/29/2016 10:49'!
readAttributesFrom: aBinaryStream
	| offsetX offsetY |
	depth _ aBinaryStream next.
	(self depth isPowerOfTwo and: [self depth between: 1 and: 32])
		ifFalse: [self error: 'invalid depth; bad Form file?'].
	width _ aBinaryStream nextUnsignedInt16BigEndian: true.
	height _ aBinaryStream nextUnsignedInt16BigEndian: true.
	offsetX  _ aBinaryStream nextSignedInt16BigEndian: true.
	offsetY _ aBinaryStream nextSignedInt16BigEndian: true.
	offset _ Point x: offsetX y: offsetY! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 2/29/2016 10:49'!
writeAttributesOn: file

	file nextPut: depth.
	file nextUnsignedInt16Put: width bigEndian: true.
	file nextUnsignedInt16Put: height bigEndian: true.
	file nextSignedInt16Put: self offset x bigEndian: true.
	file nextSignedInt16Put: self offset y bigEndian: true! !


!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 2/26/2016 16:27'!
readAttributesFrom: aBinaryStream
	super readAttributesFrom: aBinaryStream.
	colors _ ColorArray new: (2 raisedTo: depth).
	1 to: colors size do: [:idx | 
		colors basicAt: idx put: (aBinaryStream nextUnsignedInt32BigEndian: false) ]! !

!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 2/26/2016 16:36'!
writeAttributesOn: file
	| colorArray |
	super writeAttributesOn: file.
	colorArray _ self colors asColorArray.
	1 to: (2 raisedTo: depth) do: [:idx |
		file nextUnsignedInt32Put: (colorArray basicAt: idx) bigEndian: false ]! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 2/26/2016 16:27'!
readHeader
	| reserved |
	bfType _ stream nextUnsignedInt16BigEndian: false.
	bfSize _ stream nextUnsignedInt32BigEndian: false.
	reserved _ stream nextUnsignedInt32BigEndian: false.
	bfOffBits _ stream nextUnsignedInt32BigEndian: false.
	biSize _ stream nextUnsignedInt32BigEndian: false.
	biWidth _ stream nextUnsignedInt32BigEndian: false.
	biHeight _ stream nextUnsignedInt32BigEndian: false.
	biPlanes _ stream nextUnsignedInt16BigEndian: false.
	biBitCount _ stream nextUnsignedInt16BigEndian: false.
	biCompression _ stream nextUnsignedInt32BigEndian: false.
	biSizeImage _ stream nextUnsignedInt32BigEndian: false.
	biXPelsPerMeter _ stream nextUnsignedInt32BigEndian: false.
	biYPelsPerMeter _ stream nextUnsignedInt32BigEndian: false.
	biClrUsed _ stream nextUnsignedInt32BigEndian: false.
	biClrImportant _ stream nextUnsignedInt32BigEndian: false.
! !

!BMPReadWriter methodsFor: 'writing' stamp: 'jmv 2/26/2016 16:36'!
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image ppw scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	stream position: 0.
	stream nextUnsignedInt16Put: 19778 bigEndian: false.  "bfType = BM"
	stream nextUnsignedInt32Put: bfOffBits + biSizeImage bigEndian: false.  "Entire file size in bytes"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "bfReserved"
	stream nextUnsignedInt32Put: bfOffBits bigEndian: false.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextUnsignedInt32Put: biSize bigEndian: false.  "info header size in bytes"
	stream nextUnsignedInt32Put: image width bigEndian: false.  "biWidth"
	stream nextUnsignedInt32Put: image height bigEndian: false.  "biHeight"
	stream nextUnsignedInt16Put: 1 bigEndian: false.  "biPlanes"
	stream nextUnsignedInt16Put: (depth min: 24) bigEndian: false.  "biBitCount"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biCompression"
	stream nextUnsignedInt32Put: biSizeImage bigEndian: false.  "size of image section in bytes"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biXPelsPerMeter"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biYPelsPerMeter"
	stream nextUnsignedInt32Put: biClrUsed bigEndian: false.
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [:i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [:j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth < 32 ifTrue: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		ppw := 32 // depth.
		scanLineLen := biWidth + ppw - 1 // ppw * 4.  "# of bytes in line"
		1 to: biHeight do: [:i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	] ifFalse: [
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do: [ :i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure']! !


!Stream methodsFor: 'testing' stamp: 'jmv 2/26/2016 17:09'!
nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextUnsignedInt32Put: (aCollection basicAt: i) bigEndian: true ].
	^aCollection! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:04'!
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextUnsignedInt32Put: anInteger + 1 bigEndian: true.

	self nextPut: aClass name! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/29/2016 10:59'!
next
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type _ byteStream next.
	type ifNil: [pos _ byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		pos.	"so can see it in debugger"
		^ nil].
	type = 0 ifTrue: [pos _ byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType _ self noteCurrentReference: type.
	selector _ #(readNil readTrue readFalse readInteger	"<-4"
			readStringOld readSymbol readByteArray		"<-7"
			readArray readInstance readReference readBitmap	"<-11"
			readClass readUser readFloat errorRead readShortInst 	"<-16"
			readString readWordArray readWordArrayForSegment 	"<-19"
			readWordLike readMethod "<-21") at: type.
	selector = 0 ifTrue: [pos _ byteStream position.	"absolute!!!!"
			byteStream close. 
			self error: 'file is more recent than this system'. ^ nil].
	anObject _ self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."
	isARefType ifTrue: [self beginReference: anObject].

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject _ anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/29/2016 10:57'!
nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID _ self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore _ (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID _ self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector _ #(writeNil: writeTrue: writeFalse: writeInteger: 
		writeStringOld: writeSymbol: writeByteArray:
		writeArray: writeInstance: errorWriteReference: writeBitmap:
		writeClass: writeUser: writeFloat: errorWriteReference: == "<-16 short inst" 
		writeString: writeBitmap: writeBitmap: writeWordLike: 
		writeInstance: "CompiledMethod") at: typeID.
	self perform: selector with: objectToStore.

	^ anObject


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod and ViewState), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of ViewStates, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:04'!
outputReference: referencePosn
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextUnsignedInt32Put: referencePosn bigEndian: true	"relative position"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:00'!
readArray
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count _ byteStream nextUnsignedInt32BigEndian: true.

	refPosn _ self beginReference: (array _ Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:00'!
readByteArray
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count _ byteStream nextUnsignedInt32BigEndian: true.
	^ byteStream next: count  "assume stream is in binary mode"
! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 16:59'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order."

	^ byteStream nextDouble64BigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:00'!
readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	aSymbol _ self next.
	newClass _ Smalltalk at: aSymbol asSymbol.
	anObject _ newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject _ anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 16:11'!
readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextSignedInt32BigEndian: true	"signed!!!!!!"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:00'!
readMethod
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	className _ self next.
	newClass _ Smalltalk at: className asSymbol.

	xxHeader _ self next.
		"nArgs _ (xxHeader >> 24) bitAnd: 16rF."
		"nTemps _ (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit _ (xxHeader >> 17) bitAnd: 1."
	nLits _ (xxHeader >> 9) bitAnd: 16rFF.
		"primBits _ ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer _ instSize - (newClass instSize "0") - (nLits + 1 * 4).

	newMethod _ newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits _ newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*4+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:01'!
readReference
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |

	^ (referencePosition _ (byteStream nextUnsignedInt32BigEndian: true)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 16:59'!
readShortRef
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextUnsignedInt16BigEndian: true)! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:04'!
writeArray: anArray
	"PRIVATE -- Write the contents of an Array."

	byteStream nextUnsignedInt32Put: anArray size bigEndian: true.
	self nextPutAll: anArray.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:05'!
writeByteArray: aByteArray
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextUnsignedInt32Put: aByteArray size bigEndian: true.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 17:16'!
writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here.
	Write in bigEndian / PowerPC order."

	byteStream nextDouble64Put: aFloat bigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 2/26/2016 16:18'!
writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextSignedInt32Put: anInteger bigEndian: true	"signed!!!!!!!!!!"! !

!DataStream methodsFor: 'other' stamp: 'jmv 2/29/2016 10:57'!
errorWriteReference: anInteger
    "PRIVATE -- Raise an error because this case of nextPut:'s perform:
     shouldn't be called. -- 11/15/92 jhm"

    self error: 'This should never be called'! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 2/29/2016 10:56'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"DataStream initialize"

	| refTypes t |
	refTypes _ OrderedCollection new.
	t _ TypeMap _ Dictionary new: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.   refTypes add: 0.
	t at: True put: 2.   refTypes add: 0.
	t at: False put: 3.   refTypes add: 0.
	t at: SmallInteger put: 4.	 refTypes add: 0.
	t at: String put: 5.   refTypes add: 1.
	t at: Symbol put: 6.   refTypes add: 1.
	t at: ByteArray put: 7.   refTypes add: 1.
	t at: Array put: 8.   refTypes add: 1.
	"(type ID 9 is for arbitrary instances of any class, cf. typeIDFor:)"
		refTypes add: 1.
	"(type ID 10 is for references, cf. ReferenceStream>>tryToPutReference:)"
		refTypes add: 0.
	t at: Bitmap put: 11.   refTypes add: 1.
	t at: Metaclass put: 12.   refTypes add: 0.
	"Type ID 13 is used for HyperSqueak User classes that must be reconstructed."
		refTypes add: 1.
	t at: Float put: 14.  refTypes add: 1.
	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.
	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19.  refTypes add: 1."		"bitmap-like"
	t at: FloatArray put: 20.  refTypes add: 1.	"And all other word arrays, both 16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"
	"t at:  put: 22.  refTypes add: 0."
	ReferenceTypes _ refTypes.		"save it"! !


!ReferenceStream methodsFor: 'writing' stamp: 'jmv 2/26/2016 17:07'!
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."
	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.
	In ReferenceStream, class is just 5 bytes for shared symbol.
	SmartRefStream puts out the names and number of class's instances variables for checking.
6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "

	| short ref |
	short _ true.	"All tests for object header that can be written in 4 bytes"
	anInteger <= 254 ifFalse: [short _ false].	"one byte size"
	ref _ references at: aClass name ifAbsent: [short _ false. nil].
	ref isInteger ifFalse: [short _ false].
	short ifTrue: [short _ (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"
	short ifTrue: [
		byteStream skip: -1.
		short _ byteStream next = 9.
		byteStream skip: 0].	"ugly workaround"
	short 
		ifTrue: ["passed all the tests!!"
			byteStream skip: -1; nextPut: 16; "type = short header"
				nextPut: anInteger + 1;	"size is short"
				nextUnsignedInt16Put: ref bigEndian: true ]
		ifFalse: [
			"default to normal longer object header"
			byteStream nextUnsignedInt32Put: anInteger + 1 bigEndian: true.
			self nextPut: aClass name].! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 2/26/2016 17:01'!
readInstance
	"Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!!  See setStream:"
	| instSize className refPosn |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	className _ self next asSymbol.
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn
! !


!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 2/29/2016 11:04'!
nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger.
	Currently only for bigEndian. Consider following the convention in category 'normalized access'"
	| s |
	s _ 0.
	1 to: n do: 
		[:i | s _ (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 2/29/2016 11:04'!
nextNumber: n put: v 
	"Append to the receiver the argument, v, which is a positive 
	SmallInteger or a LargePositiveInteger, as the next n bytes.
	Possibly pad with leading zeros.
	Currently only for bigEndian. Consider following the convention in category 'normalized access'"

	1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
	^ v
! !

!methodRemoval: PositionableStream #boolean!
PositionableStream removeSelector: #boolean!
!methodRemoval: PositionableStream #boolean:!
PositionableStream removeSelector: #boolean:!
!methodRemoval: PositionableStream #int16!
PositionableStream removeSelector: #int16!
!methodRemoval: PositionableStream #int16:!
PositionableStream removeSelector: #int16:!
!methodRemoval: PositionableStream #int32!
PositionableStream removeSelector: #int32!
!methodRemoval: PositionableStream #int32:!
PositionableStream removeSelector: #int32:!
!methodRemoval: PositionableStream #nextInt32!
PositionableStream removeSelector: #nextInt32!
!methodRemoval: PositionableStream #nextInt32Put:!
PositionableStream removeSelector: #nextInt32Put:!
!methodRemoval: PositionableStream #nextLittleEndianNumber:!
PositionableStream removeSelector: #nextLittleEndianNumber:!
!methodRemoval: PositionableStream #nextLittleEndianNumber:put:!
PositionableStream removeSelector: #nextLittleEndianNumber:put:!
!methodRemoval: PositionableStream #nextWord!
PositionableStream removeSelector: #nextWord!
!methodRemoval: PositionableStream #nextWordPut:!
PositionableStream removeSelector: #nextWordPut:!
!methodRemoval: PositionableStream #string!
PositionableStream removeSelector: #string!
!methodRemoval: PositionableStream #string:!
PositionableStream removeSelector: #string:!
!methodRemoval: PositionableStream #uint16!
PositionableStream removeSelector: #uint16!
!methodRemoval: PositionableStream #uint16:!
PositionableStream removeSelector: #uint16:!
!methodRemoval: PositionableStream #uint24!
PositionableStream removeSelector: #uint24!
!methodRemoval: PositionableStream #uint24:!
PositionableStream removeSelector: #uint24:!
!methodRemoval: PositionableStream #uint32!
PositionableStream removeSelector: #uint32!
!methodRemoval: PositionableStream #uint32:!
PositionableStream removeSelector: #uint32:!
!methodRemoval: DummyStream #nextInt32Put:!
DummyStream removeSelector: #nextInt32Put:!
!methodRemoval: DataStream #readRectangle!
DataStream removeSelector: #readRectangle!
!methodRemoval: DataStream #writeRectangle:!
DataStream removeSelector: #writeRectangle:!
!methodRemoval: ImageReadWriter #nextLong!
ImageReadWriter removeSelector: #nextLong!
!methodRemoval: ImageReadWriter #nextLongPut:!
ImageReadWriter removeSelector: #nextLongPut:!
!methodRemoval: ImageReadWriter #nextWord!
ImageReadWriter removeSelector: #nextWord!
!methodRemoval: ImageReadWriter #nextWordPut:!
ImageReadWriter removeSelector: #nextWordPut:!

!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isCharacters isEmpty notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: copyMethodChunkFrom: fileIn fileInAnnouncing: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('nonhomogeneous accessing' nextNumber nextNumber: nextNumber:put: nextString nextStringOld nextStringPut:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
('normalized access' nextDouble64BigEndian: nextDouble64Put:bigEndian: nextFloat32BigEndian: nextFloat32Put:bigEndian: nextSignedInt16BigEndian: nextSignedInt16Put:bigEndian: nextSignedInt32BigEndian: nextSignedInt32Put:bigEndian: nextUnsignedInt16BigEndian: nextUnsignedInt16Put:bigEndian: nextUnsignedInt32BigEndian: nextUnsignedInt32Put:bigEndian:)
!

