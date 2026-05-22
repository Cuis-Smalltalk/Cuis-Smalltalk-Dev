"
This is the save-to-disk facility. A DataStream can store one or more objects in a persistent form.

To handle objects with sharing and cycles, you must use a
ReferenceStream instead of a DataStream.  (Or SmartRefStream.)  ReferenceStream is typically
faster and produces smaller files because it doesn't repeatedly write the same Symbols.

Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: 'test.obj'.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: 'test.obj'.
	<your object> := rr next.
	rr close.

Each object to be stored has two opportunities to control what gets stored.  On the high level, objectToStoreOnDataStream allows you to substitute another object on the way out.  The low level hook is storeDataOn:. The read-in counterparts to these messages are comeFullyUpOnReload and (class) readDataFrom:size:. See these methods, and the class DiskProxy, for more information about externalizing and internalizing.

NOTE: A DataStream should be treated as a write-stream for writing.  It is a read-stream for reading.  It is not a ReadWriteStream.

"
Class {
	#name : 'DataStream',
	#superclass : 'Stream',
	#instVars : [
		'byteStream',
		'topCall',
		'basePos'
	],
	#classVars : [
		'TypeMap'
	],
	#category : 'System-Object Storage',
	#package : 'System',
	#tag : 'Object Storage'
}

{ #category : 'examples' }
DataStream class >> example [
    "An example and test of DataStream/ReferenceStream.
     11/19/92 jhm: Use self testWith:."
    "DataStream example"
    "ReferenceStream example"
    | input sharedPoint |

    "Construct the test data."
    input := Array new: 9.
    input at: 1 put: nil.
    input at: 2 put: true.
    input at: 3 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 3) fillWithColor: `Color lightBlue`.
    input at: 4 put: #(3 3.0 'three').
    input at: 5 put: false.
    input at: 6 put: 1024 @ -2048.
    input at: 7 put: #x.
    input at: 8 put: (Array with: (sharedPoint := 0 @ -30000)).
    input at: 9 put: sharedPoint.

    "Write it out, read it back, and return it for inspection."
    ^ self testWith: input
]

{ #category : 'examples' }
DataStream class >> exampleWithPictures [
	"
	DataStream exampleWithPictures
	"
	| f result |
	f := Form fromUser.
	'Test-Picture' asFileEntry writeStreamDo: [ :file |
		file binary.
		(DataStream on: file) nextPut: f ].

	'Test-Picture' asFileEntry readStreamDo: [ :file |
		file binary.
		result := (DataStream on: file) next ].

	result display
]

{ #category : 'cached state access' }
DataStream class >> initCachedState [
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  #nextPut: writes these IDs to the data stream.
	NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"
	| t |
	t := TypeMap := Dictionary newWithRoomForMoreThan: 80. "sparse for fast hashing"

	t at: #UndefinedObject put: 1.
	t at: #True put: 2.
	t at: #False put: 3.
	t at: #SmallInteger32 put: 4. "Squeak's 32 bit SmallInteger format. See below #SmallInteger64."
	"(type ID 5 was for Squeak's old String format)"
	t at: #Symbol put: 6.
	t at: #ByteArray put: 7.
	t at: #Array put: 8.
	"(type ID 9 is for arbitrary instances of any class. See #typeIDFor: See #writeInstance: See #readInstance)"
	"(type ID 10 is for references, See ReferenceStream>>tryToPutReference:typeID:)"
	t at: #Bitmap put: 11.
	t at: #Metaclass put: 12.
	"Type ID 13 was for Squeak's  HyperSqueak User classes."
	t at: #Float put: 14.
	t at: #BoxedFloat64 put: 14.
	t at: #SmallFloat64 put: 14.
	"(type ID 15 was for Squeak's old Deprecated compact Rects."
	"type ID 16 is an instance with short header.  See #beginInstance:size:"
	t at: #String put: 17.	"'new' String format, 1 or 4 bytes of length"
	t at: #WordArray put: 18.
	"type ID 19 was for Squeak's Word Array for Segment."
	"For all classes that are like WordArrays, store them as bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isBehavior ifTrue: [
			cls isPointers not & cls isVariable & cls isWords ifTrue: [
				(t includesKey: cls name) ifFalse: [
					t at: cls name put: 20 ]]]].
	t at: #CompiledMethod put: 21.	"special creation method"
	t at: #Character put: 22.
	t at: #UnicodeString put: 23.
	t at: #UnicodeSymbol put: 24.
	t at: #SmallInteger64 put: 25. "Needed to handle the full 61 bit range of SmallIntegers in 64 bit Cuis images."
]

{ #category : 'instance creation' }
DataStream class >> new [
	^ self basicNew
]

{ #category : 'instance creation' }
DataStream class >> on: aStream [
	"Open a new DataStream onto a low-level I/O stream."

	^ self basicNew setStream: aStream
		"aStream binary is in setStream:"

]

{ #category : 'cached state access' }
DataStream class >> releaseClassCachedState [

	TypeMap := nil
]

{ #category : 'services' }
DataStream class >> streamedRepresentationOf: anObject [

	| file |
	file := (ReadWriteStream on: (ByteArray new: 5000)).
	(self on: file) nextPut: anObject.
	^file contents
]

{ #category : 'examples' }
DataStream class >> testWith: anObject [
	"As a test of DataStream/ReferenceStream, write out anObject and read it back.
	11/19/92 jhm: Set the file type. More informative file name.
	DataStream testWith: 'hi'
	ReferenceStream testWith: 'hi'
	"
	| result |

	(self name, ' test') asFileEntry writeStreamDo: [ :file |
		file binary.
		(self on: file) nextPut: anObject ].

	(self name, ' test') asFileEntry readStreamDo: [ :file |
		file binary.
		result := (self on: file) next ].

	^ result
]

{ #category : 'cached state access' }
DataStream class >> typeMap [

	TypeMap ifNil: [ self initCachedState ].
	^TypeMap
]

{ #category : 'services' }
DataStream class >> unStream: aByteArray [

	^(self on: ((ReadWriteStream with: aByteArray) reset)) next
]

{ #category : 'other' }
DataStream >> atEnd [
    "Answer true if the stream is at the end."

    ^ byteStream atEnd
]

{ #category : 'write and read' }
DataStream >> beginInstance: aClass size: anInteger [
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextUint32Put: anInteger + 1 bigEndian: true.

	self nextPut: aClass name
]

{ #category : 'reading' }
DataStream >> beginReference: anObject [
    "WeÕre starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0
]

{ #category : 'other' }
DataStream >> byteStream [
	^ byteStream
]

{ #category : 'other' }
DataStream >> contents [
	^byteStream contents
]

{ #category : 'other' }
DataStream >> errorRead [
    "PRIVATE -- Raise an error because this case of next's perform:
     shouldn't be called."

    self error: 'This should never be called'
]

{ #category : 'other' }
DataStream >> errorWriteReference: anInteger [
    "PRIVATE -- Raise an error because this case of nextPut:'s perform:
     shouldn't be called. -- 11/15/92 jhm"

    self error: 'This should never be called'
]

{ #category : 'other' }
DataStream >> flush [
    "Guarantee that any writes to me are actually recorded on disk. -- 11/17/92 jhm"

    ^ byteStream flush
]

{ #category : 'reading' }
DataStream >> getCurrentReference [
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0
]

{ #category : 'reading' }
DataStream >> maybeBeginReference: internalObject [
	"Do nothing.  See ReferenceStream|maybeBeginReference:"

	^ internalObject
]

{ #category : 'reading' }
DataStream >> next [
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type := byteStream next.
	type ifNil: [
		pos := byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		pos.	"so can see it in debugger"
		^ nil].
	type = 0 ifTrue: [
		pos := byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType := self noteCurrentReference: type.
	selector := #(
			readNil readTrue readFalse readInteger32Bits
			errorRead readSymbol readByteArray readArray
			readInstance readReference readBitmap readClass
			errorRead readFloat errorRead readShortInst
			readString readWordArray errorRead readWordLike
			readMethod 
			readCharacter readUnicodeString readUnicodeSymbol
			readInteger64Bits) at: type.
	anObject := self perform: selector. "A method that recursively
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
	internalObject := anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject.
]

{ #category : 'other' }
DataStream >> next: anInteger [
    "Answer an Array of the next anInteger objects in the stream."
    | array |

    array := Array new: anInteger.
    1 to: anInteger do: [:i |
        array at: i put: self next].
    ^ array
]

{ #category : 'write and read' }
DataStream >> nextPut: anObject [
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore := (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := #(
		writeNil: writeTrue: writeFalse: writeInteger32Bits: 
		errorWriteReference: writeSymbol: writeByteArray: writeArray:
		writeInstance: errorWriteReference: writeBitmap: writeClass:
		errorWriteReference: writeFloat: errorWriteReference: errorWriteReference:
		writeString: writeBitmap: errorWriteReference: writeWordLike: 
		writeInstance: "CompiledMethod"
		writeCharacter: writeUnicodeString: writeUnicodeSymbol:
		writeInteger64Bits:) at: typeID.
	self perform: selector with: objectToStore.

	^ anObject


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of these, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."
]

{ #category : 'reading' }
DataStream >> noteCurrentReference: typeID [
    "PRIVATE -- If we support references for type typeID, remember
     the current byteStream position so we can add the next object to
     the ÔobjectsÕ dictionary, and return true. Else return false.
     This method is here to be overridden by ReferenceStream"

    ^ false
]

{ #category : 'reading' }
DataStream >> objectAt: anInteger [
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn := byteStream position.		"absolute"
	refPosn := self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject := self next.

	self setCurrentReference: refPosn.		"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject
]

{ #category : 'write and read' }
DataStream >> objectIfBlocked: anObject [
	"We don't do any blocking"

	^ anObject
]

{ #category : 'write and read' }
DataStream >> outputReference: referencePosn [
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextUint32Put: referencePosn bigEndian: true	"relative position"
]

{ #category : 'reading' }
DataStream >> readArray [
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count := byteStream nextUint32BigEndian: true.

	refPosn := self beginReference: (array := Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array
]

{ #category : 'write and read' }
DataStream >> readBitmap [
	"PRIVATE -- Read the contents of a Bitmap."

	^ Bitmap newFromStream: byteStream
	"Note that the reader knows that the size is in long words, but the data is in bytes."
]

{ #category : 'reading' }
DataStream >> readByteArray [
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count := byteStream nextUint32BigEndian: true.
	^ byteStream next: count  "assume stream is in binary mode"

]

{ #category : 'reading' }
DataStream >> readCharacter [
    "PRIVATE -- Read a Character."

    ^ Character codePoint: (byteStream nextUint32BigEndian: true)
]

{ #category : 'write and read' }
DataStream >> readClass [
	"Should never be executed because a DiskProxy, not a clas comes in."

	^ self error: 'Classes should be filed in'
]

{ #category : 'reading' }
DataStream >> readFalse [
    "PRIVATE -- Read the contents of a False."

    ^ false
]

{ #category : 'reading' }
DataStream >> readFloat [
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order.
	Answer is BoxedFloat64 or SmallFloat64 as appropriate."

	^ byteStream nextFloat64BigEndian: true
]

{ #category : 'reading' }
DataStream >> readInstance [
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk at: aSymbol asSymbol ifAbsent: [
		self error: 'Can not deserialize an instance of ', aSymbol, '. Class is missing.'.
		^nil ].
	anObject := newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
]

{ #category : 'reading' }
DataStream >> readInteger32Bits [
    "PRIVATE -- Read the contents of a SmallInteger that fits in 32 bits.
	See #writeInteger32Bits:
	See #readInteger64Bits and #writeInteger64Bits:"

    ^ byteStream nextInt32BigEndian: true	"signed!!!"
]

{ #category : 'reading' }
DataStream >> readInteger64Bits [
    "PRIVATE -- Read the contents of a SmallInteger.
	See #writeInteger64Bits:
	See #readInteger32Bits and #writeInteger32Bits:"

	| highBits lowBits |
	highBits := byteStream nextInt32BigEndian: true.	"signed!!!"
	lowBits := byteStream nextUint32BigEndian: true.	"unsigned!!!"
	^(highBits bitShift: 32) + lowBits.
]

{ #category : 'write and read' }
DataStream >> readMethod [
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	className := self next.
	newClass := Smalltalk at: className asSymbol.

	xxHeader := self next.
		"nArgs := (xxHeader >> 24) bitAnd: 16rF."
		"nTemps := (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit := (xxHeader >> 17) bitAnd: 1."
	nLits := (xxHeader >> 9) bitAnd: 16rFF.
		"primBits := ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer := instSize - (newClass instSize "0") - (nLits + 1 * Smalltalk wordSize).

	newMethod := newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits := newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*Smalltalk wordSize+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod
]

{ #category : 'reading' }
DataStream >> readNil [
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil
]

{ #category : 'reading' }
DataStream >> readReference [
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |

	^ (referencePosition := (byteStream nextUint32BigEndian: true)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"
]

{ #category : 'reading' }
DataStream >> readShortInst [
	"Read the contents of an arbitrary instance that has a short header.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize := (byteStream next) - 1.	"one byte of size"
	refPosn := self getCurrentReference.
	aSymbol := self readShortRef.	"class symbol in two bytes of file pos"
	newClass := Smalltalk at: aSymbol asSymbol.
	anObject := newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
]

{ #category : 'reading' }
DataStream >> readShortRef [
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextUint16BigEndian: true)
]

{ #category : 'reading' }
DataStream >> readString [

	^byteStream nextString
]

{ #category : 'reading' }
DataStream >> readSymbol [
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readString asSymbol
]

{ #category : 'reading' }
DataStream >> readTrue [
    "PRIVATE -- Read the contents of a True."

    ^ true
]

{ #category : 'write and read' }
DataStream >> readUnicodeString [

	^byteStream nextUnicodeString
]

{ #category : 'write and read' }
DataStream >> readUnicodeSymbol [
	"PRIVATE -- Read the contents of a Symbol.
	Note: Even if what was saved was an instance of UnicodeSymbol, if it
	only contains ASCII, then we'll answer an instance of Symbol.
	See #asSymbol and #basicInternNew:withHash:"

    ^ self readUnicodeString asSymbol
]

{ #category : 'write and read' }
DataStream >> readWordArray [
	"PRIVATE -- Read the contents of a WordArray."

	^ WordArray newFromStream: byteStream
	"Size is number of long words."
]

{ #category : 'write and read' }
DataStream >> readWordLike [
	| refPosn aSymbol newClass anObject |
	"Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc)."

	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk at: aSymbol asSymbol.
	anObject := newClass newFromStream: byteStream.
	"Size is number of long words."
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject

]

{ #category : 'write and read' }
DataStream >> replace: original with: proxy [
	"We may wish to remember that in some field, the original object is being replaced by the proxy.  For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced."

	"do nothing"
]

{ #category : 'other' }
DataStream >> reset [
    "Reset the stream."

    byteStream reset
]

{ #category : 'other' }
DataStream >> rootObject [
	"Return the object at the root of the tree we are filing out.  "

	^ topCall
]

{ #category : 'other' }
DataStream >> rootObject: anObject [
	"Return the object at the root of the tree we are filing out.  "

	topCall := anObject
]

{ #category : 'reading' }
DataStream >> setCurrentReference: refPosn [
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."
]

{ #category : 'initialization' }
DataStream >> setStream: aStream [
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.
]

{ #category : 'other' }
DataStream >> setStream: aStream reading: isReading [
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.
]

{ #category : 'other' }
DataStream >> size [
    "Answer the stream's size."

    ^ byteStream size
]

{ #category : 'write and read' }
DataStream >> tryToPutReference: anObject typeID: typeID [
    "PRIVATE -- If we support references for type typeID, and if
       anObject already appears in my output stream, then put a
       reference to the place where anObject already appears. If we
       support references for typeID but didnÕt already put anObject,
       then associate the current stream position with anObject in
       case one wants to nextPut: it again.
     Return true after putting a reference; false if the object still
       needs to be put.
     For DataStream this is trivial. ReferenceStream overrides this."

    ^ false
]

{ #category : 'aux' }
DataStream >> typeIDFor: anObject [
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	"SmallInteger is a bit special"
	| typeMapKey |
	typeMapKey := anObject class name.
	typeMapKey == #SmallInteger ifTrue: [
		typeMapKey := (anObject
				between: `(1 bitShift: 30) negated bitShift: 1` 	"Avoid LargeInteger in 32 bits images"
				and: `(1 bitShift: 30) -1 * 2 + 1`) 			"Avoid LargeInteger in 32 bits images"
			ifTrue: [ #SmallInteger32 ]
			ifFalse: [#SmallInteger64 ]].
	^ DataStream typeMap at: typeMapKey ifAbsent: [9 "instance of any normal class"]
]

{ #category : 'aux' }
DataStream >> vacantRef [
	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference
	 position' to identify a reference that's not yet filled in. This must be a
	 value that won't be used as an ordinary reference. Cf. outputReference: and
	 readReference. -- 
	 NOTE: We could use a different type ID for vacant-refs rather than writing
		object-references with a magic value. (The type ID and value are
		overwritten by ordinary object-references when weak refs are fullfilled.)"

	"In 32 bit Cuis it was:"
	"^ SmallInteger maxVal"

	"Use that very same value even if in 64 bit Cuis.
	This means that DataStreams are limited to 1GibiBytes in size."
	^16r3FFFFFFF
]

{ #category : 'write and read' }
DataStream >> writeArray: anArray [
	"PRIVATE -- Write the contents of an Array."

	byteStream nextUint32Put: anArray size bigEndian: true.
	self nextPutAll: anArray.
]

{ #category : 'write and read' }
DataStream >> writeBitmap: aBitmap [
	"PRIVATE -- Write the contents of a Bitmap."

	aBitmap writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words."
]

{ #category : 'write and read' }
DataStream >> writeByteArray: aByteArray [
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextUint32Put: aByteArray size bigEndian: true.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.
]

{ #category : 'write and read' }
DataStream >> writeCharacter: aCharacter [
	"PRIVATE -- Write a Character."

	byteStream nextUint32Put: aCharacter codePoint bigEndian: true
]

{ #category : 'write and read' }
DataStream >> writeClass: aClass [
	"Write out a DiskProxy for the class.  It will look up the class's name in Smalltalk in the new sustem.  Never write classes or methodDictionaries as objects.  For novel classes, front part of file is a fileIn of the new class."

	"This method never executed because objectToStoreOnDataStream returns a DiskProxy.  See DataStream.nextPut:"
    ^ self error: 'Write a DiskProxy instead'
]

{ #category : 'write and read' }
DataStream >> writeFalse: aFalse [
    "PRIVATE -- Write the contents of a False."
]

{ #category : 'write and read' }
DataStream >> writeFloat: aFloat [
	"PRIVATE -- Write the contents of a Float.
	Supports both BoxedFloat64 and SmallFloat64.
	Write in bigEndian / PowerPC order."

	byteStream nextFloat64Put: aFloat bigEndian: true
]

{ #category : 'write and read' }
DataStream >> writeInstance: anObject [
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self
]

{ #category : 'write and read' }
DataStream >> writeInteger32Bits: anInteger [
	"PRIVATE -- Write the contents of a SmallInteger that fits in 32 bits.
	See #readInteger32Bits
	See #readInteger64Bits and #writeInteger64Bits:"

	byteStream nextInt32Put: anInteger bigEndian: true	"signed!!!!!"
]

{ #category : 'write and read' }
DataStream >> writeInteger64Bits: anInteger [
	"PRIVATE -- Write the contents of a SmallInteger.
	See #readInteger64Bits
	See #readInteger32Bits and #writeInteger32Bits:"

	byteStream nextInt32Put: anInteger >> 32 bigEndian: true.						"signed"
	byteStream nextUint32Put: (anInteger bitAnd: 16rFFFFFFFF) bigEndian: true.		"unsigned"
]

{ #category : 'write and read' }
DataStream >> writeNil: anUndefinedObject [
    "PRIVATE -- Write the contents of an UndefinedObject."
]

{ #category : 'write and read' }
DataStream >> writeString: aString [
	"PRIVATE -- Write the contents of a String."

	byteStream nextStringPut: aString.
]

{ #category : 'write and read' }
DataStream >> writeSymbol: aSymbol [
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol
]

{ #category : 'write and read' }
DataStream >> writeTrue: aTrue [
    "PRIVATE -- Write the contents of a True."
]

{ #category : 'write and read' }
DataStream >> writeUnicodeString: aString [
	"PRIVATE -- Write the contents of a String."

	byteStream nextUnicodeStringPut: aString.
]

{ #category : 'write and read' }
DataStream >> writeUnicodeSymbol: aSymbol [
    "PRIVATE -- Write the contents of a Symbol."

    self writeUnicodeString: aSymbol
]

{ #category : 'write and read' }
DataStream >> writeWordLike: aWordArray [
	"Note that we put the class name before the size."

	self nextPut: aWordArray class name.
	aWordArray writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words or double-bytes."
]
