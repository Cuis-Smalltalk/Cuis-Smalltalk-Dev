'From Cuis7.3 [latest update: #7132] on 9 May 2025 at 2:03:35 pm'!
!classDefinition: #DataStream category: #'System-Object Storage'!
Stream subclass: #DataStream
	instanceVariableNames: 'byteStream topCall basePos '
	classVariableNames: 'ReferenceTypes TypeMap '
	poolDictionaries: ''
	category: 'System-Object Storage'!

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/9/2025 11:59:30'!
nextFloat64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ])
			readStream nextFloat64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ])
			readStream nextFloat64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ (bytes float64At: 1 bigEndian: bigEndian)
		+ 0.0 "Convert to SmallFloat64 if appropriate"! !


!DataStream methodsFor: '[mkObjectStorage] reading' stamp: 'jmv 5/9/2025 14:00:40'!
next
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type := byteStream next.
	type ifNil: [
		pos := byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		pos.	"so can see it in debugger"
		^ nil].
	type = 0 ifTrue: [
		pos := byteStream position.	"absolute!!!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType := self noteCurrentReference: type.
	selector := #(
			readNil readTrue readFalse readInteger
			errorRead readSymbol readByteArray readArray
			readInstance readReference readBitmap readClass
			errorRead readFloat errorRead readShortInst
			readString readWordArray errorRead readWordLike
			readMethod 
			readCharacter readUnicodeString readUnicodeSymbol) at: type.
	selector = 0 ifTrue: [
		pos := byteStream position.	"absolute!!!!"
		byteStream close. 
		self error: 'file is more recent than this system'.
		^ nil].
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
	^ self maybeBeginReference: internalObject.! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/9/2025 14:00:54'!
nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore := (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := #(
		writeNil: writeTrue: writeFalse: writeInteger: 
		errorWriteReference: writeSymbol: writeByteArray: writeArray:
		writeInstance: errorWriteReference: writeBitmap: writeClass:
		errorWriteReference: writeFloat: errorWriteReference: errorWriteReference:
		writeString: writeBitmap: errorWriteReference: writeWordLike: 
		writeInstance: "CompiledMethod"
		writeCharacter: writeUnicodeString: writeUnicodeSymbol:) at: typeID.
	self perform: selector with: objectToStore.

	^ anObject


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of these, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/9/2025 12:00:55'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order.
	Answer is BoxedFloat64 or SmallFloat64 as appropriate."

	^ byteStream nextFloat64BigEndian: true! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/9/2025 12:00:33'!
writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	Supports both BoxedFloat64 and SmallFloat64.
	Write in bigEndian / PowerPC order."

	byteStream nextFloat64Put: aFloat bigEndian: true! !


!DataStream class methodsFor: '[mkObjectStorage] cached state access' stamp: 'jmv 5/9/2025 14:01:37'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  #nextPut: writes these IDs to the data stream.
	NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"
	| t |
	t := TypeMap := Dictionary newWithRoomForMoreThan: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.
	t at: True put: 2.
	t at: False put: 3.
	t at: SmallInteger put: 4.
	"(type ID 5 was for Squeak's old String format)"
	t at: Symbol put: 6.
	t at: ByteArray put: 7.
	t at: Array put: 8.
	"(type ID 9 is for arbitrary instances of any class. See #typeIDFor: See #writeInstance: See #readInstance)"
	"(type ID 10 is for references, See ReferenceStream>>tryToPutReference:typeID:)"
	t at: Bitmap put: 11.
	t at: Metaclass put: 12.
	"Type ID 13 was for Squeak's  HyperSqueak User classes."
	t at: Float put: 14.
	t at: BoxedFloat64 put: 14.
	t at: SmallFloat64 put: 14.
	"(type ID 15 was for Squeak's old Deprecated compact Rects."
	"type ID 16 is an instance with short header.  See #beginInstance:size:"
	t at: String put: 17.	"'new' String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.
	"type ID 19 was for Squeak's Word Array for Segment."
	"For all classes that are like WordArrays, store them as bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isBehavior ifTrue: [
			cls isPointers not & cls isVariable & cls isWords ifTrue: [
				(t includesKey: cls) ifFalse: [t at: cls put: 20]]]].
	t at: CompiledMethod put: 21.	"special creation method"
	t at: Character put: 22.
	t at: UnicodeString put: 23.
	t at: UnicodeSymbol put: 24.! !

!DataStream class methodsFor: '[mkObjectStorage] cached state access' stamp: 'jmv 5/9/2025 13:02:38'!
releaseClassCachedState

	TypeMap := nil! !


!ReferenceStream methodsFor: '[mkObjectStorage] writing' stamp: 'jmv 5/9/2025 11:54:59'!
isAReferenceType: typeID
	"Answer is:
		- true means a regular object reference (and the referenced object is in elsewhere in the stream)
		- false means either
				- an immediate (SmallInteger, Character, but not SmallFloat64) 
				- or a special object (nil, true, false) 
				- or a Class (actually an error condition. See #next and #nextPut:)
	"
	^ (#(0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 1) at: typeID) = 1! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 5/9/2025 11:33:21'!
moreObjects
	"Return true if there appears to be another object following this one on the file."

	| byte |
	byteStream atEnd ifTrue: [^ false].	"off end of file"
	(byte := byteStream peek) ifNil: [^ false].	"off end of file"
	byte = 33 "$!! asciiValue" ifTrue: [^ false].
	byte = 0 ifTrue: [^ false].
	^ true! !

!methodRemoval: DataStream class #referenceTypes stamp: 'jmv 5/9/2025 14:02:17'!
DataStream class removeSelector: #referenceTypes!
!methodRemoval: DataStream #readUser stamp: 'jmv 5/9/2025 14:02:43'!
DataStream removeSelector: #readUser!
!methodRemoval: DataStream #readWordArrayForSegment stamp: 'jmv 5/9/2025 14:02:43'!
DataStream removeSelector: #readWordArrayForSegment!
!methodRemoval: DataStream #writeUser: stamp: 'jmv 5/9/2025 14:02:43'!
DataStream removeSelector: #writeUser:!
!methodRemoval: DataStream #writeStringOld: stamp: 'jmv 5/9/2025 14:02:43'!
DataStream removeSelector: #writeStringOld:!
!classDefinition: #DataStream category: #'System-Object Storage'!
Stream subclass: #DataStream
	instanceVariableNames: 'byteStream topCall basePos'
	classVariableNames: 'TypeMap'
	poolDictionaries: ''
	category: 'System-Object Storage'!
