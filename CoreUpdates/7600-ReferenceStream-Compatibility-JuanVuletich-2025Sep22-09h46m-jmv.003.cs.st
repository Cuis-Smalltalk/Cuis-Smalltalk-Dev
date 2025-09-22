'From Cuis7.5 [latest update: #7597] on 22 September 2025 at 10:21:49 am'!

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/22/2025 09:59:00'!
writeInteger32Bits: anInteger
	"PRIVATE -- Write the contents of a SmallInteger that fits in 32 bits.
	See #readInteger32Bits
	See #readInteger64Bits and #writeInteger64Bits:"

	byteStream nextInt32Put: anInteger bigEndian: true	"signed!!!!!!!!!!"! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/22/2025 10:00:17'!
writeInteger64Bits: anInteger
	"PRIVATE -- Write the contents of a SmallInteger.
	See #readInteger64Bits
	See #readInteger32Bits and #writeInteger32Bits:"

	byteStream nextInt32Put: anInteger >> 32 bigEndian: true.						"signed"
	byteStream nextUint32Put: (anInteger bitAnd: 16rFFFFFFFF) bigEndian: true.		"unsigned"! !

!DataStream methodsFor: 'reading' stamp: 'jmv 9/22/2025 09:57:17'!
readInteger32Bits
    "PRIVATE -- Read the contents of a SmallInteger that fits in 32 bits.
	See #writeInteger32Bits:
	See #readInteger64Bits and #writeInteger64Bits:"

    ^ byteStream nextInt32BigEndian: true	"signed!!!!!!"! !

!DataStream methodsFor: 'reading' stamp: 'jmv 9/22/2025 09:57:44'!
readInteger64Bits
    "PRIVATE -- Read the contents of a SmallInteger.
	See #writeInteger64Bits:
	See #readInteger32Bits and #writeInteger32Bits:"

	| highBits lowBits |
	highBits := byteStream nextInt32BigEndian: true.	"signed!!!!!!"
	lowBits := byteStream nextUint32BigEndian: true.	"unsigned!!!!!!"
	^(highBits bitShift: 32) + lowBits.! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 9/22/2025 10:01:02'!
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
	 writeReference: -> errorWriteReference:."! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 10:01:37'!
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
	^ self maybeBeginReference: internalObject.! !

!DataStream methodsFor: 'aux' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 10:15:07'!
typeIDFor: anObject
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
	^ DataStream typeMap at: typeMapKey ifAbsent: [9 "instance of any normal class"]! !


!DataStream class methodsFor: 'cached state access' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 10:11:46'!
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
	t at: #SmallInteger64 put: 25. "Needed to handle the full 61 bit range of SmallIntegers in 64 bit Cuis images."! !


!ReferenceStream methodsFor: 'aux' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/22/2025 10:19:51'!
isAReferenceType: typeID
	"Answer is:
		- true means a regular object reference (and the referenced object is in elsewhere in the stream)
		- false means either
				- an immediate (SmallInteger, Character, but not SmallFloat64) 
				- or a special object (nil, true, false) 
				- or a Class (actually an error condition. See #next and #nextPut:)
	"
	^ (#(0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0) at: typeID) = 1! !

!methodRemoval: DataStream #writeInteger: stamp: 'jmv 9/22/2025 10:01:09'!
DataStream removeSelector: #writeInteger:!
!methodRemoval: DataStream #readInteger stamp: 'jmv 9/22/2025 10:01:49'!
DataStream removeSelector: #readInteger!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	DataStream initCachedState!

