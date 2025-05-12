'From Cuis7.3 [latest update: #7132] on 9 May 2025 at 2:08:40 pm'!

!DataStream methodsFor: '[mkObjectStorage] reading' stamp: 'jmv 5/9/2025 14:07:49'!
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

!DataStream methodsFor: 'write and read' stamp: 'jmv 5/9/2025 14:07:33'!
typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ DataStream typeMap at: anObject class name ifAbsent: [9 "instance of any normal class"]! !


!DataStream class methodsFor: '[mkObjectStorage] cached state access' stamp: 'jmv 5/9/2025 14:07:05'!
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
	t at: #SmallInteger put: 4.
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
				(t includesKey: cls) ifFalse: [
					t at: cls name put: 20 ]]]].
	t at: #CompiledMethod put: 21.	"special creation method"
	t at: #Character put: 22.
	t at: #UnicodeString put: 23.
	t at: #UnicodeSymbol put: 24.! !

