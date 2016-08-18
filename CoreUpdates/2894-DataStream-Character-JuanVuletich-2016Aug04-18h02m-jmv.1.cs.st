'From Cuis 4.2 of 25 July 2013 [latest update: #2902] on 4 August 2016 at 6:15:10 pm'!

!DataStream methodsFor: 'write and read' stamp: 'jmv 8/4/2016 18:12:43'!
readCharacter
    "PRIVATE -- Read a Character."

    ^ Character numericValue: (byteStream nextUnsignedInt32BigEndian: true)! !

!DataStream methodsFor: 'write and read' stamp: 'jmv 8/4/2016 18:13:53'!
writeCharacter: aCharacter
	"PRIVATE -- Write a Character."

	byteStream nextUnsignedInt32Put: aCharacter numericValue bigEndian: true! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 8/4/2016 18:14:19'!
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
			readWordLike readMethod 
			readCharacter "<-22") at: type.
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

!DataStream methodsFor: 'write and read' stamp: 'jmv 8/4/2016 18:14:02'!
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
		writeInstance: "CompiledMethod"
		writeCharacter: ) at: typeID.
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


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 8/4/2016 18:10:37'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"

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
	t at: BoxedFloat64 put: 14.
	t at: SmallFloat64 put: 14.

	"15: Deprecated compact Rects."
		refTypes add: 1.

	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.

	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19."  refTypes add: 1.		"bitmap-like"
	t at: FloatArray put: 20.  refTypes add: 1.	"And all other word arrays, both 16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"

	t at: Character put: 22. refTypes add: 0.

	"t at:  put: 23.  refTypes add: 0."
	ReferenceTypes _ refTypes.		"save it"! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	DataStream initCachedState!

