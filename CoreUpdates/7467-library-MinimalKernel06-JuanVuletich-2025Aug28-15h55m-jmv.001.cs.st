'From Cuis7.5 [latest update: #7462] on 28 August 2025 at 3:56:22 pm'!

!DataStream methodsFor: 'initialization' library: #MinimalKernel06ObjectSerialization stamp: 'di 6/9/97 08:03'!
setStream: aStream
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
beginReference: anObject
    "WeÕre starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
getCurrentReference
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 4/8/1999 13:11'!
maybeBeginReference: internalObject
	"Do nothing.  See ReferenceStream|maybeBeginReference:"

	^ internalObject! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/9/2025 14:07:49'!
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

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
noteCurrentReference: typeID
    "PRIVATE -- If we support references for type typeID, remember
     the current byteStream position so we can add the next object to
     the ÔobjectsÕ dictionary, and return true. Else return false.
     This method is here to be overridden by ReferenceStream"

    ^ false! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 6/30/2011 09:33'!
objectAt: anInteger
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn := byteStream position.		"absolute"
	refPosn := self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject := self next.

	self setCurrentReference: refPosn.		"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/5/2023 20:16:17'!
readArray
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
	^ array! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/5/2023 20:16:28'!
readByteArray
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count := byteStream nextUint32BigEndian: true.
	^ byteStream next: count  "assume stream is in binary mode"
! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 1/8/2024 12:18:49'!
readCharacter
    "PRIVATE -- Read a Character."

    ^ Character codePoint: (byteStream nextUint32BigEndian: true)! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readFalse
    "PRIVATE -- Read the contents of a False."

    ^ false! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/9/2025 12:00:55'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order.
	Answer is BoxedFloat64 or SmallFloat64 as appropriate."

	^ byteStream nextFloat64BigEndian: true! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/14/2023 10:24:50'!
readInstance
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
	^ anObject! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/5/2023 20:18:41'!
readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextInt32BigEndian: true	"signed!!!!!!"! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readNil
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/5/2023 20:16:43'!
readReference
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |

	^ (referencePosition := (byteStream nextUint32BigEndian: true)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 1/8/97'!
readShortInst
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
	^ anObject! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/5/2023 20:17:23'!
readShortRef
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextUint16BigEndian: true)! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/1/2023 17:03:35'!
readString

	^byteStream nextString! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readString asSymbol! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
readTrue
    "PRIVATE -- Read the contents of a True."

    ^ true! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."! !

!DataStream methodsFor: 'aux' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/9/2025 14:07:33'!
typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ DataStream typeMap at: anObject class name ifAbsent: [9 "instance of any normal class"]! !

!DataStream methodsFor: 'aux' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 12/29/2016 15:36:22'!
vacantRef
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
	^16r3FFFFFFF! !


!DataStream class methodsFor: 'cached state access' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 6/17/2025 16:24:25'!
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
				(t includesKey: cls name) ifFalse: [
					t at: cls name put: 20 ]]]].
	t at: #CompiledMethod put: 21.	"special creation method"
	t at: #Character put: 22.
	t at: #UnicodeString put: 23.
	t at: #UnicodeSymbol put: 24.! !

!DataStream class methodsFor: 'cached state access' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/1/2015 21:09'!
typeMap

	TypeMap ifNil: [ self initCachedState ].
	^TypeMap! !

!DataStream class methodsFor: 'instance creation' library: #MinimalKernel06ObjectSerialization stamp: 'di 2/15/98 14:03'!
new
	^ self basicNew! !

!DataStream class methodsFor: 'instance creation' library: #MinimalKernel06ObjectSerialization stamp: 'di 6/24/97 00:18'!
on: aStream
	"Open a new DataStream onto a low-level I/O stream."

	^ self basicNew setStream: aStream
		"aStream binary is in setStream:"
! !


!ReferenceStream methodsFor: 'initialization' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 2/6/2024 09:32:11'!
setStream: aStream
	"PRIVATE -- Initialization method."

	super setStream: aStream.
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
	skipping := IdentitySet new.
	blockers ifNil: [blockers := IdentityDictionary new].	"keep blockers we just passed in"
! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'RAA 1/18/2001 11:52'!
beginReference: anObject
	"Remember anObject as the object we read at the position recorded by
	 noteCurrentReference:. This must be done after instantiating anObject but
	 before reading any of its contents that might (directly or indirectly) refer to
	 it. (It's ok to do this redundantly, which is convenient for #next.)
	 Answer the reference position."

	objects at: currentReference ifAbsent: [
		objects at: currentReference put: anObject.
		^ currentReference].
	(skipping includes: currentReference) ifFalse: [
		"If reading just to skip it, don't record this copy."
		objects at: currentReference put: anObject
	].
	^ currentReference		"position relative to start of data portion of file"! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 6/9/97 08:26'!
getCurrentReference
    "PRIVATE -- Return the currentReference posn.  Always a relative position.  So user can cut and paste the Smalltalk source code at the beginning of the file."

    ^ currentReference	"relative position"! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 4/8/1999 13:11'!
maybeBeginReference: internalObject
	"See if need to record a reference.  In case in the file twice"

	(self isAReferenceType: (self typeIDFor: internalObject))
			ifTrue: [self beginReference: internalObject].
			"save the final object and give it out next time."
	^ internalObject! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 3/2/2010 10:05'!
next
	"Answer the next object in the stream.  If this object was already read, don't re-read it.  File is positioned just before the object."
	| curPosn skipToPosn haveIt theObject wasSkipping |

	haveIt := true.
	curPosn := byteStream position - basePos.
	theObject := objects at: curPosn ifAbsent: [haveIt := false].
		"probe in objects is done twice when coming from objectAt:.  This is OK."
	skipToPosn := fwdRefEnds at: curPosn ifAbsent: nil.
	haveIt ifFalse: [ ^ super next].

	skipToPosn ifNotNil: [
		"Skip over the object and return the already-read-in value."
		byteStream position: skipToPosn + basePos		"make absolute"
	] ifNil: [
		"File is not positioned correctly.  Read object and throw it away."
		wasSkipping := skipping includes: curPosn.
		skipping add: curPosn.
		"fake _" super next.
		wasSkipping ifFalse: [skipping remove: curPosn ifAbsent: nil].
	].
	^ theObject
		! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 6/9/97 09:00'!
noteCurrentReference: typeID
	"PRIVATE -- If we support references for type typeID, remember
	 the current byteStream position so beginReference: can add the
	 next object to the 'objects' dictionary of reference positions,
	 then return true. Else return false."
	| answer |

	(answer := self isAReferenceType: typeID)
		ifTrue: [self setCurrentReference: (byteStream position - 1) - basePos "relative"
				"subtract 1 because we already read the object's type ID byte"].
	^ answer! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: ' 6/9/97'!
objectAt: anInteger
    "PRIVATE -- Read & return the object at a given stream position.
     If we already read it, just get it from the objects dictionary.
     (Reading it again wouldn't work with cycles or sharing.)
     If not, go read it and put it in the objects dictionary.
     NOTE: This resolves a cross-reference in the ReferenceStream:
       1. A backward reference to an object already read (the normal case).
       2. A forward reference which is a sated weak reference (we record where
          the object ends so when we get to it normally we can fetch it from
          'objects' and skip over it).
       3. A backward reference to a 'non-reference type' per the long NOTE in
          nextPut: (we compensate here--seek back to re-read it and add the object
          to 'objects' to avoid seeking back to read it any more times).
       4. While reading a foward weak reference (case 2), we may recursively hit an
          ordinary backward reference to an object that we haven't yet read because
          we temporarily skipped ahead. Such a reference is forward in time so we
          treat it much like case 2.
     11/16-24/92 jhm: Handle forward refs. Cf. class comment and above NOTE.
	08:57 tk   anInteger is a relative position"
    | savedPosn refPosn anObject |

    ^ objects at: anInteger "relative position.  case 1: It's in 'objects'"
        ifAbsent:   "do like super objectAt:, but remember the fwd-ref-end position"
            [savedPosn := byteStream position.		"absolute"
            refPosn := self getCurrentReference.	"relative position"

            byteStream position: anInteger + basePos.	"was relative"
            anObject := self next.

            (self isAReferenceType: (self typeIDFor: anObject))
                ifTrue:  [fwdRefEnds at: anInteger put: byteStream position - basePos] "cases 2, 4"
                ifFalse: [objects at: anInteger put: anObject]. "case 3"

            self setCurrentReference: refPosn.		"relative position"
            byteStream position: savedPosn.		"absolute"
            anObject]! !

!ReferenceStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'tk 6/9/97 08:24'!
setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.  Always a relative position."

    currentReference := refPosn		"relative position"! !

!ReferenceStream methodsFor: 'aux' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 5/9/2025 11:54:59'!
isAReferenceType: typeID
	"Answer is:
		- true means a regular object reference (and the referenced object is in elsewhere in the stream)
		- false means either
				- an immediate (SmallInteger, Character, but not SmallFloat64) 
				- or a special object (nil, true, false) 
				- or a Class (actually an error condition. See #next and #nextPut:)
	"
	^ (#(0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 1) at: typeID) = 1! !


!DataStream reorganize!
('write and read' beginInstance:size: nextPut: objectIfBlocked: outputReference: readBitmap readClass readMethod readUnicodeString readUnicodeSymbol readWordArray readWordLike replace:with: tryToPutReference:typeID: writeArray: writeBitmap: writeByteArray: writeCharacter: writeClass: writeFalse: writeFloat: writeInstance: writeInteger: writeNil: writeString: writeSymbol: writeTrue: writeUnicodeString: writeUnicodeSymbol: writeWordLike:)
('other' atEnd byteStream contents errorRead errorWriteReference: flush next: reset rootObject rootObject: setStream:reading: size)
('initialization' setStream:)
('reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: readArray readByteArray readCharacter readFalse readFloat readInstance readInteger readNil readReference readShortInst readShortRef readString readSymbol readTrue setCurrentReference:)
('aux' typeIDFor: vacantRef)
!


!DataStream class reorganize!
('services' streamedRepresentationOf: unStream:)
('cached state access' initCachedState releaseClassCachedState typeMap)
('examples' example exampleWithPictures testWith:)
('instance creation' new on:)
!


!ReferenceStream reorganize!
('writing' addSpecialReference: beginInstance:size: blockers blockers: nextPutWeak: objectIfBlocked: references replace:with: reset setStream:reading: tryToPutReference:typeID:)
('statistics' statisticsOfRefs)
('initialization' setStream:)
('reading' beginReference: getCurrentReference maybeBeginReference: next noteCurrentReference: objectAt: setCurrentReference:)
('aux' isAReferenceType:)
!

