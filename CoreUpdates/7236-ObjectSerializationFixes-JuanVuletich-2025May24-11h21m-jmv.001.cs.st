'From Cuis7.3 [latest update: #7235] on 24 May 2025 at 11:32:26 am'!

!ThirtyTwoBitSlotsObject methodsFor: 'object serialization' stamp: 'jmv 5/24/2025 11:30:48'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]! !

!ThirtyTwoBitSlotsObject methodsFor: 'object serialization' stamp: 'jmv 5/24/2025 11:30:25'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	aStream nextInt32Put: self basicSize bigEndian: true.
	aStream nextUint32WordsPutAll: self.! !


!ThirtyTwoBitSlotsObject class methodsFor: 'object serialization' stamp: 'jmv 5/24/2025 11:31:30'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)! !


!DataStream class methodsFor: '[mkObjectSerialization] cached state access' stamp: 'jmv 5/24/2025 11:23:21'!
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
					t at: cls name print put: 20 ]]]].
	t at: #CompiledMethod put: 21.	"special creation method"
	t at: #Character put: 22.
	t at: #UnicodeString put: 23.
	t at: #UnicodeSymbol put: 24.! !

!methodRemoval: Color class #newFromStream: stamp: 'jmv 5/24/2025 11:31:30'!
Color class removeSelector: #newFromStream:!
!methodRemoval: Color #writeOn: stamp: 'jmv 5/24/2025 11:30:25'!
Color removeSelector: #writeOn:!
!methodRemoval: Color #restoreEndianness stamp: 'jmv 5/24/2025 11:30:48'!
Color removeSelector: #restoreEndianness!

!ThirtyTwoBitSlotsObject class reorganize!
('instance creation' new numSlots)
('object serialization' newFromStream:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	DataStream initCachedState!

