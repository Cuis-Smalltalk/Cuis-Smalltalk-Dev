'From Cuis7.5 [latest update: #7268] on 17 June 2025 at 5:19:13 pm'!

!DataStream class methodsFor: '[mkObjectSerialization] cached state access' stamp: 'jmv 6/17/2025 16:24:25'!
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

