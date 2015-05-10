'From Cuis 4.2 of 25 July 2013 [latest update: #2280] on 1 May 2015 at 9:10:34.474014 pm'!
!classDefinition: #DataStream category: #'System-Object Storage'!
Stream subclass: #DataStream
	instanceVariableNames: 'byteStream topCall basePos '
	classVariableNames: 'TypeMap ReferenceTypes '
	poolDictionaries: ''
	category: 'System-Object Storage'!
!classDefinition: #ReferenceStream category: #'System-Object Storage'!
DataStream subclass: #ReferenceStream
	instanceVariableNames: 'references objects currentReference fwdRefEnds blockers skipping '
	classVariableNames: 'RefTypes '
	poolDictionaries: ''
	category: 'System-Object Storage'!
!classDefinition: #SmartRefStream category: #'System-Object Storage'!
ReferenceStream subclass: #SmartRefStream
	instanceVariableNames: 'structures steady renamed renamedConv superclasses progressBar objCount '
	classVariableNames: 'ScannedObject '
	poolDictionaries: ''
	category: 'System-Object Storage'!

!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 5/1/2015 21:08'!
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
	t at: Rectangle put: 15.  refTypes add: 1.	"Allow compact Rects."
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

!DataStream class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 21:09'!
referenceTypes

	ReferenceTypes ifNil: [ self initCachedState ].
	^ReferenceTypes! !

!DataStream class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 21:09'!
releaseClassCachedState

	TypeMap _ ReferenceTypes _ nil! !

!DataStream class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 21:09'!
typeMap

	TypeMap ifNil: [ self initCachedState ].
	^TypeMap! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 5/1/2015 21:07'!
typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ DataStream typeMap at: anObject class ifAbsent: [9 "instance of any normal class"]	
"See DataStream initialize.  nil=1. true=2. false=3. a SmallInteger=4. (a String was 5). a Symbol=6.  a ByteArray=7. an Array=8. other = 9.  a Bitmap=11. a Metaclass=12. a Float=14.  a Rectangle=15. any instance that can have a short header=16.  a String=17 (new format). a WordArray=18."! !


!ReferenceStream methodsFor: 'writing' stamp: 'jmv 5/1/2015 21:06'!
isAReferenceType: typeID
	"Return true iff typeID is one of the classes that can be written as a reference to an instance elsewhere in the stream."

	"too bad we can't put Booleans in an Array literal"
	^ (DataStream referenceTypes at: typeID) = 1
		"NOTE: If you get a bounds error here, the file probably has bad bits in it.  The most common cause is a file unpacking program that puts linefeeds after carriage returns."! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 5/1/2015 21:07'!
moreObjects
	"Return true if there appears to be another object following this one on the file."

	| byte |
	byteStream atEnd ifTrue: [^ false].	"off end of file"
	(byte _ byteStream peek) ifNil: [^ false].	"off end of file"
	byte = 33 "$!! asciiValue" ifTrue: [^ false].
	byte = 0 ifTrue: [^ false].
	^ byte <= DataStream referenceTypes size		"between 1 and 16"! !

!methodRemoval: SmartRefStream class #scanFrom:!
SmartRefStream class removeSelector: #scanFrom:!
!methodRemoval: SmartRefStream class #scannedObject!
SmartRefStream class removeSelector: #scannedObject!
!methodRemoval: SmartRefStream class #scannedObject:!
SmartRefStream class removeSelector: #scannedObject:!
!methodRemoval: SmartRefStream #scanFrom:!
SmartRefStream removeSelector: #scanFrom:!
!classDefinition: #SmartRefStream category: #'System-Object Storage'!
ReferenceStream subclass: #SmartRefStream
	instanceVariableNames: 'structures steady renamed renamedConv superclasses progressBar objCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Object Storage'!
!methodRemoval: ReferenceStream class #refTypes:!
ReferenceStream class removeSelector: #refTypes:!
!classDefinition: #ReferenceStream category: #'System-Object Storage'!
DataStream subclass: #ReferenceStream
	instanceVariableNames: 'references objects currentReference fwdRefEnds blockers skipping'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Object Storage'!
!methodRemoval: DataStream class #cleanup!
DataStream class removeSelector: #cleanup!
!methodRemoval: DataStream class #initialize!
DataStream class removeSelector: #initialize!
DataStream initialize!

!DataStream class reorganize!
('as yet unclassified' example exampleWithPictures fileNamed: forceNewFileNamed: initCachedState new newFileNamed: oldFileNamed: on: streamedRepresentationOf: testWith: unStream:)
('cached state access' referenceTypes releaseClassCachedState typeMap)
!

!classDefinition: #DataStream category: #'System-Object Storage'!
Stream subclass: #DataStream
	instanceVariableNames: 'byteStream topCall basePos'
	classVariableNames: 'ReferenceTypes TypeMap'
	poolDictionaries: ''
	category: 'System-Object Storage'!
