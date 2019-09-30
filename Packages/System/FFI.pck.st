'From Cuis 5.0 [latest update: #3890] on 29 September 2019 at 1:34:51 pm'!
'Description '!
!provides: 'FFI' 1 33!
SystemOrganization addCategory: #'FFI-Pools'!
SystemOrganization addCategory: #'FFI-Kernel'!


!classDefinition: #FFIConstants category: #'FFI-Pools'!
SharedPool subclass: #FFIConstants
	instanceVariableNames: ''
	classVariableNames: 'FFIAtomicTypeMask FFIAtomicTypeShift FFICallFlagThreaded FFICallTypeApi FFICallTypeCDecl FFICallTypesMask FFIErrorAddressNotFound FFIErrorAttemptToPassVoid FFIErrorBadAddress FFIErrorBadArg FFIErrorBadArgs FFIErrorBadAtomicType FFIErrorBadExternalFunction FFIErrorBadExternalLibrary FFIErrorBadReturn FFIErrorCallFrameTooBig FFIErrorCallType FFIErrorCoercionFailed FFIErrorGenericError FFIErrorIntAsPointer FFIErrorInvalidPointer FFIErrorModuleNotFound FFIErrorNoModule FFIErrorNotFunction FFIErrorStructSize FFIErrorWrongType FFIFlagAtomic FFIFlagPointer FFIFlagStructure FFINoCalloutAvailable FFIStructSizeMask FFITypeBool FFITypeDoubleFloat FFITypeSignedByte FFITypeSignedChar FFITypeSignedInt FFITypeSignedLongLong FFITypeSignedShort FFITypeSingleFloat FFITypeUnsignedByte FFITypeUnsignedChar FFITypeUnsignedInt FFITypeUnsignedLongLong FFITypeUnsignedShort FFITypeVoid'
	poolDictionaries: ''
	category: 'FFI-Pools'!
!classDefinition: 'FFIConstants class' category: #'FFI-Pools'!
FFIConstants class
	instanceVariableNames: ''!

!classDefinition: #ExternalAddress category: #'FFI-Kernel'!
ByteArray variableByteSubclass: #ExternalAddress
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalAddress class' category: #'FFI-Kernel'!
ExternalAddress class
	instanceVariableNames: 'wordSize'!

!classDefinition: #ExternalForm category: #'FFI-Kernel'!
Form subclass: #ExternalForm
	instanceVariableNames: 'pointer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalForm class' category: #'FFI-Kernel'!
ExternalForm class
	instanceVariableNames: ''!

!classDefinition: #ExternalObject category: #'FFI-Kernel'!
Object subclass: #ExternalObject
	instanceVariableNames: 'handle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalObject class' category: #'FFI-Kernel'!
ExternalObject class
	instanceVariableNames: ''!

!classDefinition: #ExternalFunction category: #'FFI-Kernel'!
ExternalObject subclass: #ExternalFunction
	instanceVariableNames: 'flags argTypes'
	classVariableNames: 'FFIErrorMessages'
	poolDictionaries: 'FFIConstants'
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalFunction class' category: #'FFI-Kernel'!
ExternalFunction class
	instanceVariableNames: ''!

!classDefinition: #ExternalLibraryFunction category: #'FFI-Kernel'!
ExternalFunction subclass: #ExternalLibraryFunction
	instanceVariableNames: 'name module errorCodeName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalLibraryFunction class' category: #'FFI-Kernel'!
ExternalLibraryFunction class
	instanceVariableNames: ''!

!classDefinition: #ExternalLibrary category: #'FFI-Kernel'!
ExternalObject subclass: #ExternalLibrary
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalLibrary class' category: #'FFI-Kernel'!
ExternalLibrary class
	instanceVariableNames: ''!

!classDefinition: #ExternalStructure category: #'FFI-Kernel'!
ExternalObject subclass: #ExternalStructure
	instanceVariableNames: ''
	classVariableNames: 'PreviousPlatform'
	poolDictionaries: 'FFIConstants'
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalStructure class' category: #'FFI-Kernel'!
ExternalStructure class
	instanceVariableNames: 'compiledSpec'!

!classDefinition: #ExternalData category: #'FFI-Kernel'!
ExternalStructure subclass: #ExternalData
	instanceVariableNames: 'type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalData class' category: #'FFI-Kernel'!
ExternalData class
	instanceVariableNames: ''!

!classDefinition: #ExternalUnion category: #'FFI-Kernel'!
ExternalStructure subclass: #ExternalUnion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalUnion class' category: #'FFI-Kernel'!
ExternalUnion class
	instanceVariableNames: ''!

!classDefinition: #ExternalType category: #'FFI-Kernel'!
Object subclass: #ExternalType
	instanceVariableNames: 'compiledSpec referentClass referencedType pointerSize'
	classVariableNames: 'AtomicSelectors AtomicTypeNames AtomicTypes StructTypes'
	poolDictionaries: 'FFIConstants'
	category: 'FFI-Kernel'!
!classDefinition: 'ExternalType class' category: #'FFI-Kernel'!
ExternalType class
	instanceVariableNames: ''!


!ExternalAddress commentStamp: '<historical>' prior: 0!
An ExternalAddress is an opaque handle to objects outside Smalltalk memory (e.g., a pointer).!

!ExternalForm commentStamp: '<historical>' prior: 0!
An ExternalForm is a specialized Form whose pixel-data is stored in memory that the user provides a pointer to.  This can simply be memory on the C heap, or (the motivating use-case...) it can be a pointer that is temporarily "mapped" from GPU memory by an API such as OpenCL.

The user is responsible for both releasing the image-memory, as well as destroying the surface handle (perhaps the latter should be handled by automatic finalization).

Example usage:

| extent form ptr |
extent := 400@300.
form := ExternalForm extent: extent depth: 32.
ptr := ExternalAddress gcallocate: (extent x * extent y * 4).
form setManualSurfacePointer: ptr.
Display displayOn: form.
form displayAt: 100@500.
form destroySurface.
!

!ExternalObject commentStamp: '<historical>' prior: 0!
External objects represent entities that are not part of the Smalltalk universe. They are accessed using a unique handle which is interpreted depending on the actual entity that is represented. 

Instance variables:
	handle	<ByteArray | ExternalAddress>!

!ExternalFunction commentStamp: '<historical>' prior: 0!
This class represents an external function called from Smalltalk. Instances of ExternalFunction can be created if the address/parameters of the function are known by some other means than loading from a shared library or compiling the appropriate primitive specification.

Instance variables:
	flags	<Integer>	a set of flags encoding the calling convention
	args	<Array of: ExternalType>		the parameters of the function

Implementation notes:

The arguments consist of an array with the first element defining the return type, the remaining arguments defining the parameters of the call.
!

!ExternalLibraryFunction commentStamp: '<historical>' prior: 0!
An ExternalLibraryFunction specifies a fully qualified function from an external library.

Instance variables:
	name			<String | Integer>	name or ordinal of function
	module			<String | nil>		name of module (nil if bound in the VM).
	errorCodeName <String | nil>		name of temp receiving error code, if any!

!ExternalLibrary commentStamp: '<historical>' prior: 0!
An external library bundles calls to functions from the same library. It is provided mainly as convenience since every external function can be fully specified by the name and the module it resides in.

Every external function that is defined in an external library by default will use the library it is defined in. This can always be modified by providing the appropriate module in the specification. !

!ExternalStructure commentStamp: 'jmv 8/31/2018 12:00:04' prior: 0!
An ExternalStructure is for representing external data that is
- either a structure composed of different fields (a struct of C language)
- or an alias for another type (like a typedef of C language)

It reserves enough bytes of data for representing all the fields.

The data is stored into the handle instance variable which can be of two different types:
	- ExternalAddress
		If the handle is an external address then the object described does not reside in the Smalltalk object memory.
	- ByteArray
		If the handle is a byte array then the object described resides in Smalltalk memory.

A specific structure is defined by subclassing ExternalStructure and specifying its #fields via a class side method.
For example if we define a subclass:
	ExternalStructure subclass: #StructExample
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then declare the fields like this:
    StructExample class compile: 'fields  ^#( (name ''char*'') (color ''ulong'') )' classified: 'garbage'.

It means that this type is composed of two different fields:
- a string (accessed thru the field #name)
- and an unsigned 32bit integer (accessed thru the field #color).
It represents the following C type:
   struct StructExample {char *name; uint32_t color; };

The accessors for those fields can be generated automatically like this:
	StructExample defineFields.
As can be verified in a Browser:
	StructExample browse.
We see that name and color fields are stored sequentially in different zones of data.

The total size of the structure can be verified with:
	StructExample byteSize = (Smalltalk wordSize + 4).

An ExternalStructure can also be used for defining an alias.
The fields definition must contain only 2 elements: an eventual accessor (or nil) and the type.
For example, We can define a machine dependent 'unsigned long' like this:
	ExternalStructure subclass: #UnsignedLong
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then set the fields like this:
    UnsignedLong class compile: 'fields  ^(Smalltalk wordSize=4 or: [Smalltalk platformName=''Win64''])
		ifTrue: [#(nil ''ulong'')] ifFalse: [#(nil ''ulonglong'')]' classified: 'garbage'.
And verify the size on current platform:
	UnsignedLong byteSize.
	
Then, the class names 'UnsignedLong' and 'StructExamples' acts as a type specification.
They can be used for composing other types, and for defining prototype of external functions:

LibraryExample>>initMyStruct: aStructExample name: name color: anInteger
	<cdecl: void 'init_my_struct'( StructExample * char * UnsignedLong )>
	self externalCallFailed


!

!ExternalData commentStamp: '<historical>' prior: 0!
Instances of ExternalData explicitly describe objects with associated type. They can be used for describing atomic C types like arrays of atomic types (e.g., 'int[]') or pointer to atomic types (e.g., 'int *').

Instance variables:
	type	<Integer | Behavior>	The basic type of the receiver.

The encoding of type is equivalent to that of the basic type in class ExternalType. The interpretation of whether the receiver describes an array of data or a pointer to data depends on the contents of the instance variable 'handle'. If handle contains an ExternalAddress the receiver is treated as pointer to type. If the handle contains a ByteArray the receiver is interpreted as describing an array of type. Note that both interpretations are treated equivalent in external calls, e.g., if one describes an argument to an external call as taking 'int*' then, depending on the type of handle either the actual contents (if ExternalAddress) or a pointer to the contents (if ByteArray) is passed.

!

!ExternalUnion commentStamp: 'nice 4/13/2018 21:07' prior: 0!
An ExternalUnion is for representing external data that is a union of possible fields.
It corresponds to the C type union.
It reserves enough bytes of data for representing the largest field.

A specific union is defined by subclassing ExternalUnion and specifying its #fields via a class side.

For example if we define a subclass:
	ExternalUnion subclass: #UnionExample
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then set the fields like this:
    UnionExample class compile: 'fields  ^#( (name ''char*'') (color ''ulong'') )' classified: 'garbage'.

It means that this type will represents
- either a string (accessed thru the field #name)
- or an unsigned 32bit integer (accessed thru the field #color).

It represents the following C type:
   union UnionExample {char *name; uint32_t color; };

The accessors for those fields can be generated automatically like this:
	UnionExample defineFields.
As can be verified in a Browser:
	UnionExample browse.
We see that color and name fields both interpret the same zone of data (starting at 1st byte), but with a different interpretation.
The size of the union can be verified with:
	UnionExample byteSize = (Smalltalk wordSize max: 4).!

!ExternalType commentStamp: '<historical>' prior: 0!
An external type represents the type of external objects.

Instance variables:
	compiledSpec	<WordArray>		Compiled specification of the external type
	referentClass	<Behavior | nil>	Class type of argument required
	referencedType	<ExternalType>	Associated (non)pointer type with the receiver

Compiled Spec:
The compiled spec defines the type in terms which are understood by the VM. Each word is defined as:
	bits 0...15 	- byte size of the entity
	bit 16		- structure flag (FFIFlagStructure)
				  This flag is set if the following words define a structure
	bit 17		- pointer flag (FFIFlagPointer)
				  This flag is set if the entity represents a pointer to another object
	bit 18		- atomic flag (FFIFlagAtomic)
				  This flag is set if the entity represents an atomic type.
				  If the flag is set the atomic type bits are valid.
	bits 19...23	- unused
	bits 24...27	- atomic type (FFITypeVoid ... FFITypeDoubleFloat)
	bits 28...31	- unused

Note that all combinations of the flags FFIFlagPointer, FFIFlagAtomic, and FFIFlagStructure are invalid, EXCEPT from the following:

	FFIFlagPointer + FFIFlagAtomic:
		This defines a pointer to an atomic type (e.g., 'char*', 'int*').
		The actual atomic type is represented in the atomic type bits.

	FFIFlagPointer + FFIFlagStructure:
		This defines a structure which is a typedef of a pointer type as in
			typedef void* VoidPointer;
			typedef Pixmap* PixmapPtr;
		It requires a byte size of four (e.g. a 32bit pointer) to work correctly.

[Note: Other combinations may be allowed in the future]
!

!ExternalAddress class methodsFor: 'class initialization' stamp: 'eem 2/22/2016 15:30'!
startUp: resuming
	"If starting the image afresh all external addresses should be zero.
	 In addition, if the word size has changed then external addresses shoiuld be resized.
	 The two steps are combined for efficiency."
	resuming ifTrue:
		[| instances |
		 instances := self allInstances.
		 wordSize ~= Smalltalk wordSize
			ifTrue: "Implement nulling by becomming all existing instances to new (and hence null) pointers of the right size."
				[wordSize := Smalltalk wordSize.
				 instances elementsForwardIdentityTo: (instances collect: [:ea| self basicNew: wordSize])]
			ifFalse:
				[instances do: [:addr| addr beNull]]]! !

!ExternalAddress methodsFor: 'printing' stamp: 'jmv 1/9/2014 21:37'!
printOn: aStream
	"print this as a hex address ('@ 16rFFFFFFFF') to distinguish it from ByteArrays"

	aStream nextPutAll: '@ '; nextPutAll: self asInteger hex8! !

!ExternalObject class methodsFor: 'system startup' stamp: 'ar 11/28/1999 23:36'!
startUp: resuming
	"The system is coming up. If it is on a new platform, clear out the existing handles."
	ExternalAddress startUp: resuming. "Make sure handles are invalid"
	resuming ifTrue:[self installSubclasses].
! !

!ExternalFunction methodsFor: 'accessing' stamp: 'ar 11/17/1999 17:06'!
name
	^nil! !

!ExternalFunction methodsFor: 'printing' stamp: 'eem 10/28/2009 17:10'!
printOn: aStream
	aStream
		nextPut:$<;
		nextPutAll: self callingConventionString; nextPutAll:': '.
	{ 'threaded' } with: { FFICallFlagThreaded } do:
		[:modifier :flag|
		(flags anyMask: flag) ifTrue:
			[aStream nextPutAll: modifier; space]].
	aStream print: argTypes first; space.
	self name == nil
		ifTrue:[aStream nextPutAll:'(*) ']
		ifFalse:[aStream print: self name asString; space].
	aStream nextPut:$(.
	2 to: argTypes size do:[:i|
		aStream print: (argTypes at: i).
		i < argTypes size ifTrue:[aStream space]].
	aStream nextPut:$).
	self module == nil ifFalse:[
		aStream space; nextPutAll:'module: '; print: self module asString.
	].
	self errorCodeName == nil ifFalse:[
		aStream space; nextPutAll:'error: '; nextPutAll: self errorCodeName.
	].
	aStream nextPut:$>! !

!ExternalLibraryFunction methodsFor: 'accessing' stamp: 'ar 11/17/1999 17:06'!
name
	^name! !

!ExternalLibrary methodsFor: 'accessing' stamp: 'ar 11/17/1999 19:35'!
name
	^name! !

!ExternalStructure class methodsFor: 'class management' stamp: 'ar 11/22/1999 10:10'!
doneCompiling
	"I have been recompiled. Update any types that reference me."
	ExternalType noticeModificationOf: self.! !

!ExternalStructure class methodsFor: 'class management' stamp: 'jmv 3/13/2012 12:34'!
fileOutInitializerOn: aFileStream

	super fileOutInitializerOn: aFileStream.
	aFileStream newLine.
	aFileStream newLine.
	aFileStream nextChunkPut: self name , ' compileFields'.
	aFileStream newLine.! !

!ExternalStructure class methodsFor: 'class management' stamp: 'jmv 3/13/2012 12:34'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool

	super fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: aBool.
	(aBool and:[moveSource not]) ifTrue: [
		aFileStream newLine.
		aFileStream newLine.
		aFileStream nextChunkPut: self name , ' compileFields'.
		aFileStream newLine]! !

!ExternalStructure class methodsFor: 'class management' stamp: 'sma 6/16/2000 22:12'!
obsolete
	"The receiver is becoming obsolete. 
	NOTE: You if you remove the whole class category at once, you cannot
	assume that the ExternalType class is still present."

	Smalltalk at: #ExternalType ifPresent: [:class | class noticeRemovalOf: self].
	^ super obsolete! !

!ExternalStructure methodsFor: 'printing' stamp: 'jmv 8/31/2018 12:14:52'!
longPrintOn: aStream 
	"Append to the argument, aStream, the names and values of all the record's variables."
	| fields |
	fields := self class fields.
	(fields isEmpty or: [fields first isNil])
		ifTrue: [fields := #()]
		ifFalse: [fields first isArray ifFalse: [fields := Array with: fields]].
	fields do: [ :field |
		field first ifNotNil:
			[aStream nextPutAll: field first; nextPut: $:; space; tab.
			(self perform: field first) printOn: aStream.
			aStream newLine]].! !

!ExternalType methodsFor: 'printing' stamp: 'ar 12/2/1999 17:02'!
printOn: aStream
	referentClass == nil
		ifTrue:[aStream nextPutAll: (AtomicTypeNames at: self atomicType)]
		ifFalse:[aStream nextPutAll: referentClass name].
	self isPointerType ifTrue:[aStream nextPut: $*].! !

!ExternalType methodsFor: 'printing' stamp: 'nice 5/21/2006 21:40'!
storeOn: aStream
	referentClass == nil
		ifTrue:[aStream nextPutAll: ExternalType name; space; nextPutAll: (AtomicTypeNames at: self atomicType)]
		ifFalse:[aStream nextPut: $(; nextPutAll: ExternalType name; space; nextPutAll: #structTypeNamed:; space;  store: referentClass name; nextPut: $)].
	self isPointerType ifTrue: [aStream space; nextPutAll: #asPointer].! !

!FFIConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:54'!
initialize
	"FFIConstants initialize"
	self initializeTypeConstants.
	self initializeErrorConstants.
	self initializeCallingConventions.! !

!FFIConstants class methodsFor: 'pool initialization' stamp: 'eem 8/4/2009 19:49'!
initializeCallingConventions
	"These constants map onto the flags inst var of an ExternalFunction.
	 The first eight bits define the calling convention.  Attribute flags
	 occupy bits 8 on up."
	"FFIConstants initializeCallingConventions"
	FFICallTypesMask := 255.
	FFICallTypeCDecl := 0.
	FFICallTypeApi := 1.
	FFICallFlagThreaded := 256! !

!FFIConstants class methodsFor: 'pool initialization' stamp: 'eem 11/5/2009 10:21'!
initializeErrorConstants
	"FFIConstants initializeErrorConstants"

	"No callout mechanism available"
	FFINoCalloutAvailable := -1.
	"generic error"
	FFIErrorGenericError := 0.
	"primitive invoked without ExternalFunction"
	FFIErrorNotFunction := 1.
	"bad arguments to primitive call"
	FFIErrorBadArgs := 2.

	"generic bad argument"
	FFIErrorBadArg := 3.
	"int passed as pointer"
	FFIErrorIntAsPointer := 4.
	"bad atomic type (e.g., unknown)"
	FFIErrorBadAtomicType := 5.
	"argument coercion failed"
	FFIErrorCoercionFailed := 6.
	"Type check for non-atomic types failed"
	FFIErrorWrongType := 7.
	"struct size wrong or too large"
	FFIErrorStructSize := 8.
	"unsupported calling convention"
	FFIErrorCallType := 9.
	"cannot return the given type"
	FFIErrorBadReturn := 10.
	"bad function address"
	FFIErrorBadAddress := 11.
	"no module given but required for finding address"
	FFIErrorNoModule := 12.
	"function address not found"
	FFIErrorAddressNotFound := 13.
	"attempt to pass 'void' parameter"
	FFIErrorAttemptToPassVoid := 14.
	"module not found"
	FFIErrorModuleNotFound := 15.
	"external library invalid"
	FFIErrorBadExternalLibrary := 16.
	"external function invalid"
	FFIErrorBadExternalFunction := 17.
	"ExternalAddress points to ST memory (don't you dare to do this!!)"
	FFIErrorInvalidPointer := 18.
	"Stack frame required more than 16k bytes to pass arguments."
	FFIErrorCallFrameTooBig := 19! !

!FFIConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 18:34'!
initializeTypeConstants
	"type void"
	FFITypeVoid := 0.

	"type bool"
	FFITypeBool := 1.

	"basic integer types.
	note: (integerType anyMask: 1) = integerType isSigned"

	FFITypeUnsignedByte := 2.
	FFITypeSignedByte := 3.
	FFITypeUnsignedShort := 4.
	FFITypeSignedShort := 5.
	FFITypeUnsignedInt := 6.
	FFITypeSignedInt := 7.

	"64bit types"
	FFITypeUnsignedLongLong := 8.
	FFITypeSignedLongLong := 9.

	"special integer types"
	FFITypeUnsignedChar := 10.
	FFITypeSignedChar := 11.

	"float types"
	FFITypeSingleFloat := 12.
	FFITypeDoubleFloat := 13.

	"type flags"
	FFIFlagAtomic := 16r40000. "type is atomic"
	FFIFlagPointer := 16r20000. "type is pointer to base type"
	FFIFlagStructure := 16r10000. "baseType is structure of 64k length"
	FFIStructSizeMask := 16rFFFF. "mask for max size of structure"
	FFIAtomicTypeMask := 16r0F000000. "mask for atomic type spec"
	FFIAtomicTypeShift := 24. "shift for atomic type"
! !

!ExternalAddress methodsFor: 'arithmetic' stamp: 'nice 4/13/2018 07:41'!
+ offset
	"Create an address that is offset by the given number of bytes.
	More tricky than one would think due to the FFI's handling of ExternalAddress
	as pointer to an object so that 'self unsignedLongAt: ' would dereference."

	| bytes |
	"Convert xaddr -> bytes"
	bytes := self asByteArrayPointer.
	"Update bytes using platform dependent accessors"
	self size = 4
		ifTrue: [bytes unsignedLongAt: 1 put: (bytes unsignedLongAt: 1) + offset]
		ifFalse: [bytes unsignedLongLongAt: 1 put: (bytes unsignedLongLongAt: 1) + offset].
	"Convert bytes -> xaddr"
	^bytes asExternalPointer! !

!ExternalAddress methodsFor: 'private' stamp: 'nice 4/13/2018 07:48'!
asByteArrayPointer
	"Answer a ByteArray containing a copy of pointer to the contents of the receiver."
	| sz |
	^(ByteArray basicNew: (sz := self size))
		replaceFrom: 1 to: sz with: self startingAt: 1 "answers self"! !

!ExternalAddress methodsFor: 'private' stamp: 'jcg 2/16/2010 01:00'!
asExternalPointer
	"No need to convert."
	^self! !

!ExternalAddress methodsFor: 'converting' stamp: 'nice 4/13/2018 07:42'!
asInteger
	"convert address to integer"
	^ self asByteArrayPointer integerAt: 1 size: self size signed: false! !

!ExternalAddress methodsFor: 'converting' stamp: 'nice 4/13/2018 07:44'!
fromInteger: address
	"set my handle to point at address."
	"Do we really need this? bf 2/21/2001 23:48"

	| sz pointer |
	sz := self size.
	pointer := ByteArray new: sz.
	pointer integerAt: 1 put: address size: sz signed: false. .
	self basicAt: 1 put: (pointer byteAt: 1);
		basicAt: 2 put: (pointer byteAt: 2);
		basicAt: 3 put: (pointer byteAt: 3);
		basicAt: 4 put: (pointer byteAt: 4).
	sz = 8 ifTrue:
		[self basicAt: 5 put: (pointer byteAt: 5);
			basicAt: 6 put: (pointer byteAt: 6);
			basicAt: 7 put: (pointer byteAt: 7);
			basicAt: 8 put: (pointer byteAt: 8)]! !

!ExternalAddress methodsFor: 'release' stamp: 'ar 11/22/1999 04:25'!
beNull
	"Make the receiver a NULL pointer"
	self atAllPut: 0.! !

!ExternalAddress methodsFor: 'release' stamp: 'ar 11/28/1999 23:40'!
free
	"Primitive. Free the object pointed to on the external heap.
	Dangerous - may break your system if the receiver hasn't been
	allocated by ExternalAddress class>>allocate:. No checks are done."
	<primitive:'primitiveFFIFree' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ExternalAddress methodsFor: 'accessing' stamp: 'ar 11/21/1999 15:43'!
byteAt: byteOffset
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset! !

!ExternalAddress methodsFor: 'accessing' stamp: 'ar 11/21/1999 15:43'!
byteAt: byteOffset put: value
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset put: value! !

!ExternalAddress methodsFor: 'accessing' stamp: 'jcg 6/4/2010 02:40'!
isExternalAddress
	"Return true if the receiver describes the address of an object in the outside world"
	^true! !

!ExternalAddress methodsFor: 'copying' stamp: 'tbn 4/26/2012 14:10'!
clone

	<primitive: 148>
	self primitiveFailed! !

!ExternalAddress methodsFor: 'copying' stamp: 'eem 2/21/2016 15:31'!
shallowCopy
	"Re-implemented to avoid superclass call to #new:"
	"But superclass's shallowCopy sends basicNew: and basicNew: is ok. eem 2/21/2016 15:31"
	^self clone! !

!ExternalAddress methodsFor: 'initialize-release' stamp: 'EstebanLorenzano 3/10/2016 16:39'!
finalize
	"I am an executor (a copy) of an ExternalAddress that was just garbage collected.
	I must finalize. my mission is to free memory"
	self isNull ifTrue: [^self].
	self free! !

!ExternalAddress methodsFor: 'testing' stamp: 'ar 11/16/2006 15:35'!
isNull
	"Answer true if I am a null pointer"
	1 to: self size do:[:i| (self at: i) = 0 ifFalse:[^false]].
	^true! !

!ExternalAddress class methodsFor: 'instance creation' stamp: 'ar 11/28/1999 23:20'!
allocate: byteSize
	"Primitive. Allocate an object on the external heap."
	<primitive:'primitiveFFIAllocate' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ExternalAddress class methodsFor: 'instance creation' stamp: 'nice 5/19/2006 03:24'!
gcallocate: byteSize
	"Primitive. Allocate an object on the external heap.
	The external memory will be freed when i am garbage collected.
	BEWARE: there should be no copy of self, nor any pointer to a sub part..."
	
	| externalAddress |
	externalAddress := self allocate: byteSize.
	self finalizationRegistry add: externalAddress.
	^externalAddress! !

!ExternalAddress class methodsFor: 'instance creation' stamp: 'dtl 9/12/2015 18:04'!
loadSymbol: moduleSymbol module: module 
	<primitive: 'primitiveLoadSymbolFromModule' module: 'SqueakFFIPrims'>
	^ self primitiveFailed! !

!ExternalAddress class methodsFor: 'instance creation' stamp: 'eem 2/22/2016 15:42'!
new: n
	"Only create ExternalAddresses of the right size."
	^n = wordSize
		ifTrue: [super new: n]
		ifFalse: [self shouldNotImplement]! !

!ExternalAddress class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:54:29'!
numElements
	"External addresses are either four or eight bytes long."
	^ wordSize! !

!ExternalAddress class methodsFor: 'class initialization' stamp: 'eem 2/20/2016 17:29'!
initialize
	wordSize := Smalltalk wordSize! !

!ExternalAddress class methodsFor: 'accessing' stamp: 'eem 2/16/2016 12:34'!
wordSize
	^wordSize! !

!ExternalForm methodsFor: 'initialization' stamp: 'jmv 5/4/2016 12:43'!
allocateSpace
	"Convenient way to allocate space for the pixels.  This isn't done by default, because it is common to use a pointer obtained from elsewhere."
	| addr |
	pointer ifNotNil: [self error: 'space is already allocated'].
	addr := ExternalAddress gcallocate: width*height * depth abs // 8. "area times bytes/pixel"
	self setManualSurfacePointer: addr.! !

!ExternalForm methodsFor: 'initialization' stamp: 'jcg 1/29/2010 01:29'!
destroySurface
	"Users must call this explicitly when this object is no longer needed; otherwise, resource-leakage will occur in the SurfacePlugin"
	bits ifNotNil: [:surfaceID |
		bits := nil.
		self primDestroyManualSurface: surfaceID
	].! !

!ExternalForm methodsFor: 'initialization' stamp: 'jcg 6/4/2010 02:04'!
initialize
	super initialize.
	pointer := nil.
	! !

!ExternalForm methodsFor: 'initialization' stamp: 'jmv 5/4/2016 12:47'!
setExtent: extent depth: bitsPerPixel
	bits ifNotNil: [self error: 'bits are already set'].
	(#(8 16 32) includes: bitsPerPixel abs)
		ifFalse: [self error: 'only +/- 8, 16 or 32 bits for now'.  "see 'rowPitch' below"].
	width := extent x asInteger.
	width < 0 ifTrue: [width := 0].
	height := extent y asInteger.
	height < 0 ifTrue: [height := 0].
	depth := bitsPerPixel.
	bits := self 
		primCreateManualSurfaceWidth: width 
		height: height 
		rowPitch: width * (bitsPerPixel abs/8)
		depth: bitsPerPixel abs
		isMSB: bitsPerPixel > 0.! !

!ExternalForm methodsFor: 'initialization' stamp: 'jcg 1/29/2010 01:31'!
setExtent: extent depth: bitsPerPixel bits: aPointer
	self setExtent: extent depth: bitsPerPixel.
	self setManualSurfacePointer: aPointer.! !

!ExternalForm methodsFor: 'initialization' stamp: 'jmv 12/22/2016 16:04:33'!
setManualSurfacePointer: newPointer "ExternalStructure, ExternalAddress, or nil"
	"Set the memory-location of the image data.  It is OK to set a NULL pointer; in this case, any attempt to BitBlt to or from the form will result in a primitive-failure."
	| integer |
	pointer := newPointer.
	pointer ifNil: [^self primManualSurface: bits setPointer: 0].
	pointer isExternalAddress
		ifFalse: ["must already be ExternalStructure, so nothing to do"]
		ifTrue: [pointer := ExternalData 
							fromHandle: newPointer 
							type: ExternalType void asPointerType].
	"The primitive expects an unsigned integer arg, not an ExternalAddress."
	"NOTE: it used to be acceptable for 'newPointer' to be an Integer... 
	 if you get a MNU for #getHandle here, you should update your code 
	 to pass in either an ExternalStructure or an ExternalAddress."
	integer := Smalltalk wordSize caseOf: {
			[ 4 ] -> [ pointer getHandle unsignedLongAt: 1 bigEndian: Smalltalk isBigEndian ].
			[ 8 ] -> [ pointer getHandle unsignedLong64At: 1 bigEndian: Smalltalk isBigEndian ]
		}
		otherwise: [ self error ].
	self primManualSurface: bits setPointer: integer! !

!ExternalForm methodsFor: 'accessing' stamp: 'jcg 2/16/2010 00:01'!
pointer
	^pointer! !

!ExternalForm methodsFor: 'primitives' stamp: 'jcg 1/30/2010 02:52'!
primCreateManualSurfaceWidth: aWidth height: aHeight rowPitch: rowPitch depth: aDepth isMSB: isMSB
	<primitive: 'primitiveCreateManualSurface' module: 'SqueakFFIPrims'>
	self primitiveFailed
! !

!ExternalForm methodsFor: 'primitives' stamp: 'jcg 1/30/2010 02:52'!
primDestroyManualSurface: surfaceID
	<primitive: 'primitiveDestroyManualSurface' module: 'SqueakFFIPrims'>
	self primitiveFailed
! !

!ExternalForm methodsFor: 'primitives' stamp: 'jcg 1/30/2010 02:52'!
primManualSurface: surfaceID setPointer: aPointer
	"The 'surfaceID' is a handle returned by #primitiveCreateManualSurface from SurfacePlugin. The pointer is a 32-bit unsigned integer that SurfacePlugin casts to a void*."
	<primitive: 'primitiveSetManualSurfacePointer' module: 'SqueakFFIPrims'>
	self primitiveFailed
! !

!ExternalObject methodsFor: 'private' stamp: 'ar 11/16/1999 20:25'!
getHandle
	"Private. Return the handle used to represent the external entitiy."
	^handle! !

!ExternalObject methodsFor: 'private' stamp: 'ar 11/16/1999 20:26'!
setHandle: anObject
	"Private. Set the handle used to represent the external entity."
	handle := anObject! !

!ExternalObject methodsFor: 'testing' stamp: 'jcg 6/4/2010 02:40'!
isExternalAddress
	"Return true if the receiver describes the address of an object in the outside world"
	^false! !

!ExternalObject methodsFor: 'testing' stamp: 'ar 11/16/2006 15:36'!
isNull
	"Answer true if the receiver currently is a NULL pointer"
	^handle == nil or:[handle isNull]! !

!ExternalObject class methodsFor: 'class initialization' stamp: 'ar 11/19/1999 22:37'!
initialize
	"ExternalObject initialize"
"	Smalltalk addToStartUpList: self after: ShortRunArray."
	Smalltalk addToStartUpList: self.! !

!ExternalObject class methodsFor: 'system startup' stamp: 'ar 11/28/1999 23:37'!
install
	"Notify all instances of the receiver that we're coming up on a new platform.
	Note: The default implementation does nothing since the general external
	objects are cleaned up by ExternalAddress>>startUp: but subclasses may
	implement this method so that the appropriate action for existing instances can
	be taken."! !

!ExternalObject class methodsFor: 'system startup' stamp: 'ar 11/28/1999 23:36'!
installSubclasses
	"Notify all the subclasses of ExternalObject that we are starting up on a new platform."
	self withAllSubclassesDo:[:cls| cls install].! !

!ExternalFunction methodsFor: 'accessing' stamp: 'ar 11/19/1999 19:13'!
argTypes
	^argTypes! !

!ExternalFunction methodsFor: 'accessing' stamp: 'eem 10/28/2009 17:03'!
errorCodeName
	^nil! !

!ExternalFunction methodsFor: 'accessing' stamp: 'ar 11/19/1999 19:13'!
flags
	^flags! !

!ExternalFunction methodsFor: 'accessing' stamp: 'ar 11/17/1999 19:55'!
module
	^nil! !

!ExternalFunction methodsFor: 'printing' stamp: 'ar 11/19/1999 16:35'!
callingConventionString
	(flags allMask: FFICallTypeApi) 
		ifTrue:[^'apicall']
		ifFalse:[^'cdecl']! !

!ExternalFunction methodsFor: 'initialization' stamp: 'ar 11/29/1999 00:35'!
initialize
	"Initialize the receiver"
	handle := ExternalAddress new.! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:54'!
invoke
	^self invokeWithArguments: #()! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:53'!
invokeWith: arg1
	^self invokeWithArguments: (Array with: arg1)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:53'!
invokeWith: arg1 with: arg2
	^self invokeWithArguments: (Array with: arg1 with: arg2)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:53'!
invokeWith: arg1 with: arg2 with: arg3
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 19:08'!
invokeWith: arg1 with: arg2 with: arg3 with: arg4
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:53'!
invokeWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4 with: arg5)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'ar 11/19/1999 21:54'!
invokeWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6)! !

!ExternalFunction methodsFor: 'invoking' stamp: 'eem 8/23/2018 14:05'!
invokeWithArguments: argArray
	"Manually invoke the receiver, representing an external function."
	<primitive: 'primitiveCalloutWithArgs' module: 'SqueakFFIPrims' error: ec>
	^self externalCallFailed! !

!ExternalFunction methodsFor: 'invoking' stamp: 'eem 2/20/2016 17:25'!
tryInvokeWithArguments: argArray
	"Sent from the debugger to simulate an FFI call."
	<primitive: 'primitiveCalloutWithArgs' module: 'SqueakFFIPrims' error: ec>
	
	^thisContext class primitiveFailTokenFor: ec! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'ar 12/2/1999 16:20'!
atomicTypeNamed: aString
	^ExternalType atomicTypeNamed: aString! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'ar 11/17/1999 19:58'!
callingConventionFor: aString
	"Return the constant describing the calling convention for the given string specification or nil if unknown."
	aString = 'cdecl:' ifTrue:[^self callTypeCDecl].
	aString = 'apicall:' ifTrue:[^self callTypeAPI].
	^nil! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'eem 8/4/2009 19:44'!
callingConventionModifierFor: aString
	"Return the constant describing the calling convention modifier for the given string specification or nil if unknown."
	aString = 'threaded' ifTrue:[^FFICallFlagThreaded].
	^nil! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'ar 12/2/1999 16:49'!
forceTypeNamed: aString
	^ExternalType forceTypeNamed: aString! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'ar 12/2/1999 16:30'!
isValidType: anObject
	^anObject isBehavior and:[anObject includesBehavior: ExternalStructure]! !

!ExternalFunction class methodsFor: 'compiler support' stamp: 'ar 12/2/1999 16:21'!
structTypeNamed: aString
	^ExternalType structTypeNamed: aString! !

!ExternalFunction class methodsFor: 'constants' stamp: 'ar 11/19/1999 16:36'!
callTypeAPI
	^FFICallTypeApi! !

!ExternalFunction class methodsFor: 'constants' stamp: 'ar 11/19/1999 16:36'!
callTypeCDecl
	^FFICallTypeCDecl! !

!ExternalFunction class methodsFor: 'error handling' stamp: 'ar 11/19/1999 14:17'!
errorMessageFor: code
	"Return the error message for the given error code from the foreign function interface"
	^FFIErrorMessages at: code ifAbsent:['Call to external function failed'].! !

!ExternalFunction class methodsFor: 'error handling' stamp: 'ar 11/19/1999 19:09'!
externalCallFailed
	"Raise an error after a failed call to an external function"
	| errCode |
	errCode := self getLastError. "this allows us to look at the actual error code"
	^self error: (self errorMessageFor: errCode).! !

!ExternalFunction class methodsFor: 'error handling' stamp: 'eem 11/19/2009 10:47'!
externalCallFailedWith: primErrorCode
	"Raise an error after a failed call to an external function.
	 The primFailCode could be any of:
		- a symbol; one of the standard primitive errors defined in Smalltalk primitiveErrorTable
		- nil; the VM does not support primitive errors and is not providing error codes
		- an integer; one of the FFI codes incremented by Smalltalk primitiveErrorTable size + 2
		  so as not to clash with the standard primitive errors."
	^self error: (primErrorCode isInteger
					ifTrue: [self errorMessageFor: primErrorCode - (Smalltalk primitiveErrorTable size + 2)]
					ifFalse: [primErrorCode isNil
								ifTrue: ['Call to external function failed']
								ifFalse: [primErrorCode]])! !

!ExternalFunction class methodsFor: 'error handling' stamp: 'ar 11/28/1999 18:37'!
getLastError
	"Return the last error from an external call.
	Only valid immediately after the external call failed."
	<primitive: 'primitiveFFIGetLastError' module:'SqueakFFIPrims'>
	^-1! !

!ExternalFunction class methodsFor: 'class initialization' stamp: 'ar 3/9/2010 21:45'!
initialize
	"ExternalFunction initialize"
	FFIConstants initialize. "ensure proper initialization"
	self initializeErrorMessages.
	(Smalltalk specialObjectsArray at: 47) == self 
		ifFalse:[Smalltalk recreateSpecialObjectsArray].
! !

!ExternalFunction class methodsFor: 'class initialization' stamp: 'eem 11/5/2009 10:24'!
initializeErrorMessages
	"ExternalFunction initializeErrorMessages"
	FFIErrorMessages := Dictionary new.
	FFIErrorMessages
		at: FFINoCalloutAvailable put: 'Callout mechanism not available';
		at: FFIErrorGenericError put: 'A call to an external function failed';
		at: FFIErrorNotFunction put: 'Only ExternalFunctions can be called';
		at: FFIErrorBadArgs put: 'Bad arguments in primitive invocation';
		at: FFIErrorBadArg put: 'Bad argument for external function';
		at: FFIErrorIntAsPointer put: 'Cannot use integer as pointer';
		at: FFIErrorBadAtomicType put: 'Unknown atomic type in external call';
		at: FFIErrorCoercionFailed put: 'Could not coerce arguments';
		at: FFIErrorWrongType put: 'Wrong type in external call';
		at: FFIErrorStructSize put: 'Bad structure size in external call';
		at: FFIErrorCallType put: 'Unsupported calling convention';
		at: FFIErrorBadReturn put: 'Cannot return the given type';
		at: FFIErrorBadAddress put: 'Bad function address';
		at: FFIErrorNoModule put: 'No module to load address from';
		at: FFIErrorAddressNotFound put: 'Unable to find function address';
		at: FFIErrorAttemptToPassVoid put: 'Cannot pass ''void'' parameter';
		at: FFIErrorModuleNotFound put: 'External module not found';
		at: FFIErrorBadExternalLibrary put: 'External library is invalid';
		at: FFIErrorBadExternalFunction put: 'External function is invalid';
		at: FFIErrorInvalidPointer put: 'Attempt to pass invalid pointer';
		at: FFIErrorCallFrameTooBig put: 'Call requires more than 16k of stack space';
	yourself! !

!ExternalFunction class methodsFor: 'class initialization' stamp: 'ar 3/9/2010 21:45'!
unload
	"Clean out the splObj array"
	Smalltalk specialObjectsArray from: 44 to: 48 put: nil.
! !

!ExternalLibraryFunction methodsFor: 'comparing' stamp: 'eem 10/28/2009 17:07'!
analogousCodeTo: anObject
	^(anObject isKindOf: ExternalLibraryFunction)
	and: [flags = anObject flags
	and: [argTypes = anObject argTypes
	and: [name = anObject name
	and: [module = anObject module
	and: [errorCodeName = anObject errorCodeName]]]]]! !

!ExternalLibraryFunction methodsFor: 'accessing' stamp: 'eem 10/28/2009 17:07'!
errorCodeName
	^errorCodeName! !

!ExternalLibraryFunction methodsFor: 'accessing' stamp: 'ar 11/17/1999 19:55'!
module
	^module! !

!ExternalLibraryFunction methodsFor: 'accessing' stamp: 'eem 10/28/2009 17:07'!
setErrorCodeName: aString
	errorCodeName := aString! !

!ExternalLibraryFunction methodsFor: 'accessing' stamp: 'das 5/23/2005 10:50'!
setModule: aString
	"Private. Hack the module"
	module := aString.! !

!ExternalLibraryFunction methodsFor: 'private' stamp: 'ar 11/19/1999 19:12'!
name: aName module: aModule flags: anInteger argTypes: argTypeArray

	name := aName.
	module := aModule.
	flags := anInteger.
	argTypes := argTypeArray.! !

!ExternalLibraryFunction class methodsFor: 'instance creation' stamp: 'ar 11/17/1999 14:52'!
name: aName module: aModule callType: callType returnType: retType argumentTypes: argTypes
	^self new
		name: aName
		module: aModule
		flags: callType
		argTypes: (Array with: retType), argTypes! !

!ExternalLibrary methodsFor: 'initialization' stamp: 'ar 12/8/1999 21:49'!
forceLoading
	"Primitive. Force loading the given library.
	The primitive will fail if the library is not available
	or if anything is wrong with the receiver."
	<primitive: 'primitiveForceLoad' module:'SqueakFFIPrims'>
	^self externalCallFailed "The primitive will set the error code"! !

!ExternalLibrary methodsFor: 'initialization' stamp: 'ar 11/29/1999 00:35'!
initialize
	"Initialize the receiver"
	name := self class moduleName.
	handle := ExternalAddress new.! !

!ExternalLibrary methodsFor: 'accessing' stamp: 'ar 11/17/1999 19:35'!
handle
	^handle! !

!ExternalLibrary class methodsFor: 'accessing' stamp: 'ar 11/17/1999 19:33'!
moduleName
	"Return the name of the module for this library"
	^nil! !

!ExternalStructure methodsFor: 'release' stamp: 'ar 11/28/1999 23:21'!
free
	"Free the handle pointed to by the receiver"
	(handle ~~ nil and:[handle isExternalAddress]) ifTrue:[handle free].
	handle := nil.! !

!ExternalStructure methodsFor: 'printing' stamp: 'eem 5/1/2018 15:18'!
compositeName
	^self class compositeName! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 12/2/1999 14:31'!
byteSize
	"Return the size in bytes of this structure."
	^self compiledSpec first bitAnd: FFIStructSizeMask! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/16/2018 09:25'!
checkFieldLayoutChange
	"Recompile the spec and field accessors if the layout changed.
	Answer true if the layout changed.
	This is usefull at system startup if some structure are machine dependent.
	No provision is made for correct initialization order of nested structures.
	The correct order of invocation is left at upper responsibility."
	
	| newCompiledSpec oldCompiledSpec |
	oldCompiledSpec := compiledSpec.
	newCompiledSpec := self compileFields: self fields withAccessors: #never.
	oldCompiledSpec = newCompiledSpec ifTrue: [^false].
	"only regenerate the automatically generated fields: the others are under user responsibility"
	compiledSpec := self compileFields: self fields withAccessors: #generated.
	ExternalType noticeModificationOf: self.
	^true! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 14:44'!
compileAlias: spec withAccessors: aSymbol
	"Define all the fields in the receiver.
	Return the newly compiled spec."
	| fieldName fieldType isPointerField externalType newCompiledSpec |
	fieldName := spec first.
	fieldType := spec second.
	isPointerField := fieldType last = $*.
	fieldType := fieldType copyWithout: $*.
	externalType := ExternalType atomicTypeNamed: fieldType.
	externalType == nil ifTrue:["non-atomic"
		Symbol hasInterned: fieldType ifTrue:[:sym|
			externalType := ExternalType structTypeNamed: sym]].
	externalType == nil ifTrue:[
		Transcript show:'(', fieldType,' is void)'.
		externalType := ExternalType void].
	isPointerField ifTrue:[externalType := externalType asPointerType].
	(fieldName notNil and:[self shouldGenerate: fieldName policy: aSymbol]) ifTrue:[
		self defineAliasAccessorsFor: fieldName
			type: externalType].
	newCompiledSpec := isPointerField 
		ifTrue:[WordArray with: 
					(ExternalType structureSpec bitOr: ExternalType pointerSpec)]
		ifFalse:[externalType compiledSpec].
	^newCompiledSpec! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 12/2/1999 15:35'!
compileAllFields
	"ExternalStructure compileAllFields"
	self withAllSubclassesDo:[:cls|
		cls compileFields.
	].! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 12/2/1999 14:28'!
compileFields
	"Compile the field definition of the receiver.
	Return the newly compiled spec."
	^self compileFields: self fields! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 14:31'!
compileFields: fieldSpec
	"Compile the field definition of the receiver.
	Return the newly compiled spec."
	compiledSpec := self compileFields: fieldSpec withAccessors: #never.
	ExternalType noticeModificationOf: self.
	^compiledSpec! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 14:36'!
compileFields: specArray withAccessors: aSymbol 
	"Compile a type specification for the FFI machinery.
	Return the newly compiled spec.
	Eventually generate the field accessors according to following rules:
	- aSymbol = #always always generate the accessors
	- aSymbol = #never never generate the accessors
	- aSymbol = #generated only generate the auto-generated accessors
	- aSymbol = #absent only generate the absent accessors"
	| byteOffset typeSpec newCompiledSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self compileAlias: specArray withAccessors: aSymbol].
	byteOffset := 1.
	typeSpec := WriteStream on: (WordArray new: 10).
	typeSpec nextPut: FFIFlagStructure.
	"dummy for size"
	specArray do:
		[:spec | | fieldName fieldType isPointerField externalType typeSize selfRefering |
		fieldName := spec first.
		fieldType := spec second.
		isPointerField := fieldType last = $*.
		fieldType := (fieldType findTokens: ' *') first.
		externalType := ExternalType atomicTypeNamed: fieldType.
		selfRefering := isPointerField and: [externalType isNil and: [fieldType = self asString]].
		selfRefering ifTrue: [
			externalType := ExternalType void asPointerType
		] ifFalse:[
			externalType == nil ifTrue: ["non-atomic"
				Symbol
					hasInterned: fieldType
					ifTrue: [:sym | externalType := ExternalType structTypeNamed: sym].
			].
			externalType == nil ifTrue:[
				Transcript show: '(' , fieldType , ' is void)'.
				externalType := ExternalType void.
			].
			isPointerField ifTrue: [externalType := externalType asPointerType: self pointerSize]].
			typeSize := externalType byteSize.
			spec size > 2 ifTrue: ["extra size"
				spec third < typeSize
					ifTrue: [^ self error: 'Explicit type size is less than expected'].
				typeSize := spec third.
			].
			(fieldName notNil and: [self shouldGenerate: fieldName policy: aSymbol]) ifTrue: [
				self defineFieldAccessorsFor: fieldName startingAt: byteOffset type: externalType.
			].
			typeSpec nextPutAll: (externalType embeddedSpecWithSize: typeSize).
			byteOffset := byteOffset + typeSize.
		].
	newCompiledSpec := typeSpec contents.
	newCompiledSpec at: 1 put: (byteOffset - 1 bitOr: FFIFlagStructure).
	^ newCompiledSpec! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 12/2/1999 14:28'!
compiledSpec
	"Return the compiled spec of the receiver"
	^compiledSpec ifNil:[self compileFields].! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'jmv 8/31/2018 12:11:18'!
defineAliasAccessorsFor: fieldName type: type
	"Define read/write accessors for the given field"
	| code refClass argName |
	(type isVoid and:[type isPointerType not]) ifTrue:[^self].
	refClass := type referentClass.
	code := String streamContents:[:s|
		s 
			nextPutAll: fieldName; newLine; tab;
			nextPutAll:'"This method was automatically generated"'; newLine; tab;
			nextPut: $<; nextPutAll: #generated; nextPut: $>; newLine; tab.
		refClass == nil 
			ifTrue:[(type isAtomic and:[type isPointerType not]) 
				ifTrue:[s nextPutAll:'^handle']
				ifFalse:[s nextPutAll:'^ExternalData fromHandle: handle'.
						type isPointerType ifTrue:[s nextPutAll:' asExternalPointer'].
						s nextPutAll:' type: ';
						nextPutAll: type externalTypeName]]
			ifFalse:[s nextPutAll:'^', refClass name,' fromHandle: handle'.
					type isPointerType ifTrue:[s nextPutAll:' asExternalPointer']]].
	self compile: code classified: 'accessing' withStamp: nil notifying: nil.

	code := String streamContents:[:s|
		argName := refClass == nil 
			ifTrue:[(type isAtomic and:[type isPointerType not])
				ifTrue:['anObject']
				ifFalse:['anExternalData']]
			ifFalse:['a',refClass name].
		s
			nextPutAll: fieldName,': '; nextPutAll: argName; newLine; tab;
			nextPutAll:'"This method was automatically generated"'; newLine; tab;
			nextPut: $<; nextPutAll: #generated; nextPut: $>; newLine; tab.
		(refClass == nil and:[type isAtomic and:[type isPointerType not]])
			ifTrue:[s nextPutAll:'handle := ', argName]
			ifFalse:[s nextPutAll:'handle := ', argName,' getHandle'.
					type isPointerType ifTrue:[s nextPutAll:' asByteArrayPointer']]].
	self compile: code classified: 'accessing' withStamp: nil notifying: nil.! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'jmv 8/31/2018 12:13:06'!
defineFieldAccessorsFor: fieldName startingAt: byteOffset type: type
	"Define read/write accessors for the given field"
	| comment |
	(type isVoid and: [type isPointerType not]) ifTrue:[^self].
	comment := String streamContents: [:strm |
		strm newLine; tab; nextPutAll: '"This method was automatically generated. See '; nextPutAll: self class name; nextPutAll: '>>fields."'; newLine; tab.
		strm nextPut: $<; nextPutAll: #generated; nextPut: $>; newLine; tab.].
	self maybeCompileAccessor: fieldName, comment, (type readFieldAt: byteOffset)
		withSelector: fieldName asSymbol.
	self maybeCompileAccessor: fieldName,': anObject', comment, (type writeFieldAt: byteOffset with: 'anObject')
		withSelector: (fieldName, ':') asSymbol! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 12/2/1999 14:37'!
defineFields
	"Define all the fields in the receiver"
	self defineFields: self fields.! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 14:30'!
defineFields: fieldSpec
	"Define all the fields in the receiver"
	compiledSpec := self compileFields: fieldSpec withAccessors: #always.
	ExternalType noticeModificationOf: self.
	^compiledSpec! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'ar 11/29/1999 00:28'!
fields
	"Return the fields defining the receiver"
	^#()! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 21:23'!
shouldGenerate: fieldname policy: aSymbol 
	"Answer true if the field accessors must be compiled.
	Do so according to the following rules:
	- aSymbol = #always always generate the accessors
	- aSymbol = #never never generate the accessors
	- aSymbol = #generated only re-generate the auto-generated accessors
	- aSymbol = #absent only generate the absent accessors"
	aSymbol = #never ifTrue: [^ false].
	aSymbol = #always ifTrue: [^ true].
	aSymbol = #absent ifTrue: [^ (self methodDictionary includesKey: fieldname) not].
	aSymbol = #generated
		ifTrue: [^ (self methodDictionary includesKey: fieldname)
				and: [(self methodDictionary at: fieldname) pragmas
						anySatisfy: [:p | p keyword = #generated]]].
	self error: 'unknow generation policy'! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'nice 4/13/2018 14:53'!
sortStructs: structureClasses into: sortedClasses 
	"Sort the structure definitions so as to obtain a correct initialization order."
	
	[| structClass prevStructClass dependsOnOtherTypes |
	structureClasses isEmpty ifTrue: [^ self].
	structClass := structureClasses anyOne.
	
	[dependsOnOtherTypes := structClass typeNamesFromWhichIDepend.
	prevStructClass := structureClasses detect: [:c | c ~~ structClass and: [dependsOnOtherTypes includes: c name]] ifNone: [nil].
	prevStructClass isNil]
		whileFalse: [structClass := prevStructClass].

	"we found a structure/alias which does not depend on other structures/aliases
	add the corresponding class to the initialization list"
	sortedClasses add: (structureClasses remove: structClass)] repeat! !

!ExternalStructure class methodsFor: 'field definition' stamp: 'jmv 8/31/2018 12:20:47'!
typeNamesFromWhichIDepend
	"Answer the set of type names of my fields (including pointer stars)"
	| f |
	(f := self fields) isEmpty ifTrue: [^Set new].
	f first isArray ifFalse: [^Set with: f second].
	^(f collect: [:e | e second]) as: Set! !

!ExternalStructure class methodsFor: 'class management' stamp: 'ar 11/22/1999 04:12'!
rename: aString
	| oldName |
	oldName := name.
	super rename: aString.
	oldName = name ifFalse:[ExternalType noticeRenamingOf: self from: oldName to: name].! !

!ExternalStructure class methodsFor: 'instance creation' stamp: 'ar 12/1/1999 15:58'!
externalNew
	"Create an instance of the receiver on the external heap"
	^self fromHandle: (ExternalAddress allocate: self byteSize)! !

!ExternalStructure class methodsFor: 'instance creation' stamp: 'ar 11/29/1999 00:36'!
fromHandle: aHandle
	^self basicNew setHandle: aHandle! !

!ExternalStructure class methodsFor: 'instance creation' stamp: 'ar 12/1/1999 15:58'!
new
	^self fromHandle: (ByteArray new: self byteSize)! !

!ExternalStructure class methodsFor: 'converting' stamp: 'eem 5/1/2018 15:17'!
compositeName
	^'struct'! !

!ExternalStructure class methodsFor: 'converting' stamp: 'ar 12/2/1999 16:55'!
externalType
	"Return an external type describing the receiver as a structure"
	^ExternalType structTypeNamed: self name! !

!ExternalStructure class methodsFor: 'converting' stamp: 'eem 2/16/2016 10:32'!
typedef
	^self externalType typedef! !

!ExternalStructure class methodsFor: 'class initialization' stamp: 'jmv 1/13/2016 10:22'!
initialize
	ExternalType initialize.
	self compileFields! !

!ExternalStructure class methodsFor: 'compiling' stamp: 'jmv 4/16/2017 20:53:30'!
maybeCompileAccessor: aString withSelector: selector

	(self compiledMethodAt: selector ifAbsent: []) ifNotNil: [ :existingMethod |
		existingMethod getSourceFromFile asString = aString ifTrue: [
			^self]].
	self compile: aString classified: #accessing withStamp: nil notifying: nil! !

!ExternalStructure class methodsFor: 'accessing' stamp: 'eem 2/16/2016 12:42'!
pointerSize
	"Answer the size of pointers for this class.  By default answer nil.
	 Subclasses that contain pointers must define the size of a pointer if the code is to operate on 64-bit and 32-bit platforms.
	 Currently we have no way of converting a type between 32- and 64- bit versions beyond recompiling it."
	^nil! !

!ExternalStructure class methodsFor: 'system startup' stamp: 'nice 4/13/2018 12:36'!
install
	"Resuming the image on another architecture may require a re-compilation of structure layout."
	| newPlatform |
	newPlatform := Smalltalk platformName.
	PreviousPlatform = newPlatform
		ifFalse:
			[self recompileStructures.
			PreviousPlatform := newPlatform]! !

!ExternalStructure class methodsFor: 'system startup' stamp: 'nice 4/13/2018 12:35'!
recompileStructures
	"Check and update the layout of all subclasses for host machine dependency.
	Arrange to check the inner nested structures first."
	
	"ExternalStructure recompileStructures"
	| sorted unsorted |
	unsorted := self withAllSubclasses.
	sorted := OrderedCollection new: unsorted size.
	self sortStructs: unsorted into: sorted.
	sorted do: [:e | e checkFieldLayoutChange]! !

!ExternalData methodsFor: 'conversion' stamp: 'hg 2/25/2000 14:51'!
fromCString
	"Assume that the receiver represents a C string and convert it to a Smalltalk string. hg 2/25/2000 14:18"

	| stream index char |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	stream := WriteStream on: String new.
	index := 1.
	[(char := handle unsignedCharAt: index) = 0 asCharacter] whileFalse: [
		stream nextPut: char.
		index := index + 1].
	^stream contents! !

!ExternalData methodsFor: 'conversion' stamp: 'jmv 3/7/2018 18:52:40'!
fromCStringAsByteArray
	"Assume that the receiver represents a C string and convert it to a Smalltalk ByteArray"

	| stream index byte |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	stream := WriteStream on: ByteArray new.
	index := 1.
	[(byte := handle unsignedByteAt: index) = 0] whileFalse: [
		stream nextPut: byte.
		index := index + 1].
	^stream contents! !

!ExternalData methodsFor: 'conversion' stamp: 'jrd 4/23/2009 17:35'!
fromCStrings
	"Assume that the receiver represents a set of C strings and is teerminated by a empty string and convert it to a Smalltalk ordered collection of strings"

	| stream index char strings str |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	strings := OrderedCollection new.
	index := 1.
	[
		stream := WriteStream on: String new.
		[(char := handle unsignedCharAt: index) = 0 asCharacter]
			whileFalse: [
				stream nextPut: char.
				index := index + 1
			].
		str := stream contents.
		strings addLast: str.
		str size = 0
	] whileFalse.
	^strings! !

!ExternalData methodsFor: 'private' stamp: 'ar 11/21/1999 14:23'!
setHandle: aHandle type: aType
	handle := aHandle.
	type := aType.! !

!ExternalData class methodsFor: 'class initialization' stamp: 'ar 8/14/2006 23:18'!
compileFields
	"Ensure proper initialization of ExternalType when first loading"
	ExternalType initialize.
	^super compileFields! !

!ExternalData class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:38'!
initialize
	self compileFields! !

!ExternalData class methodsFor: 'field definition' stamp: 'ar 1/27/2000 01:23'!
fields
	"ExternalData defineFields"
	"Note: The definition is for completeness only.
	ExternalData is treated specially by the VM."
	^#(nil 'void*')! !

!ExternalData class methodsFor: 'instance creation' stamp: 'ar 12/2/1999 14:57'!
fromHandle: aHandle type: aType
	"Create a pointer to the given type"
	"ExternalData fromHandle: ExternalAddress new type: ExternalType float"
	^self basicNew setHandle: aHandle type: aType! !

!ExternalData class methodsFor: 'instance creation' stamp: 'ar 11/22/1999 04:28'!
new
	"You better not..."
	^self shouldNotImplement! !

!ExternalUnion class methodsFor: 'field definition' stamp: 'nice 4/13/2018 20:21'!
compileFields: specArray withAccessors: aSymbol 
	"Compile a type specification for the FFI machinery.
	Return the newly compiled spec.
	Eventually generate the field accessors according to the policy defined in aSymbol."
	| byteOffset maxByteSize typeSpec newCompiledSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self error: 'unions must have fields defined by sub-Array'].
	byteOffset := 1.
	maxByteSize := 0.
	typeSpec := WriteStream on: (WordArray new: specArray size + 1).
	typeSpec nextPut: FFIFlagStructure.
	"dummy for size"
	specArray do:
		[:spec |
		| fieldName fieldType isPointerField externalType typeSize selfRefering |
		fieldName := spec first.
		fieldType := spec second.
		isPointerField := fieldType last = $*.
		fieldType := (fieldType findTokens: ' *') first.
		externalType := ExternalType atomicTypeNamed: fieldType.
		selfRefering := isPointerField and: [externalType isNil and: [fieldType = self asString]].
		selfRefering ifTrue: [
			externalType := ExternalType void asPointerType
		] ifFalse:[
			externalType == nil ifTrue: ["non-atomic"
				Symbol
					hasInterned: fieldType
					ifTrue: [:sym | externalType := ExternalType structTypeNamed: sym].
			].
			externalType == nil ifTrue:[
				Transcript show: '(' , fieldType , ' is void)'.
				externalType := ExternalType void.
			].
			isPointerField ifTrue: [externalType := externalType asPointerType: self pointerSize]].
			typeSize := externalType byteSize.
			spec size > 2 ifTrue: ["extra size"
				spec third < typeSize
					ifTrue: [^ self error: 'Explicit type size is less than expected'].
				typeSize := spec third.
			].
			(fieldName notNil and: [self shouldGenerate: fieldName policy: aSymbol]) ifTrue: [
				self defineFieldAccessorsFor: fieldName startingAt: byteOffset type: externalType.
			].
			typeSpec nextPutAll: (externalType embeddedSpecWithSize: typeSize).
			maxByteSize := maxByteSize max: typeSize.
		].
	newCompiledSpec := typeSpec contents.
	newCompiledSpec at: 1 put: (maxByteSize bitOr: FFIFlagStructure).
	^ newCompiledSpec! !

!ExternalUnion class methodsFor: 'converting' stamp: 'eem 5/1/2018 15:18'!
compositeName
	^'union'! !

!ExternalType methodsFor: 'converting' stamp: 'ar 12/2/1999 16:41'!
asNonPointerType
	"convert the receiver into a non pointer type"
	self isPointerType
		ifTrue:[^referencedType]
		ifFalse:[^self]! !

!ExternalType methodsFor: 'converting' stamp: 'ar 12/2/1999 16:40'!
asPointerType
	"convert the receiver into a pointer type"
	self isPointerType
		ifTrue:[^self]
		ifFalse:[^referencedType]! !

!ExternalType methodsFor: 'converting' stamp: 'eem 2/16/2016 12:57'!
asPointerType: aPointerSize
	"convert the receiver into a pointer type"
	| type |
	type := self asPointerType.
	^type pointerSize = aPointerSize
		ifTrue: [type]
		ifFalse:
			[type copy pointerSize: aPointerSize; yourself]! !

!ExternalType methodsFor: 'accessing' stamp: 'ar 12/2/1999 14:15'!
atomicType
	^(self headerWord bitAnd: FFIAtomicTypeMask) >> FFIAtomicTypeShift! !

!ExternalType methodsFor: 'accessing' stamp: 'ar 12/2/1999 14:11'!
byteSize
	"Return the size in bytes of this type"
	^self headerWord bitAnd: FFIStructSizeMask! !

!ExternalType methodsFor: 'accessing' stamp: 'ar 12/2/1999 14:29'!
compiledSpec
	"Return the compiled spec of the receiver"
	^compiledSpec! !

!ExternalType methodsFor: 'accessing' stamp: 'eem 2/16/2016 12:43'!
pointerSize
	"Answer the pointer size of the receiver, if specified."
	^pointerSize! !

!ExternalType methodsFor: 'accessing' stamp: 'ar 12/2/1999 14:11'!
referentClass
	"Return the class specifying the receiver"
	^referentClass! !

!ExternalType methodsFor: 'private' stamp: 'ar 12/2/1999 15:19'!
compiledSpec: aWordArray
	compiledSpec := aWordArray.! !

!ExternalType methodsFor: 'private' stamp: 'ar 12/2/1999 20:34'!
embeddedSpecWithSize: typeSize
	"Return a compiled spec for embedding in a new compiled spec."
	| spec header |
	spec := self compiledSpec copy.
	header := spec at: 1.
	header := (header bitAnd: FFIStructSizeMask bitInvert32) bitOr: typeSize.
	spec at: 1 put: header.
	(self isStructureType and:[self isPointerType not])
		ifTrue:[spec := spec copyWith: self class structureSpec].
	^spec! !

!ExternalType methodsFor: 'private' stamp: 'ar 1/27/2000 00:22'!
externalTypeName
	^'ExternalType ', (AtomicTypeNames at: self atomicType), ' asPointerType'! !

!ExternalType methodsFor: 'private' stamp: 'ar 12/2/1999 14:11'!
headerWord
	"Return the compiled header word"
	^compiledSpec at: 1! !

!ExternalType methodsFor: 'private' stamp: 'ar 12/2/1999 20:30'!
newReferentClass: aClass
	"The class I'm referencing has changed. Update my spec."
	referentClass := aClass.
	self isPointerType ifTrue:[^self]. "for pointers only the referentClass changed"
	referentClass == nil ifTrue:[
		"my class has been removed - make me 'struct { void }'"
		compiledSpec := WordArray with: (FFIFlagStructure).
	] ifFalse:[
		"my class has been changed - update my compiledSpec"
		compiledSpec := referentClass compiledSpec.
	].! !

!ExternalType methodsFor: 'private' stamp: 'eem 2/16/2016 13:02'!
pointerSize: anInteger
	| spec |
	pointerSize := anInteger.
	(((spec := compiledSpec at: 1) anyMask: FFIFlagPointer)
	 and: [(spec bitAnd: FFIStructSizeMask) ~= anInteger]) ifTrue:
		[compiledSpec := compiledSpec shallowCopy.
		 compiledSpec at: 1 put: (spec bitClear: FFIStructSizeMask) + anInteger]! !

!ExternalType methodsFor: 'private' stamp: 'eem 2/16/2016 13:16'!
readFieldAt: byteOffset
	"Answer a string defining the accessor to an entity of the receiver type starting at the given byte offset. 
	 Private. Used for field definition only."
	self isPointerType ifTrue:
		[| accessor |
		accessor := self pointerSize caseOf: {
						[nil]	->	[#pointerAt:].
						[4]	->	[#shortPointerAt:].
						[8]	->	[#longPointerAt:] }.
		 ^String streamContents:
			[:s|
			 referentClass
				ifNil:
					[s nextPutAll: '^ExternalData fromHandle: (handle ', accessor, ' ';
						print: byteOffset;
						nextPutAll: ') type: ExternalType ';
						nextPutAll: (AtomicTypeNames at: self atomicType);
						nextPutAll: ' asPointerType']
				ifNotNil:
					[s nextPutAll: '^';
						print: referentClass;
						nextPutAll: ' fromHandle: (handle ', accessor, ' ';
						print: byteOffset;
						nextPut: $)]]].

	self isAtomic ifFalse: "structure type"
		[^String streamContents:[:s|
			s nextPutAll:'^';
				print: referentClass;
				nextPutAll:' fromHandle: (handle structAt: ';
				print: byteOffset;
				nextPutAll:' length: ';
				print: self byteSize;
				nextPutAll:')']].

	"Atomic non-pointer types"
	^String streamContents:
		[:s|
		s nextPutAll:'^handle ';
			nextPutAll: (AtomicSelectors at: self atomicType);
			space; print: byteOffset].! !

!ExternalType methodsFor: 'private' stamp: 'ar 12/2/1999 16:41'!
setReferencedType: aType
	referencedType := aType! !

!ExternalType methodsFor: 'private' stamp: 'eem 2/16/2016 13:16'!
writeFieldAt: byteOffset with: valueName
	"Answer a string defining the accessor to an entity of the receiver type starting at the given byte offset. 
	 Private. Used for field definition only."
	self isPointerType ifTrue:
		[| accessor |
		accessor := self pointerSize caseOf: {
						[nil]	->	[#pointerAt:].
						[4]	->	[#shortPointerAt:].
						[8]	->	[#longPointerAt:] }.
		^String streamContents:
			[:s|
			s nextPutAll:'handle ', accessor, ' ';
				print: byteOffset;
				nextPutAll:' put: ';
				nextPutAll: valueName;
				nextPutAll:' getHandle.']].

	self isAtomic ifFalse:[
		^String streamContents:[:s|
			s nextPutAll:'handle structAt: ';
				print: byteOffset;
				nextPutAll:' put: ';
				nextPutAll: valueName;
				nextPutAll:' getHandle';
				nextPutAll:' length: ';
				print: self byteSize;
				nextPutAll:'.']].

	^String streamContents:[:s|
		s nextPutAll:'handle ';
			nextPutAll: (AtomicSelectors at: self atomicType);
			space; print: byteOffset;
			nextPutAll:' put: ';
			nextPutAll: valueName].! !

!ExternalType methodsFor: 'testing' stamp: 'ar 12/2/1999 20:27'!
isAtomic
	"Return true if the receiver describes a built-in type"
	^self headerWord anyMask: FFIFlagAtomic! !

!ExternalType methodsFor: 'testing' stamp: 'ar 12/2/1999 14:14'!
isIntegerType
	"Return true if the receiver is a built-in integer type"
	| type |
	type := self atomicType.
	^type > FFITypeBool and:[type <= FFITypeUnsignedLongLong]! !

!ExternalType methodsFor: 'testing' stamp: 'ar 1/27/2000 00:29'!
isPointerType
	"Return true if the receiver represents a pointer type"
	^self isStructureType not and:[self headerWord anyMask: FFIFlagPointer]! !

!ExternalType methodsFor: 'testing' stamp: 'ar 12/2/1999 14:15'!
isSigned
	"Return true if the receiver is a signed type.
	Note: Only useful for integer types."
	^self atomicType anyMask: 1! !

!ExternalType methodsFor: 'testing' stamp: 'ar 12/2/1999 14:15'!
isStructureType
	"Return true if the receiver represents a structure type"
	^self headerWord anyMask: FFIFlagStructure! !

!ExternalType methodsFor: 'testing' stamp: 'ar 11/18/1999 18:28'!
isUnsigned
	"Return true if the receiver is an unsigned type.
	Note: Only useful for integer types."
	^self isSigned not! !

!ExternalType methodsFor: 'testing' stamp: 'ar 12/2/1999 14:16'!
isVoid
	"Return true if the receiver describes a plain 'void' type"
	^self isAtomic and:[self atomicType = 0]! !

!ExternalType methodsFor: 'printing' stamp: 'eem 2/16/2016 10:29'!
printAtomicType: spec on: aStream
	self assert: (spec anyMask: FFIFlagAtomic).
	aStream nextPutAll: (#(	'void' 'unsigned char' 'unsigned char' 'signed char'
							'unsigned short' 'short' 'unsigned long' 'long'
							'unsigned long long' 'long long' 'char' 'signed char'
							'float' 'double') at: ((spec bitAnd: FFIAtomicTypeMask) bitShift: FFIAtomicTypeShift negated) + 1).
	aStream space.
	(spec anyMask: FFIFlagPointer) ifTrue:
		[aStream nextPut: $*]! !

!ExternalType methodsFor: 'printing' stamp: 'jmv 8/31/2018 14:35:20'!
printStructureFieldStartingAt: initialSpecIndex withName: name inClass: structureClass on: aStream indent: indent
	"Print the structure's field starting at initialSpecIndex and answer the index in compiledSpec of the subsequent type."
	| spec subStructureClassBinding |
	aStream tab: indent.
	spec := compiledSpec at: initialSpecIndex.
	(spec anyMask: FFIFlagAtomic) ifTrue:
		[self printAtomicType: spec on: aStream.
		 aStream nextPutAll: (name ifNotNil: [name] ifNil: ['foo']).
		 ^initialSpecIndex + 1].
	 subStructureClassBinding := (structureClass >> name) literals detect:
									[:l| l isVariableBinding and: [l value inheritsFrom: ExternalStructure]].
	(spec bitClear: FFIStructSizeMask) = FFIFlagStructure ifTrue:
		[| next |
		 next := initialSpecIndex + 1.
		 aStream
			nextPutAll: subStructureClassBinding value compositeName;
			nextPutAll: ' {'.
		 subStructureClassBinding value fields withIndexDo:
			[:tuple :i|
			aStream newLine.
			next := self printStructureFieldStartingAt: next
						withName: tuple first
						inClass: subStructureClassBinding value
						on: aStream
						indent: indent + 1.
			aStream nextPut: $;].
		 aStream newLine; tab: indent; nextPut: $}.
		 name ifNotNil: [aStream space; nextPutAll: name].
		 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure]).
		 ^next <= compiledSpec size ifTrue: [next + 1] ifFalse: [next]].
	self assert: (spec anyMask: FFIFlagPointer).


self halt.
	(subStructureClassBinding value isKindOf: ExternalUnion)
			ifTrue: [aStream nextPutAll: 'union ']
			ifFalse: [aStream nextPutAll: 'struct '].
"	aStream nextPutAll: subStructureClassBinding value compositeName."




	aStream nextPutAll: subStructureClassBinding value name; nextPutAll: ' *'; nextPutAll: name.
	^initialSpecIndex + 1! !

!ExternalType methodsFor: 'printing' stamp: 'jmv 8/31/2018 12:48:49'!
printTypedefOn: s
	s nextPutAll: 'typedef '.
	(referentClass notNil
	 and: [(compiledSpec first bitClear: FFIStructSizeMask) = FFIFlagStructure])
		ifTrue:
			[| next |
			 next := 2.
			 s
				nextPutAll: referentClass compositeName;
				nextPutAll: ' {'.
			 referentClass fields withIndexDo:
				[:tuple :i|
				s newLine.
				next := self printStructureFieldStartingAt: next
							withName: tuple first
							inClass: referentClass
							on: s
							indent: 1.
				s nextPut: $;].
			 s newLine; nextPutAll: '} '.
			 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure])]
		ifFalse:
			[self printAtomicType: compiledSpec first on: s].
	s nextPutAll: (referentClass ifNotNil: [referentClass name] ifNil: ['foo'])! !

!ExternalType methodsFor: 'printing' stamp: 'eem 2/16/2016 10:05'!
typedef
	^String streamContents: [:s| self printTypedefOn: s]! !

!ExternalType class methodsFor: 'private' stamp: 'jmv 12/26/2016 17:09:18'!
atomicTypeNamed: aString
	| n |
	n _ aString.
	n = 'size_t' ifTrue: [
		n _ Smalltalk wordSize = 8 ifTrue: [ 'ulonglong' ] ifFalse: [ 'ulong' ]].
	^AtomicTypes at: n ifAbsent:[nil]! !

!ExternalType class methodsFor: 'private' stamp: 'ar 12/2/1999 16:50'!
forceTypeNamed: aString
	^self newTypeNamed: aString force: true! !

!ExternalType class methodsFor: 'private' stamp: 'ar 1/26/2000 21:41'!
newTypeNamed: aString force: aBool
	| sym type referentClass pointerType |
	sym := aString asSymbol.
	type := StructTypes at: aString ifAbsent:[nil].
	type == nil ifFalse:[^type].
	referentClass := Smalltalk at: sym ifAbsent:[nil].
	(referentClass isBehavior and:[referentClass includesBehavior: ExternalStructure])
		ifFalse:[referentClass := nil].
	"If we don't have a referent class and are not forced to create a type get out"
	(referentClass == nil and:[aBool not]) ifTrue:[^nil].
	type := self basicNew compiledSpec: 
		(WordArray with: self structureSpec).
	pointerType := self basicNew compiledSpec: 
		(WordArray with: self pointerSpec).
	type setReferencedType: pointerType.
	pointerType setReferencedType: type.
	type newReferentClass: referentClass.
	pointerType newReferentClass: referentClass.
	StructTypes at: sym put: type.
	^type! !

!ExternalType class methodsFor: 'private' stamp: 'jmv 12/20/2016 11:41:12'!
pointerSpec
	^(Smalltalk wordSize bitOr: FFIFlagPointer)! !

!ExternalType class methodsFor: 'private' stamp: 'ar 12/2/1999 16:48'!
structTypeNamed: aSymbol
	aSymbol == nil ifTrue:[^nil].
	^self newTypeNamed: aSymbol force: false! !

!ExternalType class methodsFor: 'private' stamp: 'ar 12/2/1999 20:34'!
structureSpec
	^FFIFlagStructure! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:56'!
bool
	^AtomicTypes at: 'bool'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/20/1999 17:29'!
byte
	"byte defaults to unsigned byte"
	^self unsignedByte! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/20/1999 17:29'!
char
	"char defaults to unsigned char"
	^self unsignedChar! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:56'!
double
	^AtomicTypes at: 'double'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
float
	^AtomicTypes at: 'float'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/22/1999 13:10'!
long
	^self signedLong! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/28/1999 23:43'!
sbyte
	^self signedByte! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/28/1999 23:43'!
schar
	^self signedChar! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/20/1999 17:26'!
short
	^self signedShort! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
signedByte
	^AtomicTypes at: 'sbyte'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
signedChar
	^AtomicTypes at: 'schar'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
signedLong
	^AtomicTypes at: 'long'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
signedLongLong
	^AtomicTypes at: 'longlong'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
signedShort
	^AtomicTypes at: 'short'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
string
	^(AtomicTypes at: 'char') asPointerType! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/28/1999 23:44'!
ulong
	^self unsignedLong! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:57'!
unsignedByte
	^AtomicTypes at: 'byte'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:58'!
unsignedChar
	^AtomicTypes at: 'char'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:58'!
unsignedLong
	^AtomicTypes at: 'ulong'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:58'!
unsignedLongLong
	^AtomicTypes at: 'ulonglong'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:58'!
unsignedShort
	^AtomicTypes at: 'ushort'! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 11/28/1999 23:44'!
ushort
	^self unsignedShort! !

!ExternalType class methodsFor: 'type constants' stamp: 'ar 12/2/1999 16:58'!
void
	^AtomicTypes at: 'void'! !

!ExternalType class methodsFor: 'housekeeping' stamp: 'ar 12/2/1999 18:00'!
cleanupUnusedTypes
	"ExternalType cleanupUnusedTypes"
	| value |
	StructTypes keys do:[:key|
		value := StructTypes at: key.
		value == nil ifTrue:[StructTypes removeKey: key ifAbsent:[]]].! !

!ExternalType class methodsFor: 'housekeeping' stamp: 'ar 12/2/1999 17:58'!
noticeModificationOf: aClass
	"A subclass of ExternalStructure has been redefined.
	Clean out any obsolete references to its type."
	| type |
	aClass isBehavior ifFalse:[^nil]. "how could this happen?"
	aClass withAllSubclassesDo:[:cls|
		type := StructTypes at: cls name ifAbsent:[nil].
		type == nil ifFalse:[
			type newReferentClass: cls.
			type asPointerType newReferentClass: cls].
	].! !

!ExternalType class methodsFor: 'housekeeping' stamp: 'ar 12/2/1999 17:59'!
noticeRemovalOf: aClass
	"A subclass of ExternalStructure is being removed.
	Clean out any obsolete references to its type."
	| type |
	type := StructTypes at: aClass name ifAbsent:[nil].
	type == nil ifFalse:[
		type newReferentClass: nil.
		type asPointerType newReferentClass: nil].
! !

!ExternalType class methodsFor: 'housekeeping' stamp: 'ar 12/2/1999 16:14'!
noticeRenamingOf: aClass from: oldName to: newName
	"An ExternalStructure has been renamed from oldName to newName.
	Keep our type names in sync."
	| type |
	type := StructTypes at: oldName ifAbsent:[nil].
	type == nil ifFalse:[StructTypes at: newName put: type].
	StructTypes removeKey: oldName ifAbsent:[].! !

!ExternalType class methodsFor: 'class initialization' stamp: 'ar 12/2/1999 16:15'!
initialize
	"ExternalType initialize"
	self initializeFFIConstants.
	self initializeDefaultTypes.! !

!ExternalType class methodsFor: 'class initialization' stamp: 'jmv 12/26/2016 15:02:13'!
initializeAtomicTypes
	"ExternalType initialize"
	| atomicType byteSize type typeName |
	#(
		"name		atomic id		byte size"
		('void' 			0 				0)
		('bool' 			1 				1)
		('byte' 		2 				1)
		('sbyte' 		3 				1)
		('ushort' 		4 				2)
		('short' 		5 				2)
		('ulong' 		6 				4)
		('long' 			7 				4)
		('ulonglong' 	8 				8)
		('longlong' 	9 				8)
		('char' 			10 				1)
		('schar' 		11 				1)
		('float' 		12 				4)
		('double' 		13 				8)
	) do:[:typeSpec| | compiled |
		typeName := typeSpec first.
		atomicType := typeSpec second.
		byteSize := typeSpec third.
		compiled := WordArray with: ((byteSize bitOr: FFIFlagAtomic) bitOr:
				(atomicType bitShift: FFIAtomicTypeShift)).
		type := (AtomicTypes at: typeName).
		type compiledSpec: compiled.
		compiled := WordArray with: ((self pointerSpec bitOr: FFIFlagAtomic) bitOr:
				(atomicType bitShift: FFIAtomicTypeShift)).
		type asPointerType compiledSpec: compiled.
	].! !

!ExternalType class methodsFor: 'class initialization' stamp: 'ar 12/2/1999 17:01'!
initializeDefaultTypes
	"ExternalType initialize"
	| type pointerType |
	AtomicTypes = nil ifTrue:[
		"Create new atomic types and setup the dictionaries"
		AtomicTypes := Dictionary new.
		StructTypes := WeakValueDictionary new.
		AtomicTypeNames valuesDo:[:k|
			type := self basicNew.
			pointerType := self basicNew.
			AtomicTypes at: k put: type.
			type setReferencedType: pointerType.
			pointerType setReferencedType: type.
		].
	].
	self initializeAtomicTypes.
	self initializeStructureTypes.
	"AtomicTypes := nil"! !

!ExternalType class methodsFor: 'class initialization' stamp: 'ar 8/14/2006 23:13'!
initializeFFIConstants
	"ExternalType initialize"
	FFIConstants initialize. "ensure proper initialization"
	AtomicTypeNames := IdentityDictionary new.
	AtomicSelectors := IdentityDictionary new.
	AtomicTypeNames
		at: FFITypeVoid put: 'void';
		at: FFITypeBool put: 'bool';
		at: FFITypeUnsignedByte put: 'byte';
		at: FFITypeSignedByte put: 'sbyte';
		at: FFITypeUnsignedShort put: 'ushort';
		at: FFITypeSignedShort put: 'short';
		at: FFITypeUnsignedInt put: 'ulong';
		at: FFITypeSignedInt put: 'long';
		at: FFITypeUnsignedLongLong put: 'ulonglong';
		at: FFITypeSignedLongLong put: 'longlong';
		at: FFITypeUnsignedChar put: 'char';
		at: FFITypeSignedChar put: 'schar';
		at: FFITypeSingleFloat put: 'float';
		at: FFITypeDoubleFloat put: 'double';
	yourself.

	AtomicSelectors
		at: FFITypeVoid put: #voidAt:;
		at: FFITypeBool put: #booleanAt:;
		at: FFITypeUnsignedByte put: #unsignedByteAt:;
		at: FFITypeSignedByte put: #signedByteAt:;
		at: FFITypeUnsignedShort put: #unsignedShortAt:;
		at: FFITypeSignedShort put: #signedShortAt:;
		at: FFITypeUnsignedInt put: #unsignedLongAt:;
		at: FFITypeSignedInt put: #signedLongAt:;
		at: FFITypeUnsignedLongLong put: #unsignedLongLongAt:;
		at: FFITypeSignedLongLong put: #signedLongLongAt:;
		at: FFITypeUnsignedChar put: #unsignedCharAt:;
		at: FFITypeSignedChar put: #signedCharAt:;
		at: FFITypeSingleFloat put: #floatAt:;
		at: FFITypeDoubleFloat put: #doubleAt:;
	yourself! !

!ExternalType class methodsFor: 'class initialization' stamp: 'ar 3/22/2007 20:14'!
initializeStructureTypes
	"ExternalType initialize"
	| referentClass pointerType |
	self cleanupUnusedTypes.
	StructTypes keysAndValuesDo:[:referentName :type|
		referentClass := (Smalltalk at: referentName ifAbsent:[nil]).
		(referentClass isBehavior and:[
			referentClass includesBehavior: ExternalStructure])
				ifFalse:[referentClass := nil].
		type compiledSpec: 
			(WordArray with: self structureSpec).
		type newReferentClass: referentClass.
		pointerType := type asPointerType.
		pointerType compiledSpec: 
			(WordArray with: self pointerSpec).
		pointerType newReferentClass: referentClass.
	].! !

!ExternalType class methodsFor: 'instance creation' stamp: 'ar 1/26/2000 14:58'!
new
	"Use either the type constants or #externalType for creating external types"
	^self shouldNotImplement! !

!Object methodsFor: '*FFI-Kernel' stamp: 'ar 8/14/2006 23:02'!
externalCallFailed
	"Raise an error after a failed call to an external function"
	| errCode |
	errCode := ExternalFunction getLastError. "this allows us to look at the actual error code"
	^self error: (ExternalFunction errorMessageFor: errCode).! !

!SequenceableCollection methodsFor: '*FFI-Kernel' stamp: 'jmv 12/26/2016 10:45:56'!
asMachineWordArray
	"Answer an IntegerArray or Bytearray with the elements are the elements of the receiver stored as machine words,
	suitable for FFI."

	| answer |
	Smalltalk wordSize = 4 ifTrue: [
		^self as: IntegerArray ].
	answer _ ByteArray new: self size * Smalltalk wordSize.
	1 to: self size do: [ :i |
		answer unsignedLong64At: i-1*Smalltalk wordSize+1 put: (self at: i) bigEndian: Smalltalk isBigEndian ].
	^answer! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'EstebanLorenzano 11/4/2016 14:53'!
asExternalPointer
	"Convert the receiver assuming that it describes a pointer to an object."
	^ self pointerAt: 1! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 8/31/2018 11:49:53'!
booleanAt: byteOffset
	"Booleans are just integers in C word"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^(self integerAt: byteOffset size: 1 signed: false) ~= 0! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:06'!
booleanAt: byteOffset put: value
	"Booleans are just integers in C word"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:09'!
doubleAt: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive:'primitiveFFIDoubleAt' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:13'!
doubleAt: byteOffset put: value
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive:'primitiveFFIDoubleAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:16'!
floatAt: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive:'primitiveFFIFloatAt' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:20'!
floatAt: byteOffset put: value
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive:'primitiveFFIFloatAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:24'!
integerAt: byteOffset put: value size: nBytes signed: aBoolean
	"Primitive. Store the given value as integer of nBytes size
	in the receiver. Fail if the value is out of range.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive: 'primitiveFFIIntegerAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:27'!
integerAt: byteOffset size: nBytes signed: aBoolean
	"Primitive. Return an integer of nBytes size from the receiver.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	<primitive: 'primitiveFFIIntegerAt' module:'SqueakFFIPrims'>
	^self primitiveFailed! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jcg 6/4/2010 02:40'!
isExternalAddress
	"Return true if the receiver describes the address of an object in the outside world"
	^false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'ar 11/16/2006 15:35'!
isNull
	"Answer false since only external addresses can be null"
	^false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:30'!
longPointerAt: byteOffset
	"Answer an 8-byte pointer object stored at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| addr |
	addr := ExternalAddress basicNew: 8.
	1 to: 8 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:32'!
longPointerAt: byteOffset put: value
	"Store an 8-byte pointer object at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 8 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:35'!
pointerAt: byteOffset
	"Answer a pointer object stored at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:38'!
pointerAt: byteOffset put: value
	"Store a pointer object at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:41'!
shortPointerAt: byteOffset
	"Answer a 4-byte pointer object stored at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| addr |
	addr := ExternalAddress basicNew: 4.
	1 to: 4 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:44'!
shortPointerAt: byteOffset put: value
	"Store a 4-byte pointer object at the given byte address"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 4 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:48'!
signedByteAt: byteOffset
	"Return a 8bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 1 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:56'!
signedByteAt: byteOffset put: value
	"Store a 8bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 1 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:10:59'!
signedCharAt: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^(self unsignedByteAt: byteOffset) asCharacter! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:03'!
signedCharAt: byteOffset put: aCharacter
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self unsignedByteAt: byteOffset put: aCharacter numericValue! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:06'!
signedLongAt: byteOffset
	"Return a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 4 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:12'!
signedLongAt: byteOffset put: value
	"Store a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 4 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:17'!
signedLongLongAt: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| int |
	int := self unsignedLongLongAt: byteOffset.
	int > 16r7FFFFFFFFFFFFFFF ifTrue: [^int - 16r10000000000000000].
	^int! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:20'!
signedLongLongAt: byteOffset put: value
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	self unsignedLongLongAt: byteOffset put: (value < 0
		ifTrue: [ value + 16r10000000000000000 ]
		ifFalse: [ value ])! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:23'!
signedShortAt: byteOffset
	"Return a 16bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 2 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:27'!
signedShortAt: byteOffset put: value
	"Store a 16bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 2 signed: true! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:31'!
structAt: byteOffset length: length
	"Return a structure of the given length starting at the indicated byte offset."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	| value |
	value := ByteArray new: length.
	1 to: length do:[:i|
		value unsignedByteAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^value! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:33'!
structAt: byteOffset put: value length: length
	"Store a structure of the given length starting at the indicated byte offset."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	1 to: length do:[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value unsignedByteAt: i)].
	^value! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:36'!
unsignedByteAt: byteOffset
	"Return a 8bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 1 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:39'!
unsignedByteAt: byteOffset put: value
	"Store a 8bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 1 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:42'!
unsignedCharAt: byteOffset
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^(self unsignedByteAt: byteOffset) asCharacter! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:45'!
unsignedCharAt: byteOffset put: aCharacter
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self unsignedByteAt: byteOffset put: aCharacter numericValue! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:47'!
unsignedLongAt: byteOffset
	"Return a 32bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 4 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:50'!
unsignedLongAt: byteOffset put: value
	"Store a 32bit signed integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 4 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:53'!
unsignedLongLongAt: byteOffset
	"Answer a 64-bit integer in Smalltalk order (little-endian)."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 8 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:55'!
unsignedLongLongAt: byteOffset put: value
	"I store 64-bit integers in Smalltalk (little-endian) order."
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 8 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:11:59'!
unsignedShortAt: byteOffset
	"Return a 16bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset size: 2 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'jmv 12/22/2016 17:12:01'!
unsignedShortAt: byteOffset put: value
	"Store a 16bit unsigned integer starting at the given byte offset"
	"If a regular ByteArray, byteOffset is the start position in self.
	If an ExternalAddress, take us as a pointer. byteOffset is applied to the pointer. Access memory at pointer+offset."
	^self integerAt: byteOffset put: value size: 2 signed: false! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'ar 11/29/1999 00:16'!
voidAt: byteOffset
	"no accessors for void"
	^self shouldNotImplement! !

!ByteArray methodsFor: '*FFI-Kernel' stamp: 'ar 11/29/1999 00:16'!
voidAt: byteOffset put: value
	"no accessors for void"
	^self shouldNotImplement! !
ExternalAddress initialize!
FFIConstants initialize!
ExternalObject initialize!
ExternalFunction initialize!
ExternalStructure initialize!
ExternalData initialize!
ExternalType initialize!
