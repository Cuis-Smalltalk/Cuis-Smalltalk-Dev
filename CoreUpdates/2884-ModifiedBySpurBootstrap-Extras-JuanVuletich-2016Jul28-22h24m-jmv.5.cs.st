'From Cuis 4.2 of 25 July 2013 [latest update: #2877] on 11 August 2016 at 2:20:11 pm'!

!ProtoObject methodsFor: 'comparing' stamp: 'eem 9/6/2013 14:31'!
scaledIdentityHash
	"For identityHash values returned by primitive 75, answer
	 such values times 2^8.  Otherwise, match the existing
	 identityHash implementation"

	^self identityHash * 256 "bitShift: 8"! !


!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 15:04'!
isPinned
	"Answer if the receiver is pinned.  The VM's garbage collector routinely moves
	 objects as it reclaims and compacts memory.  But it can also pin an object so
	 that it will not be moved, which can make it easier to pass objects out through
	 the FFI."
	<primitive: 183 error: ec>
	^self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 15:13'!
pin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is pinned, and answers whether it was already pinned."
	^self setPinned: true! !

!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 15:13'!
setPinned: aBoolean
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This primitive either pins or unpins an object, and answers if it was already pinned."
	<primitive: 184 error: ec>
	^self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 15:12'!
unpin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is unpinned, and answers whether it was pinned."
	^self setPinned: false! !


!Behavior methodsFor: 'testing' stamp: 'eem 12/1/2014 11:46'!
isCompiledMethodClass
	"Answer whether the receiver has compiled method instances that mix pointers and bytes."
	^self instSpec >= 24! !

!Behavior methodsFor: 'testing' stamp: 'eem 3/3/2014 17:23'!
isEphemeronClass
	"Answer whether the receiver has ephemeral instance variables.  The garbage collector will
	 fire (queue for finalization) any ephemeron whose first instance variable is not referenced
	 other than from the transitive closure of references from ephemerons. Hence referring to
	 an object from the first inst var of an ephemeron will cause the ephemeron to fire when
	 the rest of the system does not refer to the object and that object is ready to be collected.
	 Since references from the remaining inst vars of an ephemeron will not prevent the ephemeron
	 from firing, ephemerons may act as the associations in weak dictionaries such that the value
	 (e.g. properties attached to the key) will not prevent firing when the key is no longer referenced
	 other than from ephemerons.  Ephemerons can therefore be used to implement instance-based
	 pre-mortem finalization."
	^self instSpec = 5! !

!Behavior methodsFor: 'testing' stamp: 'eem 3/3/2014 17:23'!
isImmediateClass
	"Answer whether the receiver has immediate instances.  Immediate instances
	 store their value in their object pointer, not in an object body.  Hence immediates
	 take no space and are immutable.  The immediates are distinguished by tag bits
	 in the pointer. They include SmallIntegers and Characters.  Hence in the 32-bit
	 system SmallIntegers are 31-bit signed integers and Characters are 30-bit
	 unsigned character codes."
	^self instSpec = 7! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'eem 6/28/2014 17:14'!
byteSizeOfInstance
	"Only for Spur!!"
	"Answer the total memory size of an instance of the receiver."

	<primitive: 181 error: ec>
	self isVariable ifTrue:
		[^self byteSizeOfInstanceOfSize: 0].
	self primitiveFailed! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'eem 2/25/2014 11:25'!
elementSize
	"Answer the size in bytes of an element in the receiver.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	instSpec < 9 ifTrue: [^Smalltalk wordSize].
	instSpec >= 16 ifTrue: [^1].
	instSpec >= 12 ifTrue: [^2].
	instSpec >= 10 ifTrue: [^4].
	^8! !

!Behavior methodsFor: 'enumerating' stamp: 'eem 3/10/2014 19:04'!
allInstancesOrNil
	"Answer all instances of the receiver, or nil if the primitive
	 fails, which it may be due to being out of memory."
	<primitive: 177>
	^nil! !

!Behavior methodsFor: 'private' stamp: 'eem 2/26/2014 07:56'!
handleFailingBasicNew
	"handleFailingBasicNew gets sent after basicNew has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew.  If
	 handleFailingBasicNew fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	Smalltalk garbageCollect < 1048576 ifTrue:
		[Smalltalk growMemoryByAtLeast: 1048576].
	^self handleFailingFailingBasicNew "retry after global garbage collect"! !

!Behavior methodsFor: 'private' stamp: 'eem 3/12/2014 10:32'!
handleFailingBasicNew: sizeRequested
	"handleFailingBasicNew: gets sent after basicNew: has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew:.  If
	 handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingBasicNew: sizeRequested! !

!Behavior methodsFor: 'private' stamp: 'eem 2/26/2014 08:00'!
handleFailingFailingBasicNew
	"This basicNew gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	"space must be low"
	OutOfMemory signal.
	^self basicNew  "retry if user proceeds"! !

!Behavior methodsFor: 'private' stamp: 'eem 2/26/2014 08:01'!
handleFailingFailingBasicNew: sizeRequested
	"This basicNew: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	"space must be low."
	OutOfMemory signal.
	^self basicNew: sizeRequested  "retry if user proceeds"! !

!Behavior methodsFor: 'comparing' stamp: 'eem 2/25/2014 11:32'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  If the VM is using
	 this implementation then classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.  In Spur as of
	 2014 there are 22 bits of classTable index and 22 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 175>
	self primitiveFailed! !


!ClassDescription methodsFor: 'initialize-release' stamp: 'eem 5/27/2014 11:08'!
updateMethodBindingsTo: aBinding
	"ClassBuilder support for maintaining valid method bindings."
	methodDict do: [:method| method methodClassAssociation: aBinding]! !


!Class methodsFor: 'subclass creation' stamp: 'eem 3/3/2014 13:37'!
immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new
	 immediate class as a subclass of an existing class (the receiver)."
	^ClassBuilder new
		superclass: self
		immediateSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat! !


!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/11/2016 11:38:39'!
preSpurFormat: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak
	"Compute the format for the given instance specfication."
	| cClass instSpec sizeHiBits fmt |
"
	NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
	For now the format word is...
		<2 bits=instSize//64><5 bits=cClass><4 bits=instSpec><6 bits=instSize\\64><1 bit=0>
	But when we revise the image format, it should become...
		<5 bits=cClass><4 bits=instSpec><8 bits=instSize><1 bit=0>
"
	sizeHiBits _ (nInstVars+1) // 64.
	cClass _ 0.  "for now"
	instSpec _ isWeak
		ifTrue:[4]
		ifFalse:[isPointers
				ifTrue: [isVar
						ifTrue: [nInstVars>0 ifTrue: [3] ifFalse: [2]]
						ifFalse: [nInstVars>0 ifTrue: [1] ifFalse: [0]]]
				ifFalse: [isWords ifTrue: [6] ifFalse: [8]]].
	fmt _ sizeHiBits.
	fmt _ (fmt bitShift: 5) + cClass.
	fmt _ (fmt bitShift: 4) + instSpec.
	fmt _ (fmt bitShift: 6) + ((nInstVars+1)\\64).  "+1 since prim size field includes header"
	fmt _ (fmt bitShift: 1). "This shift plus integer bit lets wordSize work like byteSize"
	^fmt! !

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/11/2016 11:38:48'!
spurFormat: nInstVars variable: isVar words: is32BitWords pointers: isPointers weak: isWeak
	"Compute the format for the given instance specfication.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= reserved for 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := isWeak
					ifTrue:
						[isVar
							ifTrue: [4]
							ifFalse: [5]]
					ifFalse:
						[isPointers
							ifTrue:
								[isVar
									ifTrue: [nInstVars > 0 ifTrue: [3] ifFalse: [2]]
									ifFalse: [nInstVars > 0 ifTrue: [1] ifFalse: [0]]]
							ifFalse:
								[isVar
									ifTrue: [is32BitWords ifTrue: [10] ifFalse: [16]]
									ifFalse: [7]]].
	^(instSpec bitShift: 16) + nInstVars! !


!BoxedFloat64 class methodsFor: 'instance creation' stamp: 'nice 1/2/2013 07:54'!
basicNew
	^self basicNew: 2! !


!SmallFloat64 methodsFor: 'comparing' stamp: 'eem 12/22/2014 12:36'!
identityHash
	"Answer an integer unique to the receiver."
	<primitive: 171>
	^self primitiveFailed! !

!SmallFloat64 methodsFor: 'copying' stamp: 'eem 11/25/2014 07:54'!
shallowCopy
	"Answer the receiver, because SmallFloat64s are unique."
	^self! !


!Character methodsFor: 'accessing' stamp: 'jmv 8/11/2016 09:43:22'!
immediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be immediate (i.e. as in Spur)"

	<primitive: 171>
	^self primitiveFailed! !

!Character methodsFor: 'accessing' stamp: 'jmv 8/11/2016 09:46:09'!
nonImmediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be regular (i.e. not in Spur)"

	^self instVarAt: 1! !

!Character methodsFor: 'accessing' stamp: 'jmv 8/11/2016 09:47:05'!
numericValue
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding."

	^ Character isImmediateClass
		ifTrue: [ self immediateNumericValue ]
		ifFalse: [ self nonImmediateNumericValue ]! !

!Character methodsFor: 'comparing' stamp: 'eem 10/11/2013 12:55'!
= aCharacter 
	"Primitive. Answer if the receiver and the argument are the
	 same object (have the same object pointer). Optional. See
	 Object documentation whatIsAPrimitive."
	<primitive: 110>
	^self == aCharacter! !

!Character methodsFor: 'comparing' stamp: 'jmv 8/16/2016 12:56'!
identityHash
	"Answer the receiver's character code."

	"For Spur"
	<primitive: 171>

	"For preSpur"
	^super identityHash! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 09:54:19'!
immediateNumericValue: anInteger
	"Answer the Character whose value is anInteger, if instances happen to be immediate (i.e. as in Spur)"

	<primitive: 170>
	^self primitiveFailed! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 09:55:27'!
nonImmediateNumericValue: anInteger
	"Answer the Character whose value is anInteger, if instances happen to be regular (i.e. not in Spur)"

	^CharacterTable at: anInteger + 1! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 09:56:03'!
numericValue: anInteger
	"Answer the Character whose value is anInteger."

	^ Character isImmediateClass
		ifTrue: [ self immediateNumericValue: anInteger ]
		ifFalse: [ self nonImmediateNumericValue: anInteger ]! !


!InstructionClient methodsFor: 'instruction decoding' stamp: 'eem 8/8/2014 07:50'!
callPrimitive: pimIndex
	"V3PlusClosures:	139 10001011	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 NewsqueakV4:		249 11111001	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 SistaV1:			248 11111000 iiiiiiii mjjjjjjj  Call Primitive #iiiiiiii + ( jjjjjjj * 256)
							m=1 means inlined primitive, no hard return after execution."! !


!ContextPart methodsFor: 'private' stamp: 'eem 8/11/2014 12:57'!
isPrimFailToken: anObject
	^(self objectClass: anObject) == Array
	  and: [anObject size = 2
	  and: [anObject first == PrimitiveFailToken]]! !


!MethodContext methodsFor: 'system simulation' stamp: 'eem 8/11/2014 13:18'!
failPrimitiveWith: maybePrimFailToken
	"The receiver is a freshly-created context on a primitive method.  Skip the callPrimitive:
	 bytecode and store the primitive fail code if there is one and the method consumes it."
	self skipCallPrimitive.
	((self isPrimFailToken: maybePrimFailToken)
	  and: [method encoderClass isStoreAt: pc in: method]) ifTrue:
		[self at: stackp put: maybePrimFailToken last]! !


!MethodContext class methodsFor: 'enumerating' stamp: 'eem 7/24/2014 10:39'!
allInstances
	"Answer all instances of the receiver."
	<primitive: 177>
	"The primitive can fail because memory is low.  If so, fall back on the old
	 enumeration code, which gives the system a chance to GC and/or grow.
	 Because aBlock might change the class of inst (for example, using become:),
	 it is essential to compute next before aBlock value: inst.
	 Only count until thisContext since this context has been created only to
	 compute the existing instances."
	| inst insts next |
	insts := WriteStream on: (Array new: 64).
	inst := self someInstance.
	[inst == thisContext or: [inst == nil]] whileFalse:
		[next := inst nextInstance.
		 insts nextPut: inst.
		 inst := next].
	^insts contents! !


!CompiledMethod methodsFor: 'accessing' stamp: 'eem 9/11/2014 12:04'!
bytecodeSetName
	^self encoderClass name copyReplaceAll: 'EncoderFor' with: ''! !


!CompiledMethod class methodsFor: 'private' stamp: 'eem 8/13/2014 05:52'!
handleFailingFailingNewMethod: numberOfBytes header: headerWord
	"This newMethod:header: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails then the
	 system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, headerWord, and the number of bytecodes specified
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough
	 memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79>
	"space must be low."
	OutOfMemory signal.
	"retry if user proceeds"
	^self newMethod: numberOfBytes header: headerWord! !

!CompiledMethod class methodsFor: 'private' stamp: 'eem 8/13/2014 05:51'!
handleFailingNewMethod: numberOfBytes header: headerWord
	"This newMethod:header: gets sent after newMethod:header: has failed
	 and allowed a scavenging garbage collection to occur.  The scavenging
	 collection will have happened as the VM is activating the (failing) basicNew:.
	 If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, headerWord, and the number of bytecodes specified
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough
	 memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79>
	| bytesRequested |
	bytesRequested := (headerWord bitAnd: 16rFFFF) + 1 * Smalltalk wordSize + numberOfBytes + 16.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingNewMethod: numberOfBytes header: headerWord! !


!SystemDictionary methodsFor: 'memory space' stamp: 'eem 6/23/2016 16:16'!
growMemoryByAtLeast: numBytes
	"Only for Spur!!"
	"Grow memory by at least the requested number of bytes.
	 Primitive.  Fail if no memory is available.  Essential.
	 N.B. In Spur, the heap is composed of segments. Growing memory adds a new
	 segment; it does not extend existing segments. Growth is by at least that
	 established by SmalltalkImage current vmParameterAt: 25, which defaults to 16Mb."
	<primitive: 180>
	^(numBytes isInteger and: [numBytes > 0])
		ifTrue: [OutOfMemory signal]
		ifFalse: [self primitiveFailed]! !

!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 8/11/2016 10:42:07'!
compactClassesArrayIncludes: aClass
	"No compact classes in Spur, but usually some in non-Spur"
	^self compactClassesArray
		ifNil: [ false ]
		ifNotNil: [ :it | it identityIncludes: aClass ]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 8/11/2016 10:59:24'!
isSpur
	"Answer true if we are a Spur ObjectMemory.
	Spur introduces a new format of header for objects, new format for classes, etc."
	^ self compactClassesArray isNil! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 9/5/2013 17:10'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM."
	<primitive: 176>
	^self primitiveFailed! !


!BytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 8/11/2016 14:05:02'!
computePreSpurMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	| primBits |
	numTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 255 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	primBits := primitiveIndex <= 16r1FF
					ifTrue: [primitiveIndex]
					ifFalse: "For now the high bit of primitive no. is in the 29th bit of header"
						[primitiveIndex > 16r3FF ifTrue: [self error: 'prim num too large'].
						 (primitiveIndex bitAnd: 16r1FF) + ((primitiveIndex bitAnd: 16r200) bitShift: 19)].
	^(numArgs bitShift: 24)
	+ (numTemps bitShift: 18)
	"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
	+ (numLits bitShift: 9)
	+ primBits! !

!BytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 8/11/2016 14:04:12'!
computeSpurMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	numArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	numTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	^(CompiledMethod headerFlagForEncoder: self)
	+ (numArgs bitShift: 24)
	+ (numTemps bitShift: 18)
	"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
	+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])
	+ numLits! !


!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 8/8/2014 09:14'!
callPrimitiveCode
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	^139! !


!SpaceTally methodsFor: 'instance size' stamp: 'jmv 8/11/2016 14:15:07'!
spaceForInstancesOfPreSpur: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instCount isCompact instVarBytes bytesPerElement contentBytes headerBytes total |
	instCount _ aClass instanceCount.
	instCount = 0 ifTrue: [^ 0].
	isCompact _ aClass indexIfCompact > 0.
	instVarBytes _ aClass instSize * 4.
	aClass isVariable
		ifTrue: [
			bytesPerElement _ aClass isBytes ifTrue: [1] ifFalse: [4].
			total _ 0.
			aClass allInstancesDo: [:inst |
				contentBytes _ instVarBytes + (inst size * bytesPerElement).
				headerBytes _
					contentBytes > 255
						ifTrue: [12]
						ifFalse: [isCompact ifTrue: [4] ifFalse: [8]].
				total _ total + headerBytes + contentBytes].
			^ {total. instCount}]
		ifFalse: [
			headerBytes _
				instVarBytes > 255
					ifTrue: [12]
					ifFalse: [isCompact ifTrue: [4] ifFalse: [8]].
			^ {instCount * (headerBytes + instVarBytes). instCount}].
! !

!SpaceTally methodsFor: 'instance size' stamp: 'jmv 8/11/2016 14:12:51'!
spaceForInstancesOfSpur: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instances total |
	instances := aClass allInstances.
	instances isEmpty ifTrue: [^#(0 0)].
	total := 0.
	aClass isVariable
		ifTrue:
			[instances do:
				[:i| total := total + (aClass byteSizeOfInstanceOfSize: i basicSize)]]
		ifFalse:
			[total := instances size * aClass byteSizeOfInstance].
	^{ total. instances size }! !


!SystemNavigation methodsFor: 'query' stamp: 'eem 3/10/2014 19:05'!
allObjects
	"Answer an Array of all objects in the system.  Fail if
	 there isn't enough memory to instantiate the result."
	<primitive: 178>
	^self primitiveFailed! !

!SystemNavigation methodsFor: 'query' stamp: 'eem 3/11/2014 13:59'!
allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger and Character."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:
			["Fall back on the old single object primitive code.  With closures, this needs
			  to use an end marker (lastObject) since activation of the block will create
			  new contexts and cause an infinite loop.  The lastObject must be created
			  before calling someObject, so that the VM can settle the enumeration (e.g.
			  by flushing new space) as a side effect of  someObject"
			| object lastObject |
			lastObject := Object new.
			object := self someObject.
			[lastObject == object or: [0 == object]] whileFalse:
				[aBlock value: object.
				 object := object nextObject]]! !

!SystemNavigation methodsFor: 'query' stamp: 'eem 3/11/2014 13:56'!
allObjectsOrNil
	"Answer an Array of all objects in the system.  Fail if there isn't
	 enough memory to instantiate the result and answer nil."
	<primitive: 178>
	^nil! !


!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 18:01'!
instVarAt: index
	"Primitive. Answer a fixed variable in an object. The numbering of the variables
	 corresponds to the named instance variables, followed by the indexed instance
	 variables. Fail if the index is not an Integer or is not the index of a fixed variable.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'eem 12/25/2014 18:01'!
instVarAt: index put: anObject
	"Primitive. Store a value into a fixed variable in an object. The numbering of the
	 variables corresponds to the named instance variables, followed by the indexed
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed
	 variable. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	self primitiveFailed! !


!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 11:33:18'!
instSize
	"Answer the number of named instance variables
	(as opposed to indexed variables) of the receiver."

	^ Smalltalk isSpur

		ifTrue: [
	 		"Above Cog Spur the class format is
				<5 bits inst spec><16 bits inst size>"
			format bitAnd: 16rFFFF ]

		ifFalse: [
			"NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
			When we revise the image format, it should become...
			^ ((format bitShift: -1) bitAnd: 16rFF) - 1
			Note also that every other method in this category will require
			2 bits more of right shift after the change."
			((format bitShift: -10) bitAnd: 16rC0) + ((format bitShift: -1) bitAnd: 16r3F) - 1 ]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 11:35:30'!
instSpec
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is."

	^ Smalltalk isSpur

		ifTrue: [
			"The formats are
					0	= 0 sized objects (UndefinedObject True False et al)
					1	= non-indexable objects with inst vars (Point et al)
					2	= indexable objects with no inst vars (Array et al)
					3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
					4	= weak indexable objects with inst vars (WeakArray et al)
					5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
					6	= unused
					7	= immediates (SmallInteger, Character)
					8	= unused
					9	= 64-bit indexable
				10-11	= 32-bit indexable (Bitmap)					(plus one odd bit, unused in 32-bits)
				12-15	= 16-bit indexable							(plus two odd bits, one unused in 32-bits)
				16-23	= 8-bit indexable							(plus three odd bits, one unused in 32-bits)
				24-31	= compiled methods (CompiledMethod)	(plus three odd bits, one unused in 32-bits)
			 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
			 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
			 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
			 has format 18 in 32-bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
			 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6)."
			(format bitShift: -16) bitAnd: 16r1F ]

		ifFalse: [
			(format bitShift: -7) bitAnd: 16rF ]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 11:29:03'!
isBits
	"Answer whether the receiver contains just bits (not pointers).
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec >= 7 ]
		ifFalse: [ self instSpec >= 6 ]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 11:20:40'!
isBytes
	"Answer whether the receiver contains just bits (not pointers).
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec >= 16 ]
		ifFalse: [ self instSpec >= 8 ]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 10:41:22'!
shouldNotBeRedefined
	"Return true if the receiver should not be redefined.
	The assumption is that compact classes,
	classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"
	^(Smalltalk compactClassesArrayIncludes: self)
		or:[(Smalltalk specialObjectsArray identityIncludes: self)
			or:[self isKindOf: self]]! !

!Behavior methodsFor: 'instance creation' stamp: 'eem 8/13/2014 06:32'!
basicNew
	"Primitive. Answer an instance of the receiver (which is a class) with no 
	 indexable variables. Fail if the class is indexable. Essential. See Object 
	 documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run
	 before the method is activated.  Check that space was low and retry
	 via handleFailingBasicNew if so."

	<primitive: 70 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingBasicNew].
	self isVariable ifTrue: [^self basicNew: 0].
	self primitiveFailed! !


!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/11/2016 11:39:37'!
format: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak
	"Compute the format for the given instance specfication."

	^ Smalltalk isSpur
		ifTrue: [ self spurFormat: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak ]
		ifFalse: [ self preSpurFormat: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak ]! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 8/11/2016 11:58:36'!
update: oldClass to: newClass
	"Convert oldClass, all its instances and possibly its meta class into newClass,
	 instances of newClass and possibly its meta class. The process is surprisingly
	 simple in its implementation and surprisingly complex in its nuances and potentially
	 bad side effects.
	 We can rely on two assumptions (which are critical):
		#1: The method #updateInstancesFrom: will not create any lasting pointers to
			 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do
			 a become of the old vs. the new instances and therefore it will not create
			 pointers to *new* instances before the #become: which are *old* afterwards)
		#2: The non-preemptive execution of the critical piece of code guarantees that
			 nobody can get a hold by 'other means' (such as process interruption and
			 reflection) on the old instances.
	 Given the above two, we know that after #updateInstancesFrom: there are no pointers
	 to any old instances. After the forwarding become there will be no pointers to the old
	 class or meta class either.
	<preSpur> Meaning that if we throw in a nice fat GC at the end of the critical block, everything will 
	be gone (but see the comment right there). </preSpur> 
	 Andreas Raab, 2/27/2003 23:42"
	| meta |
	meta := oldClass isMeta.
	"Note: Everything from here on will run without the ability to get interrupted
	to prevent any other process to create new instances of the old class."
	["Note: The following removal may look somewhat obscure and needs an explanation.
	  When we mutate the class hierarchy we create new classes for any existing subclass.
	  So it may look as if we don't have to remove the old class from its superclass. However,
	  at the top of the hierarchy (the first class we reshape) that superclass itself is not newly
	  created so therefore it will hold both the oldClass and newClass in its (obsolete or not)
	  subclasses. Since the #become: below will transparently replace the pointers to oldClass
	  with newClass the superclass would have newClass in its subclasses TWICE. With rather
	  unclear effects if we consider that we may convert the meta-class hierarchy itself (which
	  is derived from the non-meta class hierarchy).
	  Due to this problem ALL classes are removed from their superclass just prior to converting
	  them. Here, breaking the superclass/subclass invariant really doesn't matter since we will
	  effectively remove the oldClass 
	(<spur>becomeForward:</spur> or <preSpur>become+GC</preSpur>) just a few lines below."

		oldClass superclass removeSubclass: oldClass.
		oldClass superclass removeObsoleteSubclass: oldClass.

		"make sure that the VM cache is clean"
		oldClass methodDict do: [:cm | cm flushCache].
		
		"Convert the instances of oldClass into instances of newClass"
		newClass updateInstancesFrom: oldClass.

		meta
			ifTrue:
				[oldClass becomeForward: newClass.
				 oldClass updateMethodBindingsTo: oldClass binding]
			ifFalse:
				[{oldClass. oldClass class} elementsForwardIdentityTo: {newClass. newClass class}.
				 oldClass updateMethodBindingsTo: oldClass binding.
				 oldClass class updateMethodBindingsTo: oldClass class binding].

 Smalltalk isSpur
	ifTrue: [
		"eem 5/31/2014 07:22 At this point there used to be a garbage collect whose purpose was
		 to ensure no old instances existed after the becomeForward:.  Without the GC it was possible
		 to resurrect old instances using e.g. allInstancesDo:.  This was because the becomeForward:
		 updated references from the old objects to new objects but didn't destroy the old objects.
		 But as of late 2013/early 2014 becomeForward: has been modified to free all the old objects."
		]

	ifFalse: [
		"jmv: Squeak 4.6 (pre Spur) includes the GC. So, do it if not Spur.
		Original note by Andreas Raab below."
		Smalltalk garbageCollect.
		"Warning: Read this before you even think about removing the GC. Yes, it slows us down. Quite heavily if you have a large image. However, there's no good and simple alternative here, since unfortunately, #become: does change class pointers. What happens is that after the above become all of the instances of the old class will have a class pointer identifying them as instances of newClass. If we get our hands on any of these instances we will break immediately since their expected instance layout (that of its class, e.g., newClass) will not match their actual instance layout (that of oldClass). And getting your hands on any of those instances is really simple - just reshaping one class two times in rapid succession will do it. Reflection techniques, interrupts, etc. will only add to this problem. In the case of Metaclass things get even worse since when we recompile the entire class hierarchy we will recompile both, Metaclass and its instances (and some of its instances will have the old and some the new layout).
		The only easy solution to this problem would be to 'fix up' the class pointers of the old instances to point to the old class (using primitiveChangeClassTo:). But this won't work either - as we do a one-way become we would have to search the entire object memory for the oldClass and couldn't even clearly identify it unless we give it some 'special token' which sounds quite error-prone. If you really need to get rid of the GC here are some alternatives:
		On the image level, one could create a copy of the oldClass before becoming it into the new class and, after becoming it, 'fix up' the old instances. That would certainly work but it sounds quite complex, as we need to make sure we're not breaking any of the superclass/subclass meta/non-meta class variants.
		Alternatively, fix up #becomeForward on the VM-level to 'dump the source objects' of #become. This would be quite doable (just 'convert' them into a well known special class such as bitmap) yet it has problems if (accidentally or not) one of the objects in #become: appears on 'both sides of the fence' (right now, this will work ... in a way ... even though the consequences are unclear).
		Another alternative is to provide a dedicated primitive for this (instead of using it implicitly in become) which would allow us to dump all the existing instances right here. This is equivalent to a more general primitiveChangeClassTo: and might be worthwhile but it would likely have to keep in mind the differences between bits and pointer thingies etc.
		Since all of the alternatives seem rather complex and magical compared to a straight-forward GC it seems best to stick with the GC solution for now. If someone has a real need to fix this problem, that person will likely be motivated enough to check out the alternatives. Personally I'd probably go for #1 (copy the old class and remap the instances to it) since it's a solution that could be easily reverted from within the image if there's any problem with it.
		 Andreas Raab, 2/27/2003 23:42"
		]
	] valueUnpreemptively! !


!Float class methodsFor: 'instance creation' stamp: 'eem 11/25/2014 07:54'!
basicNew
	^BoxedFloat64 basicNew: 2! !

!Float class methodsFor: 'instance creation' stamp: 'eem 11/25/2014 07:54'!
basicNew: anInteger
	^BoxedFloat64 basicNew: 2! !

!Float class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 12:50:07'!
fromIEEE32Bit: word
  	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from
	a 32bit IEEE floating point representation into an actual Squeak float object (being
	64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
  	
 	| sign exponent mantissa exponentBits fractionBits answerFractionBits delta signBit answerExponent |
  	word negative ifTrue: [ ^ self error: 'Cannot deal with negative numbers' ].
  	word = 0 ifTrue: [ ^ Float zero ].
 	word = 16r80000000 ifTrue: [ ^Float negativeZero ].
  	
	signBit _ word bitAnd: 16r80000000.
	sign _ (word bitShift: -31) = 0 ifTrue: [1] ifFalse: [-1].
  	exponentBits _ (word bitShift: -23) bitAnd: 16rFF.
  	fractionBits _ word bitAnd: 16r7FFFFF.
  
	" Special cases: infinites and NaN"
	exponentBits = 16rFF ifTrue: [
  		fractionBits = 0 ifFalse: [ ^ Float nan ].
  		^ sign positive
  			ifTrue: [ Float infinity ]
  			ifFalse: [ Float negativeInfinity ]].

	" Unbias exponent: 16r3FF is bias"
	exponent _ exponentBits - 16r7F.

"Older version."
false ifTrue: [
	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"
 	exponentBits = 0
 		ifTrue: [
 			"gradual underflow (denormalized number)
 			There is no implied one, but the exponent is -126"
			mantissa _ fractionBits.
			answerExponent _ exponent + 1 ]
 		ifFalse: [
			mantissa _ fractionBits + 16r800000.
			answerExponent _ exponent ].
		^ (sign * mantissa) asFloat timesTwoPower: answerExponent - 23 ].

	"Newer version"
 	exponentBits = 0
		ifTrue: [
			"gradual underflow (denormalized number)
			 Remove first bit of mantissa and adjust exponent"
			delta := fractionBits highBit.
			answerFractionBits := (fractionBits bitAnd: (1 bitShift: delta - 1) - 1) bitShift: 24 - delta.
			answerExponent := exponent + delta - 23]
		ifFalse: [
			answerFractionBits _ fractionBits.
			answerExponent _ exponent ].
  
  	"Create new float"
	^ (self basicNew: 2)
		basicAt: 1 put: ((signBit bitOr: (1023 + answerExponent bitShift: 20)) bitOr: (answerFractionBits bitShift: -3));
		basicAt: 2 put: ((answerFractionBits bitAnd: 7) bitShift: 29);
		* 1.0. "reduce to SmallFloat64 if possible"

"
Float fromIEEE32Bit: Float pi asIEEE32BitWord 
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) = Float pi
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) - Float pi

Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  = (Float pi / 1e40)
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  - (Float pi / 1e40)
"! !


!BoxedFloat64 class methodsFor: 'instance creation' stamp: 'eem 11/25/2014 07:54'!
basicNew: sizeRequested 
	"Primitive. Answer an instance of this class with the number
	 of indexable variables specified by the argument, sizeRequested.
	 Fail if this class is not indexable or if the argument is not a
	 positive Integer, or if there is not enough memory available. 
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	sizeRequested isInteger ifTrue:
		[^sizeRequested = 2
			ifTrue: "arg okay; space must be low."
				[OutOfMemory signal.
				 self basicNew: sizeRequested]  "retry if user proceeds"
			ifFalse:
				[self error: 'a Float shall always have two slots']].
	self primitiveFailed! !


!SmallFloat64 class methodsFor: 'instance creation' stamp: 'eem 11/25/2014 07:54'!
basicNew
	self error: 'SmallFloat64s can only be created by performing arithmetic'! !

!SmallFloat64 class methodsFor: 'instance creation' stamp: 'eem 11/25/2014 07:54'!
basicNew: anInteger
	^self basicNew! !


!Integer methodsFor: 'converting' stamp: 'jmv 8/11/2016 10:04:41'!
asCharacter
	"Answer the Character whose value is the receiver."

	^Character numericValue: self! !


!Integer class methodsFor: 'class initialization' stamp: 'eem 7/16/2014 15:27'!
initialize
	"Integer initialize"
"Not in Cuis right now..."
"	self initializeLowBitPerByteTable"! !


!SmallInteger methodsFor: 'system primitives' stamp: 'eem 1/26/2015 14:50'!
digitLength
	"Answer the number of indexable fields in the receiver. This value is the 
	 same as the largest legal subscript. Included so that a SmallInteger can 
	 behave like a LargePositiveInteger or LargeNegativeInteger."

	| value length |
	length := 1.
	value := self.
	value >= 0
		ifTrue:
			[[value > 255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]]
		ifFalse:
			[[value < -255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]].
	^length! !


!Character methodsFor: 'accessing' stamp: 'jmv 8/11/2016 09:49:58'!
asciiValue
	"Answer the value of the receiver that represents its ISO 8859-15 encoding.
	This is the same as ASCII for the first 128 characters."

	^self numericValue! !

!Character methodsFor: 'accessing' stamp: 'jmv 7/29/2016 14:59:11'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36."

	self numericValue <= $9 numericValue 
		ifTrue: [
			^self numericValue - $0 numericValue].
	self numericValue >= $A numericValue 
		ifTrue: [
			self numericValue <= $Z numericValue ifTrue: [
				^self numericValue - $A numericValue + 10]].
	^ -1! !

!Character methodsFor: 'accessing' stamp: 'jmv 7/29/2016 14:56:12'!
unicodeCodePoint
	"
	self assert: $A unicodeCodePoint hex = '16r41'.
	self assert: $¤ unicodeCodePoint hex = '16r20AC'.
	"
	^UnicodeCodePoints at: self numericValue + 1! !

!Character methodsFor: 'accessing' stamp: 'jmv 7/29/2016 15:01:04'!
value
self flag: #borrar.
^self numericValue! !

!Character methodsFor: 'comparing' stamp: 'jmv 8/11/2016 09:59:51'!
hash
	"Hash is reimplemented because = is implemented."

	^self numericValue! !

!Character methodsFor: 'testing' stamp: 'jmv 8/11/2016 10:02:54'!
isCharacter

	^ true! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:06:16'!
isDigit
	"Answer whether the receiver is a digit."

	^self numericValue >= 48 and: [self numericValue <= 57]! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:03:54'!
isLetter
	"Answer whether the receiver is a letter."

	^ (ClassificationTable at: self numericValue + 1)
		anyMask: LetterBits! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:09:01'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	cr, line feed, or form feed."

	self numericValue = 10 ifTrue: [^true].	"line feed"
	self numericValue = 13 ifTrue: [^true].	"cr"
	self numericValue = 12 ifTrue: [^true].	"form feed"
	^false! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:05:23'!
isLowercase
	"Answer whether the receiver is a lowercase letter."

	^ ((ClassificationTable at: self numericValue + 1)
		bitAnd: LowercaseBit)
			= LowercaseBit! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:04:50'!
isSeparator
	"Answer whether the receiver is one of the separator characters--space, 
	cr, tab, line feed, or form feed."

	self numericValue = 32 ifTrue: [^true].	"space"
	self numericValue = 13 ifTrue: [^true].	"cr"
	self numericValue = 9 ifTrue: [^true].		"tab"
	self numericValue = 10 ifTrue: [^true].	"line feed"
	self numericValue = 12 ifTrue: [^true].	"form feed"
	^false! !

!Character methodsFor: 'testing' stamp: 'jmv 7/29/2016 15:08:36'!
isUppercase
	"Answer whether the receiver is an uppercase letter."

	^ ((ClassificationTable at: self numericValue + 1)
		bitAnd: UppercaseBit)
			= UppercaseBit! !

!Character methodsFor: 'copying' stamp: 'eem 2/5/2014 12:53'!
shallowCopy
	"Answer the receiver, because Characters are unique."
	^self! !

!Character methodsFor: 'printing' stamp: 'jmv 7/29/2016 15:12:10'!
hex
	^ String
		with: ('0123456789ABCDEF' at: self numericValue//16+1)
		with: ('0123456789ABCDEF' at: self numericValue\\16+1)! !

!Character methodsFor: 'converting' stamp: 'jmv 8/11/2016 09:51:22'!
asInteger
	"Answer the value of the receiver."

	^self numericValue! !

!Character methodsFor: 'converting' stamp: 'jmv 7/29/2016 15:02:29'!
asLowercase

	^ Character numericValue: ((ClassificationTable at: self numericValue + 1) bitAnd: 255)! !

!Character methodsFor: 'converting' stamp: 'jmv 7/29/2016 15:09:39'!
asUnaccented
	"
	$A asUnaccented
	$Á asUnaccented
	(0 to: 255) collect: [ :i | (Character value: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character value: i) asUnaccented  asLowercase]
	"
	^ Character
		value: (UnaccentedTable at: self numericValue + 1)! !

!Character methodsFor: 'converting' stamp: 'jmv 7/29/2016 15:08:14'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	
	^ Character
		value: (((ClassificationTable at: self numericValue + 1)
				bitShift: -8)
				bitAnd: 255)! !

!Character methodsFor: 'object fileIn' stamp: 'jmv 8/11/2016 09:59:36'!
comeFullyUpOnReload: smartRefStream
	"Use existing an Character.  Don't use the new copy."

	^ self class numericValue: self numericValue! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 10:02:24'!
digitValue: x 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self numericValue: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 7/29/2016 15:01:43'!
value: anInteger
	"Answer the Character whose value is anInteger."
self flag: #borrar.
	^ self numericValue: anInteger! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 4/10/2015 11:06'!
callPrimitive: index
	"Print the callPrimitive bytecode."

	self print: 'callPrimitive: ' , index printString! !


!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 8/8/2014 07:43'!
interpretV3ClosuresExtension: offset in: method for: client
	| type offset2 byte2 byte3 byte4 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	byte2 := method at: pc. pc := pc + 1.
	offset = 10 ifTrue:
		[^byte2 < 128
			ifTrue: [client pushNewArrayOfSize: byte2]
			ifFalse: [client pushConsArrayWithElements: byte2 - 128]].
	byte3 := method at: pc.  pc := pc + 1.
	offset = 11 ifTrue: [^client callPrimitive: byte2 + (byte3 bitShift: 8)].
	offset = 12 ifTrue: [^client pushRemoteTemp: byte2 inVectorAt: byte3].
	offset = 13 ifTrue: [^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	offset = 14 ifTrue: [^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"offset = 15"
	byte4 := method at: pc.  pc := pc + 1.
	^client
		pushClosureCopyNumCopiedValues: (byte2 bitShift: -4)
		numArgs: (byte2 bitAnd: 16rF)
		blockSize: (byte3 * 256) + byte4! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 8/11/2016 13:59:40'!
numLiterals
	"Answer the number of literals used by the receiver."

	^ Smalltalk isSpur
		ifTrue: [ self header bitAnd: 16r7FFF ]
		ifFalse: [ (self header bitShift: -9) bitAnd: 16rFF ]! !

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 8/11/2016 14:01:33'!
primitive
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method."
	| initialPC primBits |
	Smalltalk isSpur
		ifTrue: [
			^(self header anyMask: 65536) "Is the hasPrimitive? flag set?"
				ifTrue: [(self at: (initialPC := self initialPC) + 1) + ((self at: initialPC + 2) bitShift: 8)]
				ifFalse: [0]]
		ifFalse: [
			"We currently allow 10 bits of primitive index, but they are in two places
			for  backward compatibility.  The time to unpack is negligible,
			since the reconstituted full index is stored in the method cache."
			primBits _ self header bitAnd: 16r100001FF.
			^ (primBits bitAnd: 16r1FF) + (primBits bitShift: -19)]! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 5/12/2014 17:50'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass ifNil:
		[PrimaryBytecodeSetEncoderClass := EncoderForV3PlusClosures].
	SecondaryBytecodeSetEncoderClass ifNil:
		[SecondaryBytecodeSetEncoderClass := EncoderForV3PlusClosures]! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'eem 8/13/2014 05:55'!
newMethod: numberOfBytes header: headerWord
	"Primitive. Answer an instance of me. The number of literals (and other 
	 information) is specified by the headerWord (see my class comment).
	 The first argument specifies the number of fields for bytecodes in the
	 method. Fail if either argument is not a SmallInteger, or if numberOfBytes
	 is negative, or if memory is low. Once the header of a method is set by
	 this primitive, it cannot be changed to change the number of literals.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingNewMethod: numberOfBytes header: headerWord].
	^self primitiveFailed! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'eem 8/10/2014 12:41'!
toReturnConstant: index trailerBytes: trailer
	"Answer an instance of me that is a quick return of the constant
	indexed in (true false nil -1 0 1 2)."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256 + index! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'eem 8/10/2014 12:41'!
toReturnField: field trailerBytes: trailer
	"Answer an instance of me that is a quick return of the instance variable 
	indexed by the argument, field."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 264 + field! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'eem 8/10/2014 12:41'!
toReturnSelfTrailerBytes: trailer
	"Answer an instance of me that is a quick return of the instance (^self)."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256! !

!CompiledMethod class methodsFor: 'method encoding' stamp: 'eem 8/12/2014 12:39'!
headerFlagForEncoder: anEncoder
	anEncoder class == PrimaryBytecodeSetEncoderClass ifTrue:
		[^0].
	anEncoder class == SecondaryBytecodeSetEncoderClass ifTrue:
		[^SmallInteger minVal].
	self error: 'The encoder is not one of the two installed bytecode sets'! !


!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 10/23/2012 16:50'!
sizeCallPrimitive: primitiveIndex
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !

!BytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 8/11/2016 14:06:48'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	"Compute the compiled method header that encodes the arguments
	 in the receiver's header format (see CompiledMehtod's class comment)."
	^ Smalltalk isSpur ifTrue: [
		self
			computeSpurMethodHeaderForNumArgs: numArgs
			numTemps: numTemps
			numLits: numLits
			primitive: primitiveIndex ]
	ifFalse: [
		self
			computePreSpurMethodHeaderForNumArgs: numArgs
			numTemps: numTemps
			numLits: numLits
			primitive: primitiveIndex ]! !


!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 8/8/2014 08:05'!
bytecodeSize: bytecode
	"Answer the number of bytes in the bytecode."
	bytecode <= 125 ifTrue:
		[^1].
	bytecode >= 176 ifTrue:
		[^1].
	bytecode >= 160 ifTrue: "long jumps"
		[^2].
	bytecode >= 144 ifTrue: "short jumps"
		[^1].
	"extensions"
	bytecode >= 128 ifTrue:
		[^#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127].
	^nil! !


!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:00'!
generate: trailer 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod! !

!MethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 14:59'!
generate: trailer using: aCompiledMethodClass ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]! !


!SpaceTally methodsFor: 'instance size' stamp: 'jmv 8/11/2016 14:15:02'!
spaceForInstancesOf: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	^ Smalltalk isSpur
		ifTrue: [ self spaceForInstancesOfSpur: aClass ]
		ifFalse: [ self spaceForInstancesOfPreSpur: aClass ]! !

!methodRemoval: LeafNode #veryDeepFixupWith:!
LeafNode removeSelector: #veryDeepFixupWith:!
!methodRemoval: LeafNode #veryDeepInner:!
LeafNode removeSelector: #veryDeepInner:!
!methodRemoval: EncoderForV3 #computeMethodHeaderForNumArgs:numTemps:numLits:primitive:!
EncoderForV3 removeSelector: #computeMethodHeaderForNumArgs:numTemps:numLits:primitive:!
!methodRemoval: BytecodeEncoder #computeNonSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
BytecodeEncoder removeSelector: #computeNonSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
!methodRemoval: Character #clone!
Character removeSelector: #clone!
!methodRemoval: Character #copy!
Character removeSelector: #copy!
!methodRemoval: Character #deepCopy!
Character removeSelector: #deepCopy!
!methodRemoval: Character #veryDeepCopyWith:!
Character removeSelector: #veryDeepCopyWith:!
!methodRemoval: SmallInteger #asCharacter!
SmallInteger removeSelector: #asCharacter!
!methodRemoval: SmallInteger #clone!
SmallInteger removeSelector: #clone!
!methodRemoval: SmallFloat64 #clone!
SmallFloat64 removeSelector: #clone!
!methodRemoval: SmallFloat64 #copy!
SmallFloat64 removeSelector: #copy!
!methodRemoval: SmallFloat64 #deepCopy!
SmallFloat64 removeSelector: #deepCopy!
!methodRemoval: SmallFloat64 #veryDeepCopyWith:!
SmallFloat64 removeSelector: #veryDeepCopyWith:!
!methodRemoval: Object #clone!
Object removeSelector: #clone!
Integer initialize!
CompiledMethod initialize!
