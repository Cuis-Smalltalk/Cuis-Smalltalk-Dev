"
My instances are methods suitable for interpretation by the virtual machine.  This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields. The pointer fields are used for literals and metadata, and the bytes are used for bytecodes and a variety of encoded informaton such as source code, source code position, etc.  The first part of a CompiledMethod object is pointers, the second part is bytes.  CompiledMethod inherits from ByteArray to avoid duplicating some of ByteArray's methods, not because a CompiledMethod is a ByteArray.

Instance variables: *indexed* (no named inst vars)

Class variables:
SmallFrame							- the number of stack slots in a small frame Context
LargeFrame							- the number of stack slots in a large frame Context
PreferredBytecodeSetEncoderClass	- the encoder class that is used to create new methods (either EncoderForV3PlusClosures or EncoderForSistaV1)
	
TThe current format of a CompiledMethod is as follows:

	header (4 or 8 bytes, SmallInteger)
	literals (4 or 8 bytes each, Object, see "The last literal..." below)
	bytecodes  (variable, bytes)
	trailer (variable, bytes)

The header is a SmallInteger (which in the 32-bit system has 31 bits, and in the 64-bit system, 61 bits) in the following format:

	(index 0)		15 bits:	number of literals (#numLiterals)
	(index 15)		  1 bit:	jit without counters - reserved for methods that have been optimized by Sista
	(index 16)		  1 bit:	has primitive
	(index 17)		  1 bit:	whether a large frame size is needed (#frameSize => either SmallFrame or LargeFrame)
	(index 18)		  6 bits:	number of temporary variables (#numTemps)
	(index 24)		  4 bits:	number of arguments to the method (#numArgs)
	(index 28)		  2 bits:	reserved for an access modifier (00-unused, 01-private, 10-protected, 11-public), although accessors for bit 29 exist (see #flag).
	sign bit:			  1 bit:	selects the instruction set, >= 0 Primary, < 0 Secondary (#signFlag)

If the method has a primitive then the first bytecode of the method must be a callPrimitive: bytecode that encodes the primitive index.  This bytecode can encode a primitive index from 0 to 65535.

The trailer has two variant formats.  In the first variant, the last byte is at least 252 and the last four bytes represent a source pointer into one of the sources files (see #sourcePointer).  In the second variant, the last byte is less than 252, and the last several bytes are a compressed version of the names of the method's temporary variables.  The number of bytes used for this purpose is the value of the last byte in the method.

While there are disadvantages to this "flat" representation (it is impossible to add named instance variables to CompiledMethodor its subclasses, but it is possible indirectly; see AdditionalMethodState) it is effective for interpreters.  It means that both bytecodes and literals can be fetched directly from a single method object, and that only one object, the method, must be saved and restored on activation and return.  A more natural representation, in which there are searate instance variables for the bytecode, and (conveniently) the literals, requires either much more work on activation and return setting up references to the literals and bytecodes, or slower access to bytecodes and literals, indirecting on each access.

The last literal of a CompiledMethod is reserved for special use by the kernel and/or the virtual machine.  It must either be the methodClassAssociation, used to implement super sends, or nil, if the method is anonymous.

By convention, the penultimate literal is reserved for special use by the kernel. In CompiledMethod instances it must either be the method selector, or an instance of AdditionalMethodState which holds the selector and any pragmas or properties in the method.

"
Class {
	#name : 'CompiledMethod',
	#superclass : 'ByteArray',
	#classVars : [
		'LargeFrame',
		'PreferredBytecodeSetEncoderClass',
		'SmallFrame'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'services' }
CompiledMethod class >> authorAndStamp: aStamp partsDo: aBlock [
	| stamp aux timePart datePart dateAndTimePart |

	aStamp ifNil: [^aBlock value: nil value: nil].
	stamp := aStamp.
	aux := stamp indexOf: $:.
	aux = 0 ifFalse: [
		timePart := stamp copyFrom: aux-2 to: stamp size.
		stamp := stamp copyFrom: 1 to: aux-4 ].
	aux := stamp lastIndexOf: Character space.
	(stamp indexOf: $/ startingAt: (stamp indexOf: $/)+1) > 0 ifTrue: [
		datePart := stamp copyFrom: aux+1 to: stamp size.
		stamp := aux > 0 ifTrue: [stamp copyFrom: 1 to: aux] ifFalse: ['unknown']].
	dateAndTimePart := aStamp copyFrom: aux+1 to: aStamp size.
	^aBlock
		value: stamp withBlanksTrimmed
		value: (datePart ifNotNil: [
			DateAndTime
				date: (Date fromString: datePart)
				time: (timePart ifNotNil: [Time fromString: timePart withBlanksTrimmed])])
]

{ #category : 'instance creation' }
CompiledMethod class >> basicNew: size [

	self error: 'CompiledMethods may only be created with newMethod:header:' 
]

{ #category : 'method encoding' }
CompiledMethod class >> byteCodeSetsKnownToTheVM [
	"Answer the encoder names for the bytecode sets reported to be supported
	by the virtual machine. Informational, failure indicates only that the VM does
	not support the primitive query."

	"CompiledMethod byteCodeSetsKnownToTheVM"

	<primitive: 'primitiveBytecodeSetsAvailable'>
	^ self primitiveFailed
]

{ #category : 'source code management aux' }
CompiledMethod class >> classCategoryFrom: tokens [

	^self field: #category: from: tokens ifAbsentOrNil: [ SystemOrganizer defaultClassCategory ]
]

{ #category : 'instance creation' }
CompiledMethod class >> createMethod: numberOfBytesForAllButTrailer trailer: bytes header: headerWord  [
	| meth delta |
	meth := self newMethod: numberOfBytesForAllButTrailer + bytes size header: headerWord.
	"copy the encoded trailer data"
	delta := meth size - bytes size.
	1 to: bytes size do: [ :i |
		meth at: delta + i put: (bytes at: i)].
	^meth
]

{ #category : 'source code management aux' }
CompiledMethod class >> field: aFieldName from: tokens ifAbsentOrNil: aBlock [

	| fieldValue fieldIndex |
	
	fieldIndex := tokens indexOf: aFieldName ifAbsent: [ ^aBlock value ].
	fieldValue := tokens at: fieldIndex + 1 ifAbsent: [ ^aBlock value ].
	
	^ fieldValue ifNil: aBlock
]

{ #category : 'class initialization' }
CompiledMethod class >> fullFrameSize  "CompiledMethod fullFrameSize" [
	^ LargeFrame
]

{ #category : 'private' }
CompiledMethod class >> handleFailingFailingNewMethod: numberOfBytes header: headerWord [
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
	^self newMethod: numberOfBytes header: headerWord
]

{ #category : 'private' }
CompiledMethod class >> handleFailingNewMethod: numberOfBytes header: headerWord [
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
	^self handleFailingFailingNewMethod: numberOfBytes header: headerWord
]

{ #category : 'method encoding' }
CompiledMethod class >> headerFlagForEncoderClass: anEncoderClass [
	
	(anEncoderClass includesBehavior: EncoderForV3PlusClosures) ifTrue: [^0].
	(anEncoderClass includesBehavior: EncoderForSistaV1) ifTrue: [^SmallInteger minVal].
	
	self error: 'The encoder is not one of the two installed bytecode sets'
]

{ #category : 'class initialization' }
CompiledMethod class >> initialize    "CompiledMethod initialize" [
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PreferredBytecodeSetEncoderClass ifNil:
		[PreferredBytecodeSetEncoderClass := EncoderForV3PlusClosures]
]

{ #category : 'source code management aux' }
CompiledMethod class >> libraryNameFrom: tokens [

	^ self field: #library: from: tokens ifAbsentOrNil: nil
]

{ #category : 'constants' }
CompiledMethod class >> maxNumLiterals [
	"The current header format and the VM's interpretation of it allows for a maximum of 32767 literals."

	^32767
]

{ #category : 'instance creation' }
CompiledMethod class >> new [
	"This will not make a meaningful method, but it could be used
	to invoke some otherwise useful method in this class."
	^ self newMethod: 0 header: 0
]

{ #category : 'instance creation' }
CompiledMethod class >> new: size [

	self error: 'CompiledMethods may only be created with newMethod:header:'
]

{ #category : 'instance creation' }
CompiledMethod class >> newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex [
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| method pc |
"15 | 63 | 32767"
	nArgs > 15
		ifTrue: [^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63
		ifTrue: [^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 32767
		ifTrue: [^self error: 'Cannot compile -- too many literals'].

	method := self
				createMethod: numberOfBytes
				trailer: trailer
				header:  (CompiledMethod headerFlagForEncoderClass: PreferredBytecodeSetEncoderClass)
						+ (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue: [
		pc := method initialPC.
		method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method
]

{ #category : 'instance creation' }
CompiledMethod class >> newFrom: aCompiledMethod [
	| inst |
	inst := super basicNew: aCompiledMethod size.
	1 to: aCompiledMethod size do: [:index |
		inst at: index put: (aCompiledMethod at: index)].
	^ inst.
]

{ #category : 'instance creation' }
CompiledMethod class >> newInstanceFrom: oldInstance variable: variable size: instSize map: map [
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"
	| new |
	new := self newFrom: oldInstance.
	1 to: instSize do: 
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	^new
]

{ #category : 'instance creation' }
CompiledMethod class >> newMethod: numberOfBytes header: headerWord [
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
	^self primitiveFailed
]

{ #category : 'accessing' }
CompiledMethod class >> preferredBytecodeSetEncoderClass [
	^PreferredBytecodeSetEncoderClass
]

{ #category : 'preferences' }
CompiledMethod class >> preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass [
	"Set the class that determines the bytecode set used to compile methods with.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self assert: (aBytecodeEncoderSubclass includesBehavior: BytecodeEncoder).
	(aBytecodeEncoderSubclass == EncoderForV3PlusClosures
	 or: [aBytecodeEncoderSubclass == EncoderForSistaV1]) ifTrue:
		[PreferredBytecodeSetEncoderClass := aBytecodeEncoderSubclass.
		 ^self].
	self error: 'Cannot set preferred bytecode set.  Both of the current sets appear to be in use.'
]

{ #category : 'source code management aux' }
CompiledMethod class >> priorReferenceFrom: tokens [

	^ self field: #prior: from: tokens ifAbsentOrNil: nil
]

{ #category : 'evaluating' }
CompiledMethod class >> receiver: receiver withArguments: argArray executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and the arguments in argArray"

	<primitive: 188>
	^receiver withArgs: argArray executeMethod: compiledMethod
]

{ #category : 'class initialization' }
CompiledMethod class >> smallFrameSize [

	^ SmallFrame
]

{ #category : 'source code management aux' }
CompiledMethod class >> stampFrom: tokens [

	^self field: #stamp: from: tokens ifAbsentOrNil: [ '' ]	
	

]

{ #category : 'instance creation' }
CompiledMethod class >> toReturnConstant: index trailerBytes: trailer [
	"Answer an instance of me that is a quick return of the constant
	indexed in (true false nil -1 0 1 2)."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256 + index
]

{ #category : 'instance creation' }
CompiledMethod class >> toReturnField: field trailerBytes: trailer [
	"Answer an instance of me that is a quick return of the instance variable 
	indexed by the argument, field."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 264 + field
]

{ #category : 'instance creation' }
CompiledMethod class >> toReturnSelf [
	"Answer an instance of me that is a quick return of the instance (^self)."

	^ self toReturnSelfTrailerBytes: #(0 0 0 0)
]

{ #category : 'instance creation' }
CompiledMethod class >> toReturnSelfTrailerBytes: trailer [
	"Answer an instance of me that is a quick return of the instance (^self)."

	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256
]

{ #category : 'services' }
CompiledMethod class >> unboundMethods [
	^self allInstances reject: [ :m | m isInstalled ]
]

{ #category : 'preferences' }
CompiledMethod class >> useSistaBytecodes [
	"
	CompiledMethod useSistaBytecodes.

		| nPrimary nSecondary |
		Smalltalk garbageCollect.
		nPrimary := nSecondary := 0.
		CompiledMethod allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self preferredBytecodeSetEncoderClass: EncoderForSistaV1.
	Compiler recompileAll.
	ProcessorScheduler startUp.
	Delay startTimerEventLoop.
	EphemeronFinalizationProcess restartFinalizationProcess.
	Sensor installInterruptWatcher.
	Sensor installEventTickler.
	UISupervisor ui ifNotNil: [ :w |
		[
			w showTaskbar.
			UISupervisor newUIProcess.
			Smalltalk garbageCollect ] fork.
		w
			initSortBlocks;
			hideTaskbar ].
	PreferenceSet installMiscPreferences.
	Smalltalk garbageCollect.
]

{ #category : 'preferences' }
CompiledMethod class >> useV3Bytecodes [
	"
	CompiledMethod useV3Bytecodes.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self preferredBytecodeSetEncoderClass: EncoderForV3PlusClosures.
	Compiler recompileAll.
	ProcessorScheduler startUp.
	Delay startTimerEventLoop.
	EphemeronFinalizationProcess restartFinalizationProcess.
	Sensor installInterruptWatcher.
	Sensor installEventTickler.
	UISupervisor ui ifNotNil: [ :w |
		[
			w showTaskbar.
			UISupervisor newUIProcess.
			Smalltalk garbageCollect ] fork.
		w
			initSortBlocks;
			hideTaskbar ].
	PreferenceSet installMiscPreferences.
	Smalltalk garbageCollect.
]

{ #category : 'comparing' }
CompiledMethod >> = method [
	| numLits lit1 lit2 firstLitIndex |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits := self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false  ].

	firstLitIndex := 1.
	(#(117 120) includes: self primitive) ifTrue: [
		lit1 := self literalAt: firstLitIndex.
		lit2 := method literalAt: firstLitIndex.
			lit1 isArray
				ifTrue: [
					(lit2 isArray and: [ lit1 first = lit2 first and: [lit1 second = lit2 second]]) ifFalse: [
						^false ]]
				ifFalse: [ "ExternalLibraryFunction"
					(lit1 analogousCodeTo: lit2) ifFalse: [
						^false ]].
		firstLitIndex := 2 ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	firstLitIndex to: numLits-2 do: [ :i |
		lit1 := self literalAt: i.
		lit2 := method literalAt: i.
		lit1 = lit2 ifFalse: [
			"any other discrepancy is a failure"
			^ false ]].
	^true
]

{ #category : 'debugger support' }
CompiledMethod >> abstractPCForConcretePC: concretePC [
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	self flag: 'belongs in DebuggerMethodMap?'.
	abstractPC := 1.
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue
]

{ #category : 'printing' }
CompiledMethod >> abstractSymbolic [
	"Answer a String that contains a list of all the byte codes in a method with a
	 short description of each, using relative addresses and not including code bytes."

	^ String streamContents: [ :strm |
		self longPrintRelativeOn: strm indent: 0 ].
]

{ #category : 'testing' }
CompiledMethod >> accessesInstanceVariable: aName [

	^(self readsInstanceVariable: aName) or: [self writesInstanceVariable: aName].
]

{ #category : 'accessing' }
CompiledMethod >> accessorDescriptionOf: anInstVarName at: anInstVarIndex [
	
	| isReader isWriter |
	
	(self isGetterOf: anInstVarName at: anInstVarIndex) ifTrue: [ ^ 'getter' ].	
	(self isSetterOf: anInstVarName at: anInstVarIndex) ifTrue: [ ^ 'setter' ].
	
	isReader := self readsField: anInstVarIndex.
	isWriter := self writesField: anInstVarIndex.
	
	(isReader and: [ isWriter ]) ifTrue: [ ^ 'write/read' ].
	isReader ifTrue: [ ^ 'read' ].
	isWriter ifTrue: [ ^ 'write' ].
	
	^''
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> addPragma: aPragma [
	self properties: (self properties copyWith: aPragma)
]

{ #category : 'literals' }
CompiledMethod >> allImplicitLiteralsDo: aBlock [

	self codeLiteralsDo: [:compiledCode |
		compiledCode implicitLiteralsDo: aBlock].
]

{ #category : 'literals' }
CompiledMethod >> allLiterals [
	"Skip compiled-code objects. Keep literal arrays, bindings, etc."
	
	^ Array streamContents: [:result |
		self allLiteralsDo: [:literal | result nextPut: literal]]
]

{ #category : 'literals' }
CompiledMethod >> allLiteralsDo: aBlock [
	"Overwritten to really enumerate all literals. Also the implicit ones."
	
	self
		allLiteralsDo: aBlock
		scanForSpecial: true.
]

{ #category : 'literals' }
CompiledMethod >> allLiteralsDo: aBlock scanForSpecial: aBoolean [
	"Overwritten to skip certain (raw) literals."
		
	"Exclude method selector (or properties) and the method's class. Include code literals."
	1 to: self numLiterals - 2 do: [:index |
		(self literalAt: index) allLiteralsDo: aBlock scanForSpecial: aBoolean].

	"Enumerate the implicit literals in bytecodes of the receiver."
	aBoolean ifTrue: [self implicitLiteralsDo: aBlock].

	"Enumerate method selector only through additional method state."
	self penultimateLiteral isMethodProperties
		ifTrue: [self penultimateLiteral allLiteralsDo: aBlock scanForSpecial: aBoolean]
]

{ #category : 'converting' }
CompiledMethod >> asMethodReference [
	
	^MethodReference method: self
]

{ #category : 'converting' }
CompiledMethod >> asString [

	^self printString
]

{ #category : 'time stamp' }
CompiledMethod >> author [
	"
	(CompiledMethod compiledMethodAt: #author) author
	(BitBlt>>#colorMap) author
	"
	
	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | authorInitials ]
]

{ #category : 'time stamp' }
CompiledMethod >> authorAndStamp [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	| asSaved |
	asSaved := self authorAndStampAsSaved.
	asSaved isEmpty ifTrue: [ ^asSaved ].
	self class authorAndStamp: asSaved partsDo: [ :authorInitials :dateAndTime |
		^authorInitials, ' ' , (dateAndTime date printFormat: #(1 2 3 $/ 2 1)), ' ',
			(String streamContents: [:s | dateAndTime time print24: true showSeconds: true on: s]) ].
		
]

{ #category : 'time stamp' }
CompiledMethod >> authorAndStampAsSaved [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	| preamble tokens |
	preamble := self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens := Scanner new scanTokens: preamble.
	^CompiledMethod stampFrom: tokens
]

{ #category : 'auto complete' }
CompiledMethod >> autoCompleterDocumentationAppendingToParameter: aParameterAppendBlock toReturn: aReturnAppendBlock [
	
	"This message is sent either by the dynamic typing or live typing auto complete. If you do not have 
	live typing installed you will see one sender, do not refactor it! - Hernan"
	
	| methodNode text |
 
	text := self receiverTextAutoCompleterDocumentation.
	
	methodNode := self methodNode.
	text := self selectorAutoCompleterDocumentationAppendingTo: text using: methodNode appendingToParameter: aParameterAppendBlock.
	text := text append: aReturnAppendBlock value.
	text := self commentAutoCompleterDocumentationAppendigTo: text using: methodNode.
			
	^text
]

{ #category : 'debugger support' }
CompiledMethod >> blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock [
	"Support routine for startpcsToBlockExtents"
	| extentStart blockSizeOrLocator |
	self flag: 'belongs in DebuggerMethodMap'.
	extentStart := numbererBlock value.
	[scanner pc <= endPC] whileTrue:
		[blockSizeOrLocator := scanner interpretNextInstructionFor: BlockStartLocator new.
		 blockSizeOrLocator isInteger ifTrue:
			[self
				blockExtentsInto: aDictionary
				from: scanner pc
				to: scanner pc + blockSizeOrLocator - 1
				scanner: scanner
				numberer: numbererBlock]].
	aDictionary at: initialPC put: (extentStart to: numbererBlock value).
	^aDictionary
]

{ #category : 'user interface support' }
CompiledMethod >> browse [

	Smalltalk browseFull: self methodClass selector: self selector.
]

{ #category : 'accessing' }
CompiledMethod >> bytecodeSetName [
	^self encoderClass name copyReplaceAll: 'EncoderFor' with: ''
]

{ #category : 'accessing' }
CompiledMethod >> bytecodes [
	"Answer a ByteArray with my bytecodes."

	| initialPC answer |
	initialPC := self initialPC.
	answer := ByteArray new: self endPC-initialPC+1.
	1 to: answer size do: [ :i |
		answer at: i put: (self at: i-1+initialPC) ].
	^answer
]

{ #category : 'organization' }
CompiledMethod >> category [

	^self methodClass organization categoryOfElement: self selector
]

{ #category : 'source code management' }
CompiledMethod >> checkOKToAdd: size at: filePosition in: fileIndex [
	"Issue several warnings if the end of the changes file is approaching
	a fixed size limit, and finally halt with an error if the limit is reached."

	^ SourceFiles checkOKToAdd: size at: filePosition in: fileIndex

]

{ #category : 'printing' }
CompiledMethod >> classAndSelector [

	^String streamContents: [:stream | self printClassAndSelectorOn: stream ]
]

{ #category : 'accessing' }
CompiledMethod >> clearFlag [
	"Clear the user-level flag bit"

	self objectAt: 1 put: (self header bitAnd: (1 << 29) bitInvert)
]

{ #category : 'accessing' }
CompiledMethod >> clearSignFlag [
	"Clear the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifTrue:
		[self objectAt: 1 put: self header - SmallInteger minVal]
]

{ #category : 'literals' }
CompiledMethod >> codeLiterals [

	^ Array streamContents: [:stream |
		self codeLiteralsDo: [:compiledCode | stream nextPut: compiledCode]]
]

{ #category : 'literals' }
CompiledMethod >> codeLiteralsDo: aBlock [
	"Enumerate all literals that represent instances of CompiledCode. This is especially required for SistaV1."
	
	aBlock value: self.
	
	self literalsDo: [:literal | literal isCompiledCode ifTrue: [
		literal codeLiteralsDo: aBlock]].
]

{ #category : 'auto complete' }
CompiledMethod >> commentAutoCompleterDocumentationAppendigTo: text using: methodNode [
	
	| comment |

	comment := methodNode comment.
	^ comment
		ifNil: [ text ] 
		ifNotNil: [ text append: (self commentAutoCompleterDocumentationOf: comment)].
		
	
]

{ #category : 'auto complete' }
CompiledMethod >> commentAutoCompleterDocumentationOf: comment [
 	
	^ Text 
		string: (String streamContents: [ :stream |				
			stream
				newLine; newLine;
				nextPutAll: comment first ])
		attributes: (SHTextStylerST80 attributesFor: #comment)
]

{ #category : 'decompiling' }
CompiledMethod >> compilerClass [
	^self methodClass 
		ifNil: [Compiler] 
		ifNotNil: [:class | class compilerClass].
]

{ #category : 'closures' }
CompiledMethod >> containsBlockClosures [

	| scanner |
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		self encoderClass createClosureScanBlock
]

{ #category : 'initialization' }
CompiledMethod >> copyWithBytecodes: bytecodes [
	"Assume bytecodes are regular bytecodes, even if we ourselves are a primitive method.
	The new bytecodes are in the same bytecode set as the current ones.
	Remove the primitive bit from the header if present. Answer will not be a primitive."

	| copy header |
	header := self header bitAt: 17 put: 0.
	copy := CompiledMethod newMethod: bytecodes size+4 header: header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	bytecodes withIndexDo: [ :byte :i |
		copy at: i+self initialPC-1 put: (bytecodes at: i) ].
	^ copy
]

{ #category : 'initialization' }
CompiledMethod >> copyWithTrailerBytes: bytes [
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start := self initialPC.
	end := self endPC.
	copy := CompiledMethod newMethod: end - start + 1 + bytes size
				header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	1 to: bytes size do: [:i | copy at: end + i put: (bytes at: i)].
	^ copy
]

{ #category : 'method node' }
CompiledMethod >> createMethodNode [
	
	^self createMethodNodeOptimizeSpecialSends: true
]

{ #category : 'method node' }
CompiledMethod >> createMethodNodeOptimizeSpecialSends: doOptimizeSpecialSends [
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail.
	To avoid incopatibility with other parsers, we only send the message methodNodeFor:noPattern:optimizeSpecialSends: 
	when it is false that is the special case needed for the refactorings - Hernan"
	^ (doOptimizeSpecialSends
		ifTrue: [ 	(aClass methodNodeFor: source) ]
		ifFalse: [ (aClass methodNodeFor: source noPattern: false optimizeSpecialSends: false) ]) 
			ifNil: [ self decompile ].
]

{ #category : 'time stamp' }
CompiledMethod >> dateAndTime [
	"Answer a DateAndTime object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateAndTime
	"

	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | dateAndTime ]
]

{ #category : 'time stamp' }
CompiledMethod >> dateMethodLastSaved [
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSaved) dateMethodLastSaved
	"

	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]
]

{ #category : 'debugger support' }
CompiledMethod >> debuggerMap [
	^DebuggerMethodMap forMethod: self
]

{ #category : 'decompiling' }
CompiledMethod >> decompile [
	"Return the decompiled parse tree that represents self"

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].
	^class decompilerClass new decompile: selector in: class method: self
]

{ #category : 'printing' }
CompiledMethod >> decompileString [
	^self decompile decompileString
]

{ #category : 'decompiling' }
CompiledMethod >> decompilerClass [
	^self compilerClass decompilerClass
]

{ #category : 'accessing' }
CompiledMethod >> defaultSelector  [
	
	"Invent and answer an appropriate message selector (a Symbol) for me, 
	that is, one that will parse with the correct number of arguments."
	
	^Scanner doItSelector numArgs: self numArgs
]

{ #category : 'source code management' }
CompiledMethod >> destroySourcePointer [
	self setSourcePointer: 0
]

{ #category : 'auto complete' }
CompiledMethod >> dynamicTypingAutoCompleterDocumentation [
	
	^ self 
		autoCompleterDocumentationAppendingToParameter: [ :parameterName | '' ]
		toReturn: [ '' ]
]

{ #category : 'closures' }
CompiledMethod >> embeddedBlockClosures [
	"
	(CompiledMethod >> #embeddedBlockClosures)embeddedBlockClosures
	"
	| bms extractor scanner |
	bms := OrderedCollection new.
	scanner := self scanner.
	extractor := ClosureExtractor withAction: [ :c | bms add: c ] andScanner: scanner.
	[ scanner pc <= self endPC ] whileTrue: [ scanner interpretNextInstructionFor: extractor ].
	^ bms
]

{ #category : 'accessing' }
CompiledMethod >> encoderClass [
	"Answer the encoder class that encoded the bytecodes in this method.
	 The sign flag bit is used by the VM to select a bytecode set.  This formulation
	 may seem odd but this has to be fast, so no property probe unless needed."

	^self header >= 0
		ifTrue: 
			[ EncoderForV3PlusClosures ]
		ifFalse:
			[ EncoderForSistaV1 ]
]

{ #category : 'accessing' }
CompiledMethod >> endPC [
	"Answer the index of the last bytecode."
	| size flagByte |
	"Can't create a zero-sized CompiledMethod so no need to use last for the errorEmptyCollection check.
	 We can reuse size."
	size := self size.
	flagByte := self at: size.
	flagByte = 0 ifTrue: [
		"If last byte = 0, may be either 0, 0, 0, 0 or just 0"
		size-1 to: size-3 by: -1 do: [ :i |
			i < self initialPC ifTrue: [ ^ i ].
			(self at: i) = 0 ifFalse: [ ^ i ]].
		^size - 4].
	flagByte < 252 ifTrue: [
		"Magic sources (temp names encoded in last few bytes)"
		^flagByte <= 127
			ifTrue: [size - flagByte - 1]
			ifFalse: [size - (flagByte - 128 * 128) - (self at: size - 1) - 2]].
	"Normal 4-byte source pointer"
	^size - 4
]

{ #category : 'comparing' }
CompiledMethod >> equivalentTo: aCompiledMethod  [
	"does not work yet with non-RB parseTrees"
	^ self = aCompiledMethod
		or: [self class == aCompiledMethod class
				and: [self numArgs == aCompiledMethod numArgs
						and: [self decompile = aCompiledMethod decompile]]].
]

{ #category : 'inspecting' }
CompiledMethod >> explorerContents [
	"(CompiledMethod compiledMethodAt: #explorerContents) explore"
	
	^Array streamContents:
		[:s| | tokens |
		tokens := Scanner new scanTokens: (self headerDescription readStream skipTo: $"; upTo: $").
		s nextPut: (ObjectExplorerWrapper
						with: ((0 to: tokens size by: 2) collect:
								[:i| i = 0 ifTrue: [self header] ifFalse: [{tokens at: i - 1. tokens at: i}]])
						name: 'header'
						model: self).
		(1 to: self numLiterals) do:
			[:key|
			s nextPut: (ObjectExplorerWrapper
							with: (self literalAt: key)
							name: ('literal', key printString contractTo: 32)
							model: self)].
		self isQuick
			ifTrue: [s nextPut: (ObjectExplorerWrapper
									with: (String streamContents: [ :strm | self printQuickOn: strm indent: 0])
									name: #symbolic
									model: self)].
		self symbolicLinesDo:
				[:pc :line|
				pc <= 1
					ifTrue:
						[s nextPut: (ObjectExplorerWrapper
										with: line
										name: 'pragma'
										model: self)]
					ifFalse:
						[s nextPut: (ObjectExplorerWrapper
										with: line
										name: pc printString
										model: self)]].
				"should be self numLiterals + 1 * Smalltalk wordSize + 1"
		self endPC + 1
			to: self basicSize
			do: [:key|
				s nextPut: (ObjectExplorerWrapper
								with: (self basicAt: key)
								name: key printString
								model: self)]]
]

{ #category : 'source code management' }
CompiledMethod >> fileIndex [
	^SourceFiles fileIndexFromSourcePointer: self sourcePointer
]

{ #category : 'source code management' }
CompiledMethod >> filePosition [
	^SourceFiles filePositionFromSourcePointer: self sourcePointer
]

{ #category : 'accessing' }
CompiledMethod >> flag [
	"Answer the user-level flag bit"

	^((self header bitShift: -29) bitAnd: 1) = 1
]

{ #category : 'copying' }
CompiledMethod >> flattenTo: flattenedStream [
	"No senese in flattening the method's bytes."

	flattenedStream nextPut: self
]

{ #category : 'private' }
CompiledMethod >> flushCache [
	"Tell the virtual machine to remove all references to this method from its method
	 lookup cache(s), if it has them. This is provided for backwards compatibility.
	 When a method is redefined (added to, removed from, or replaced in, a method
	 dictionary) then Symbol flushCache should be used. This is because adding or
	 removing a method may alter the method that a given selector should invoke
	 for methods other than the receiver.  For example, if a subclass inherits a
	 method and this is overridden, flushing the new method will not flush the existing
	 method.

	 If a method is modified in-place (for example, some bytecode is replaced by
	 bytecode that effects a breakpoint) then voidCogVMState should be used."

	<primitive: 116>
]

{ #category : 'accessing' }
CompiledMethod >> frameSize [
	"Answer the size of temporary frame needed to run the receiver."
	"NOTE:  Versions 2.7 and later use two sizes of contexts."

	(self header noMask: 16r20000)
		ifTrue: [^ SmallFrame]
		ifFalse: [^ LargeFrame]

]

{ #category : 'source code management' }
CompiledMethod >> getPreamble [
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file := SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFullFileEntry readStreamDo: [ :stream | | p |
			preamble := ''.
			p := 0 max: self filePosition.
			p < stream size ifTrue: [
				"It appears that on Linux systems, immediately after adding a
				new method, the OS file cache may not be updated, so contents
				may still not be accessible. Protect against that."
				stream position: p.
				"Skip back blank space."
				stream backChunk.
				"Find and answer preamble chunk."
				preamble := stream backChunk ]]
	] on: FileDoesNotExistException do: [ :ex | preamble := '' ].
	^ preamble
]

{ #category : 'source code management' }
CompiledMethod >> getSource [
	^ self getSourceFor: self selector in:self methodClass.
]

{ #category : 'source code management' }
CompiledMethod >> getSourceFor: selector in: class [
	"Retrieve or reconstruct the source code for this method."

	| flagByte source |

	flagByte := self last.
	
	"If no source pointer..."
	source := flagByte < 252 
		ifTrue: [ nil ]
		ifFalse: [ 
			"Situation normal; read the sourceCode from the file
			An error can happen here if, for example, the changes file has been truncated by an aborted download.  
			The present solution is to ignore the error and fall back on the decompiler.  
			A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  
			Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling 
			into this error case, random source code will get returned."
			[self getSourceFromFile]
				on: Error
				do: [ :ex | ex return: nil]].
		
	"If source code not available, use DoIt source code or if absent decompile blind (no temps)"
	^source ifNil: [ (class decompilerClass new decompile: selector in: class method: self) decompileString ]
]

{ #category : 'source code management' }
CompiledMethod >> getSourceFromFile [
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."
	| position |
	(position := self filePosition) = 0 ifTrue: [^ nil].
	^ (RemoteString newFileNumber: self fileIndex position: position)
			text
]

{ #category : 'testing' }
CompiledMethod >> hasArgumentOrTemporaryNamed: aVariable [

	^self methodNode hasArgumentOrTemporaryNamed: aVariable

]

{ #category : 'breakpoints' }
CompiledMethod >> hasBreakpoint [
	"In the standard image only BreakingMethodWrapper instances have breakpoints."
	
	^false
]

{ #category : 'literals' }
CompiledMethod >> hasLiteral: aLiteral [
	"Overwrite this method to invoke the bytecode encoder scanner explicitly.
	 This might be removed if there would be a way to enumerate special literals in a compiled-code object.
	See #allLiteralsDo:.
	See #hasLiteralExplicitly:. Prefer this for finding senders of selectors."

	^ self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: (self encoderClass canBeSpecialLiteral: aLiteral)
]

{ #category : 'testing' }
CompiledMethod >> hasLiteralExplicitly: aLiteral [
	"Similar to #hasLiteral:
	But do not include methods that (for example) #hasLiteral: #==
	only because the Compiler implements #ifNil: that way.
	See #hasLiteral:.
	Useful for finding senders of selectors."

	| canBeSpecialLiteral |
	canBeSpecialLiteral := self encoderClass canBeSpecialLiteral: aLiteral.

	"If we don't hasLiteral: it, just answer false."
	(self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: canBeSpecialLiteral)
			ifFalse: [ ^false ].

	"Any literal but these appears for sure in source code, so it appears explicitly."
	canBeSpecialLiteral
		ifFalse: [ ^true ].

	"#at:put: is the only special selector that can't be found with #findString:
	But, it is also not generated implicitly by the Compiler.
	So we know it is actually sent in source code anyway."
	aLiteral = #at:put: ifTrue: [
		^true ].

	"Other special selectors, like #== must be found in source code to be considered."
	^(self getSource findString: aLiteral) > 0
]

{ #category : 'literals' }
CompiledMethod >> hasLiteralSuchThat: litBlock [
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	
	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralSuchThat: litBlock) ifTrue: [ ^true ]]. 

	2 to: self numLiterals + 1 do: [ :index |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false
]

{ #category : 'testing' }
CompiledMethod >> hasNewPropertyFormat [
	"As of the closure compiler all methods have (or better have) the new
	 format where the penultimate literal is either the method's selector
	 or its properties and the ultimate literal is the class association."
	^true
]

{ #category : 'testing' }
CompiledMethod >> hasReportableSlip [
	"Answer whether the receiver contains anything that should be brought 
	to the attention of the author when filing out. Customize the lists here 
	to suit your preferences. If slips do not get reported in spite of your 
	best efforts here, make certain that the Preference 'checkForSlips' is set 
	to true."

	#(#halt #halt: #hottest #toRemove #personal #urgent  #haltOnce #haltOnce: #haltIf: )
		do: [ :aLit | 
			(self hasLiteral: aLit)
				ifTrue: [^ true]].
	#(#Transcript #AA #BB #CC #DD #EE )
		do: [ :aSymbol | 	| assoc |
			(assoc := Smalltalk
				associationAt: aSymbol
				ifAbsent: nil)
					ifNotNil: [(self hasLiteral: assoc)
						ifTrue: [^ true]]].
	^ false
]

{ #category : 'testing' }
CompiledMethod >> hasVariableBindingTo: aClass  [
	
	self literalsDo: [ :aLiteral |
		(aLiteral isVariableBinding and: [ aLiteral value = aClass ]) ifTrue: [ ^true ]].
	
	^false
]

{ #category : 'comparing' }
CompiledMethod >> hash [
	"CompiledMethod>>#= compares code, i.e. same literals and same bytecode.
	 So we look at the header, methodClass and some bytes between initialPC and endPC,
	 but /not/ the selector because the equal method does not compare selectors.
	 Note that we must override ByteArray>hash which looks at all bytes of the receiver.
	 Using bytes from the pointer part of a COmpiledmethod can lead to a variable hash
	 if and when when the GC moves literals in the receiver.
	jmv: As I made #= not to compare for methodClass, remove it from here."
	| initialPC endPC hash |
	initialPC := self initialPC.
	endPC := self endPC.
	hash := self species hash + self header + initialPC + endPC "+ self methodClass hash" bitAnd: 16rFFFFFFF.
	"sample approximately 20 bytes"
	initialPC to: endPC by: (endPC - initialPC // 20 max: 1) do: [ :i |
		hash := hash + (self at: i)].
	^hash

	"(CompiledMethod>>#hash) hash"
]

{ #category : 'literals' }
CompiledMethod >> header [
	"Answer the word containing the information about the form of the
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1
]

{ #category : 'literals' }
CompiledMethod >> headerDescription [
	"Answer a description containing the information about the form of the
	 receiver and the form of the context needed to run the receiver."

	^String streamContents: [ :stream |
		stream
			print: self header; newLine;
			nextPutAll: '"primitive: '; print: self primitive; newLine;
			nextPutAll: ' numArgs: '; print: self numArgs; newLine;
			nextPutAll: ' numTemps: '; print: self numTemps; newLine;
			nextPutAll: ' numLiterals: '; print: self numLiterals; newLine;
			nextPutAll: ' frameSize: '; print: self frameSize; newLine;
			nextPutAll: ' bytecodeSet: '; nextPutAll: self bytecodeSetName;
			nextPut: $"; newLine
	]
]

{ #category : 'accessing' }
CompiledMethod >> headerWithoutSignFlag [
	"The sign flag is different in 64 and 32 bit systems.
	Answer the rest of the header, that doesn't depend on that."

	| answer |
	answer := self header.
	answer < 0 ifTrue: [
		answer := answer - SmallInteger minVal ].
	^answer
]

{ #category : 'literals' }
CompiledMethod >> implicitLiteralsDo: aBlock [
	"Enumerate the implicit literals in bytecodes of the receiver."
	
	| stream client encoderClass endPC |
	"Enumerate special selectors & special literals such as true and false."
	stream := InstructionStream on: self.
	encoderClass := self encoderClass.
	"cache endPC for methods with embedded source; finding out the endPC is very slow in this case..."
	endPC := self endPC.
	client := ImplicitLiteralFinder new.
	[stream pc <= endPC] whileTrue:
		[| literalOrClient |
		literalOrClient := encoderClass interpretNextInstructionFor: client in: stream.
		literalOrClient ~~ client ifTrue:
			[aBlock value: literalOrClient]]
]

{ #category : 'literals' }
CompiledMethod >> indexOfLiteral: literal [
	"Answer the literal index of the argument, literal, or zero if none."
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do:
		[:index |
		literal == (self objectAt: index) ifTrue: [^index - 1]].
	^0
]

{ #category : 'accessing' }
CompiledMethod >> initialPC [
	"Answer the program counter for the receiver's first bytecode."
	^ (self numLiterals + 1) * Smalltalk wordSize + 1
]

{ #category : 'inspecting' }
CompiledMethod >> inspectorClass [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ CompiledMethodInspector
]

{ #category : 'testing' }
CompiledMethod >> is: aSymbol [
	^ aSymbol == #CompiledMethod or: [ super is: aSymbol ]
]

{ #category : 'testing' }
CompiledMethod >> isAbstract [
	
	^self sendsSelector: #subclassResponsibility
]

{ #category : 'testing' }
CompiledMethod >> isCompiledBlock [

	^false
]

{ #category : 'testing' }
CompiledMethod >> isCompiledCode [

	^true
]

{ #category : 'testing' }
CompiledMethod >> isCompiledMethod [

	^true
]

{ #category : 'testing' }
CompiledMethod >> isGetterOf: anInstVarName at: anInstVarIndex [
	
	| scanner instruction |
	
	"Quick return. (Short form)."
	(self selector = anInstVarName 
		and: [ self isReturnField 
		and: [ self returnField + 1 = anInstVarIndex ]])
			ifTrue: [ ^true ].
	
	"Long Form"
	self isQuick ifTrue: [ ^false ].
	self selector = anInstVarName ifFalse: [ ^false ].
	scanner := InstructionStream on: self.
	instruction := scanner nextInstruction.
	(instruction selector == #pushReceiverVariable:
	 and: [instruction argument = (anInstVarIndex - 1)]) ifFalse:
		[^false].
	^scanner nextInstruction selector == #methodReturnTop
]

{ #category : 'testing' }
CompiledMethod >> isInstalled [
	self methodClass ifNotNil:
		[ :class |
		self selector ifNotNil:
			[ :selector |
			^self == (class compiledMethodAt: selector ifAbsent: nil)]].
	^false
]

{ #category : 'testing' }
CompiledMethod >> isLinkedNamedPrimitive [
	"Answer if the receiver invokes a named primitive, and the method is linked to an actual primitive.
	 For example if the method hasn't yet been used in the current session, it won't be linked
	For additional details, In VMMaker package check references to 
	ExternalCallLiteralFlagsIndex
	ExternalCallLiteralTargetFunctionIndex
	(Apparently -1 means failure, most likely named primitive not found)
	"
	^self isNamedPrimitive and: [(self literalAt: 1) fourth ~= 0]
	
	"
	Smalltalk browseAllSelect: [:m| m isLinkedNamedPrimitive]
	"
]

{ #category : 'testing' }
CompiledMethod >> isNamedPrimitive [
	"Answer if the receiver invokes a named primitive."
	^self primitive = 117
	
	"
	Smalltalk browseAllSelect: [:m| m isNamedPrimitive]
	"
]

{ #category : 'testing' }
CompiledMethod >> isQuick [
	"Answer whether the receiver is a quick return (of self or of an instance 
	variable)."
	^ self primitive between: 256 and: 519
]

{ #category : 'testing' }
CompiledMethod >> isReturnField [
	"Answer whether the receiver is a quick return of an instance variable."
	^ self primitive between: 264 and: 519
]

{ #category : 'testing' }
CompiledMethod >> isReturnSelf [
	"Answer whether the receiver is a quick return of self."

	^ self primitive = 256
]

{ #category : 'testing' }
CompiledMethod >> isReturnSpecial [
	"Answer whether the receiver is a quick return of self or constant."

	^ self primitive between: 256 and: 263
]

{ #category : 'testing' }
CompiledMethod >> isSetterOf: anInstVarName at: anInstVarIndex [
	"Check if the receiver is a setter of the give inst var located at instVarIndex.
	 Only look at the bytecode to avoid soiurce code parsing issues."
	| scanner instruction |
	
	self isQuick ifTrue: [ ^false ].
	self selector = (anInstVarName, ':') ifFalse: [ ^false ].
	scanner := InstructionStream on: self.
	instruction := scanner nextInstruction.
	(instruction selector == #pushTemporaryVariable:
	 and: [instruction argument = 0]) ifFalse:
		[^false].
	instruction := scanner nextInstruction.
	(instruction selector == #popIntoReceiverVariable:
	 and: [instruction argument = (anInstVarIndex - 1)]) ifFalse:
		[^false].
	scanner peekInstruction selector == #doNop ifTrue: [
		scanner nextInstruction ].
	^scanner nextInstruction selector == #methodReturnReceiver
]

{ #category : 'testing' }
CompiledMethod >> isTestMethod [

    ^TestCase isTestMethod: self
]

{ #category : 'testing' }
CompiledMethod >> isValid [

	"To be polimorphic with MethodReference, important for refactorings - Hernan"
	^true
]

{ #category : 'source code management' }
CompiledMethod >> linesOfCode [
	"An approximate measure of lines of code.
	Use decompiled source code. In this way, the measure:
	- Doesn't include comments
	- Doesn't include blank lines
	- Is not sensitive to code formatting
	The motivation is to consider LOC as an expense, not an asset. Minimizing LOC is good.
	But it is not like that for comments!
	See #messageSendsCount"

	| lines |
	lines := 0.
	self decompileString lineIndicesDo: [ :start :endWithoutDelimiters :end |
		endWithoutDelimiters - start > 0 ifTrue: [
			lines := lines+1 ]].
	^lines
]

{ #category : 'literals' }
CompiledMethod >> literalAt: index  [
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1
]

{ #category : 'literals' }
CompiledMethod >> literalAt: index put: value  [
	"Replace the literal indexed by the first argument with the
	second argument."

	^self objectAt: index + 1 put: value
]

{ #category : 'literals' }
CompiledMethod >> literalStrings [
	| litStrs |
	litStrs := OrderedCollection new.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs
]

{ #category : 'literals' }
CompiledMethod >> literals [
	
	^ Array streamContents: [:result |
		self literalsDo: [:lit | result nextPut: lit]]
]

{ #category : 'literals' }
CompiledMethod >> literalsDo: aBlock [
	"Evaluate aBlock for each of the literals referenced by the receiver."
	1 to: self numLiterals do:
		[:index |
		aBlock value: (self objectAt: index + 1)]
]

{ #category : 'printing' }
CompiledMethod >> longPrintOn: aStream [
	"List of all the byte codes in a method with a short description of each" 

	self longPrintOn: aStream indent: 0
]

{ #category : 'printing' }
CompiledMethod >> longPrintOn: aStream indent: tabs [
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue:
		[ self printQuickOn: aStream indent: tabs ].

	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(InstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.

]

{ #category : 'printing' }
CompiledMethod >> longPrintRelativeOn: aStream indent: tabs [
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[^self longPrintOn: aStream indent: tabs].
	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(RelativeInstructionPrinter on: self)
		indent: tabs;
		printCode: false;
		printInstructionsOn: aStream.

]

{ #category : 'debugger support' }
CompiledMethod >> mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString [
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek == $(
				ifTrue: [tempSequence addAll: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~~ $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAll: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek == $]] whileTrue: [tempNames next].
		tempNames peek == $[ ifTrue:
			[tempNames next]].
	^map
]

{ #category : 'source code management' }
CompiledMethod >> messageSendsCount [
	"Number of message sends in code.
	A measure of the complexity of code. Alternative to #linesOfCode"

	| count scanner |
	count := 0.
	scanner := InstructionStream on: self.
	scanner scanFor:
		[:x | | selectorOrSelf |
		(selectorOrSelf := scanner selectorToSendOrSelf) == scanner ifFalse:
			[count := count + 1].
		false	"keep scanning"].
	^count
]

{ #category : 'source code ranges' }
CompiledMethod >> messageSendsRangesOf: aSentSelector [

	| methodNode ranges |

	methodNode := self methodNode.
	ranges := OrderedCollection new.

	methodNode nodesDo: [ :aParseNode |
		(aParseNode isMessageNamed: aSentSelector) ifTrue: [
			(methodNode rangeForNode: aParseNode ifAbsent: nil) ifNotNil: [ :range |
				ranges add: range ]]].

	^ranges 
]

{ #category : 'scanning' }
CompiledMethod >> messages [
	"Answer a Set of all the message selectors sent by this method."

	| scanner selectors auxSet |
	selectors := OrderedCollection new.
	auxSet := Set new.
	scanner := InstructionStream on: self.
	scanner	
		scanFor: [ :x |
			scanner addSelectorOnceTo: selectors auxSet: auxSet.
			false	"keep scanning"].
	^selectors
]

{ #category : 'scanning' }
CompiledMethod >> messagesDo: aBlock  [

	^ self messages do:aBlock.
]

{ #category : 'scanning' }
CompiledMethod >> messagesSequence [
	"Answer a Set of all the message selectors sent by this method."

	^Array streamContents:
		[:str| | scanner |
		scanner := InstructionStream on: self.
		scanner	scanFor: 
			[:x | | selectorOrSelf |
			(selectorOrSelf := scanner selectorToSendOrSelf) == scanner ifFalse:
				[str nextPut: selectorOrSelf].
			false	"keep scanning"]]
]

{ #category : 'accessing' }
CompiledMethod >> methodClass [
	"answer the class that I am installed in"
	^(self literalAt: self numLiterals) value.
]

{ #category : 'accessing' }
CompiledMethod >> methodClass: aClass [
	"set the class binding in the last literal to aClass"
	self literalAt: self numLiterals put: aClass binding
]

{ #category : 'accessing' }
CompiledMethod >> methodClassAssociation [
	"answer the association to the class that I am installed in, or nil if none."
	^self literalAt: self numLiterals
]

{ #category : 'accessing' }
CompiledMethod >> methodClassAssociation: aBinding [
	"sets the association to the class that I am installed in"
	^self literalAt: self numLiterals put: aBinding
]

{ #category : 'decompiling' }
CompiledMethod >> methodForDecompile [
	"This is a hook to allow recursive methods like MwMethodWrapper to avoid infinite recursion."
	^self
]

{ #category : 'method node' }
CompiledMethod >> methodNode [

	"Return the parse tree that represents self"
	
	"I do not save the method node in the #methodNode property if it does not
	exist to avoid keeping the method node in memory. 
	The methodNode is saved in the property #methodNode to avoid loosing the source
	code when debugging - Hernan"
	^self propertyValueAt: #methodNode ifAbsent: [ self createMethodNode ]
]

{ #category : 'method node' }
CompiledMethod >> methodNode: aMethodNode [

	self propertyValueAt: #methodNode put: aMethodNode
]

{ #category : 'method node' }
CompiledMethod >> methodNodeForLookingEquivalentNodes [

	^ self methodNodeWithoutImplicitSelfReturn movingTemporariesDeclarationToBlock
]

{ #category : 'method node' }
CompiledMethod >> methodNodeWithoutImplicitSelfReturn [
	
	^self methodNode withoutImplicitSelfReturn 
]

{ #category : 'accessing' }
CompiledMethod >> methodReference [
	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^MethodReference class: class selector: selector.
	
]

{ #category : 'initialization' }
CompiledMethod >> needsFrameSize: newFrameSize [
	"Set the largeFrameBit to accomodate the newFrameSize"
	| largeFrameBit header |
	largeFrameBit := 16r20000.
	(self numTemps + newFrameSize) > LargeFrame ifTrue:
		[^ self error: 'Cannot compile -- stack including temps is too deep'].
	header := self objectAt: 1.
	(header bitAnd: largeFrameBit) ~= 0
		ifTrue: [header := header - largeFrameBit].
	self objectAt: 1 put: header
 			+ ( ((self numTemps + newFrameSize) > SmallFrame or: [ self primitive = 84 "perform:withArguments:"])
					ifTrue: [largeFrameBit]
					ifFalse: [0])
]

{ #category : 'method node' }
CompiledMethod >> notOptimizedMethodNode [

	"Returns the method node without any optimization applied.
	 Heavely used in refactorings - Hernan"
	^self createMethodNodeOptimizeSpecialSends: false
]

{ #category : 'accessing' }
CompiledMethod >> numArgs [
	"Answer the number of arguments the receiver takes."

	^ (self header bitShift: -24) bitAnd: 16r0F
]

{ #category : 'accessing' }
CompiledMethod >> numLiterals [
	"Answer the number of literals used by the receiver."

	^ self header bitAnd: 16r7FFF
]

{ #category : 'accessing' }
CompiledMethod >> numTemps [
	"Answer the number of temporary variables used by the receiver."
	
	^ (self header bitShift: -18) bitAnd: 16r3F
]

{ #category : 'literals' }
CompiledMethod >> objectAt: index  [
	"Answer with the method header (if index=1) or a literal (if index >1) from the 
	receiver.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed
]

{ #category : 'literals' }
CompiledMethod >> objectAt: index put: value  [
	"Store the value argument into a literal in the receiver.   An index of 2 
	corresponds to the first literal.  Fails if the index is less than 2 or greater than 
	the number of literals.  Answer the value as the result.  Normally only the
	compiler sends this message, because only the compiler stores values in 
	CompiledMethods.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed
]

{ #category : 'file in/out' }
CompiledMethod >> objectForDataStream: refStrm [
	
	self primitive = 117 ifTrue: [self literals first at: 4 put: 0].

]

{ #category : 'tracing' }
CompiledMethod >> outboundPointersDo: aBlock [

	| numLiterals |
	aBlock value: self class.
	numLiterals := self numLiterals.
	1 to: numLiterals do: [:i | aBlock value: (self literalAt: i)]
]

{ #category : 'decompiling' }
CompiledMethod >> parserClass [
	^self methodClass 
		ifNil: [Compiler parserClass] 
		ifNotNil: [:class | class parserClass].
]

{ #category : 'debugger support' }
CompiledMethod >> pcPreviousTo: pc [
	| scanner client prevPc |
	self flag: 'belongs in DebuggerMethodMap?'.
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	^prevPc
]

{ #category : 'private' }
CompiledMethod >> penultimateLiteral [
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	^(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex]
		ifFalse: [nil]
]

{ #category : 'private' }
CompiledMethod >> penultimateLiteral: anObject [
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex put: anObject]
		ifFalse: [self error: 'insufficient literals']
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> pragmaAt: aKey [

	"Answer the pragma with selector aKey, or nil if none."

	^self withPropertiesDo: [ :properties | properties at: aKey ifAbsent: nil ] ifSelector: [ :selector | nil ].
	
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> pragmas [
	
	^self withPropertiesDo: [ :properties | properties pragmas ] ifSelector: [ :selector | #() ]
]

{ #category : 'accessing' }
CompiledMethod >> primitive [
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method."

	| initialPC |
	^(self header anyMask: 65536) "Is the hasPrimitive? flag set?"
		ifTrue: [(self at: (initialPC := self initialPC) + 1) + ((self at: initialPC + 2) bitShift: 8)]
		ifFalse: [0]
]

{ #category : 'printing' }
CompiledMethod >> primitiveErrorVariableName [
	"Answer the primitive error code temp name, or nil if none."
	self primitive > 0 ifTrue:
		[self pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil
]

{ #category : 'printing' }
CompiledMethod >> printClassAndSelectorOn: aStream [

	aStream
		print: self methodClass;
		nextPutAll: '>>'.
	self selector storeOn: aStream
]

{ #category : 'printing' }
CompiledMethod >> printOn: aStream [
	"Overrides method inherited from the byte arrayed collection."

	aStream nextPut: $(.
	self printClassAndSelectorOn: aStream.
	aStream space; nextPut: $"; nextPut: $a; space.
	aStream nextPutAll: self class name.
	aStream nextPut: $:; print: self size.
	aStream nextPut: $(; print: self identityHash; nextPut: $); nextPut: $"; nextPut: $)
]

{ #category : 'printing' }
CompiledMethod >> printPrimitiveOn: aStream [
	"Print the primitive on aStream"
	| primIndex primDecl |
	(primIndex := self primitive) = 0 ifTrue:
		[^self].
	primIndex = 120 ifTrue: "External call spec"
		[^aStream print: (self literalAt: 1); newLine].
	aStream nextPutAll: '<primitive: '.
	primIndex = 117
		ifTrue:
			[primDecl := self literalAt: 1.
			(primDecl at: 2) printOn: aStream.
			(primDecl at: 1) ifNotNil: [ :moduleName |
				aStream nextPutAll:' module: '.
				moduleName printOn: aStream]]
		ifFalse:
			[aStream print: primIndex].
	self primitiveErrorVariableName ifNotNil: [ :primitiveErrorVariableName |
		aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; newLine
]

{ #category : 'printing' }
CompiledMethod >> printQuickOn: aStream indent: tabs [

	self isQuick ifTrue:  [
		self isReturnSpecial
			ifTrue: [
				aStream tab: tabs; nextPutAll: 'Quick return ' , 
					(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255);
					newLine ]
			ifFalse: [
				aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'; newLine ]].
]

{ #category : 'accessing' }
CompiledMethod >> properties [

	"Answer the method properties of the receiver."
	
	^self 
		withPropertiesDo: [ :properties | properties ] 
		ifSelector: [ :selector | AdditionalMethodState forMethod: self selector: selector ]
]

{ #category : 'accessing' }
CompiledMethod >> properties: aMethodProperties [
	"Set the method-properties of the receiver to aMethodProperties."
	self literalAt: self numLiterals - 1
		put: (aMethodProperties isEmpty
				ifTrue: [aMethodProperties selector]
				ifFalse: [aMethodProperties
							setMethod: self;
							yourself])
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> propertyKeysAndValuesDo: aBlock [

	"Enumerate the receiver with all the keys and values."

	self withPropertiesDo: [ :properties | properties propertyKeysAndValuesDo: aBlock]
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> propertyValueAt: propName [

	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: nil] 
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> propertyValueAt: propName ifAbsent: aBlock [
	
	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: aBlock ] ifSelector: [ :selector | aBlock value ]
	
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> propertyValueAt: propName put: propValue [
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	
	self 
		withPropertiesDo: [:properties | 
			(properties includesProperty: propName) ifTrue: [^properties at: propName put: propValue].
			self penultimateLiteral: (properties
								copyWith: (Association
												key: propName asSymbol
												value: propValue)).
			^propValue ] 
		ifSelector: [ :selector |
			self penultimateLiteral: ((AdditionalMethodState
									selector: selector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
			^propValue].
]

{ #category : 'source code management' }
CompiledMethod >> putSource: sourceStr fromParseNode: methodNode class: class category: catName [
	withStamp: changeStamp libraryName: aStringOrNil inFile: fileIndex priorMethod: priorMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				libraryName: aStringOrNil
				priorMethod: priorMethod.
			file newLine ]
]

{ #category : 'source code management' }
CompiledMethod >> putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock [
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString := RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].
]

{ #category : 'object serialization' }
CompiledMethod >> readDataFrom: aDataStream size: varsOnDisk [
	"Fill in my fields.  My header and number of literals are already installed.  Must read both objects for the literals and bytes for the bytecodes."

	self error: 'Must use readMethod'.
]

{ #category : 'scanning' }
CompiledMethod >> readsField: varIndex  [
	"Answer whether the receiver loads the instance variable indexed by the 
	 argument."
	"eem 5/24/2008 Rewritten to no longer assume the compiler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	self isReturnField ifTrue: [^self returnField = varIndexCode].
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass instVarReadScanBlockFor: varIndexCode using: scanner)
]

{ #category : 'testing' }
CompiledMethod >> readsInstanceVariable: aName [

	^self readsField: (self methodClass indexOfInstanceVariable: aName) 
]

{ #category : 'scanning' }
CompiledMethod >> readsRef: literalAssociation  [
	"Answer whether the receiver loads the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass bindingReadScanBlockFor: litIndex using: scanner)
]

{ #category : 'auto complete' }
CompiledMethod >> receiverTextAutoCompleterDocumentation [
	
	| receiverString |
	
	receiverString := String streamContents: [ :stream |
		stream 
			nextPutAll: self methodClass typeName;
			nextPutAll: '>>' ].
	
	^Text string: receiverString attributes: (SHTextStylerST80 attributesFor: #patternKeyword).
		
	
]

{ #category : 'testing' }
CompiledMethod >> referencesParameterAt: parameterIndex [

	| methodNode implementorParameterNodeToRemove parameterRanges |

	methodNode := self methodNode.
	implementorParameterNodeToRemove := methodNode arguments at: parameterIndex.
	parameterRanges := methodNode positionsForTemporaryVariable: implementorParameterNodeToRemove name ifAbsent: [#()].

	^parameterRanges size ~= 1
]

{ #category : 'testing' }
CompiledMethod >> referencesSelf [

	^self methodNode referencesSelf
]

{ #category : 'testing' }
CompiledMethod >> referencesSuper [

	^self methodNode referencesSuper
]

{ #category : 'literals' }
CompiledMethod >> regularLiterals [
	"Answer an Array with the regular literals. I.e. not including the last two, #penultimateLiteral and the #methodClassAssociation"

	| answer |
	answer := Array new: self numLiterals-2.
	1 to: answer size do: [ :i |
		answer at: i put: (self literalAt: i) ].
	^answer
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> removeProperties [
	
	self penultimateLiteral: self selector
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> removeProperty: propName [
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^nil].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> removeProperty: propName ifAbsent: aBlock [
	"Remove the property propName if it exists.
	 Answer the evaluation of aBlock if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^aBlock value].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value
]

{ #category : 'accessing' }
CompiledMethod >> returnField [
	"Answer the index of the instance variable returned by a quick return 
	method."
	| prim |
	prim := self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]
]

{ #category : 'scanning' }
CompiledMethod >> scanFor: byteOrClosure [
	"Answer whether the receiver contains the argument as a bytecode,
	 if it is a number, or evaluates to true if a block."
	^ (InstructionStream on: self) scanFor: (byteOrClosure isBlock
													ifTrue: [byteOrClosure]
													ifFalse: [[:instr | instr = byteOrClosure]])
"
Smalltalk browseAllSelect: [:m | m scanFor: 134]
"
]

{ #category : 'accessing' }
CompiledMethod >> scanner [

	^ InstructionStream on: self
]

{ #category : 'accessing' }
CompiledMethod >> searchForClass [
	"search me in all classes, if found, return my class. Slow!"
	Smalltalk allBehaviorsDo: [:class | 
		(class methodDict keyAtIdentityValue: self ifAbsent: nil) ifNotNil: [^class]].
	^nil
]

{ #category : 'accessing' }
CompiledMethod >> searchForSelector [
	"search me in all classes, if found, return my selector. Slow!"

	Smalltalk allBehaviorsDo: [:class | 
		(class methodDict keyAtIdentityValue: self ifAbsent: nil) ifNotNil: [ :selector | ^selector]].
	^nil
]

{ #category : 'accessing' }
CompiledMethod >> selector [
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	^self 
		withPropertiesDo: [ :properties | properties selector ] 
		ifSelector: [ :selector | selector ]

]

{ #category : 'accessing' }
CompiledMethod >> selector: aSelector [
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	self 
		withPropertiesDo: [ :properties | properties selector: aSelector ] 
		ifSelector: [ :selector | | numberOfLiterals |
			(numberOfLiterals := self numLiterals) < 2 ifTrue: [self error: 'insufficient literals to hold selector'].
			self literalAt: numberOfLiterals - 1 put: aSelector]
]

{ #category : 'decompiling' }
CompiledMethod >> selectorAndArgumentsAsString [
	
	^self methodNode selectorAndArgumentsAsString 
]

{ #category : 'auto complete' }
CompiledMethod >> selectorAutoCompleterDocumentationAppendingTo: sourceText using: methodNode appendingToParameter: aParameterAppendBlock [

	| selector text |
	
	selector := methodNode selectorNode key.
	selector isUnary 
		ifTrue: [ text := sourceText append: (Text string: selector attributes: (SHTextStylerST80 attributesFor: #patternKeyword)) ]
		ifFalse: [
			text := sourceText.
			selector keywords 
				with: methodNode argumentNames 
				do: [ :keyword :argumentName |
					text := text append: (Text string: keyword attributes: (SHTextStylerST80 attributesFor: #patternKeyword)).
					text := text append: (Text string: ' ', argumentName, ' ' attributes: (SHTextStylerST80 attributesFor: #methodArg)).
					text := text append: (aParameterAppendBlock value: argumentName) ] 
				separatedBy: [ text := text append: String newLineString, String tab ]].

	^ text

]

{ #category : 'literals' }
CompiledMethod >> sendsSelector: aSelector  [
	| scanner |
	scanner := InstructionStream on: self.
	scanner scanFor: 
		[:x | 
		 scanner selectorToSendOrSelf == aSelector ifTrue:
			[^true].
		 false	"keep scanning"].
	^false
]

{ #category : 'scanning' }
CompiledMethod >> sendsToSuper [
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass superSendScanBlockUsing: scanner)
]

{ #category : 'accessing' }
CompiledMethod >> setSignFlag [
	"Set the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifFalse:
		[self objectAt: 1 put: self header + SmallInteger minVal]
]

{ #category : 'source code management' }
CompiledMethod >> setSourcePointer: srcPointer [
	srcPointer = 0 ifTrue: [
		0 to: 3 do: [ :i |
			self at: self size-i put: 0].
		^self].
	(srcPointer between: 16r1000000 and: 16r4FFFFFF) ifFalse: [self error: 'Source pointer out of range'].
	self at: self size put: (srcPointer bitShift: -24) + 251.
	1 to: 3 do: [:i |
		self at: self size-i put: ((srcPointer bitShift: (i-3)*8) bitAnd: 16rFF)]
]

{ #category : 'source code management' }
CompiledMethod >> setSourcePosition: position inFile: fileIndex  [
	self setSourcePointer: (SourceFiles sourcePointerFromFileIndex: fileIndex andPosition: position)
]

{ #category : 'accessing' }
CompiledMethod >> signFlag [
	"Answer the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	^self header < 0
]

{ #category : 'source code management' }
CompiledMethod >> sourceCode [

	"This method is implemented because getSource is not so intuitive - Hernan"
	^self getSource
]

{ #category : 'source code management' }
CompiledMethod >> sourcePointer [
	"Answer the integer which can be used to find the source file and position for this method.
	The returned value is either 0 (if no source is stored) or a number between 16r1000000 and 16r4FFFFFF.
	The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles."

	| pos |
	self last < 252 ifTrue: [^ 0  "no source"].
	pos := self last - 251.
	self size - 1 to: self size - 3 by: -1 do: [:i | pos := pos * 256 + (self at: i)].
	^pos
]

{ #category : 'method node' }
CompiledMethod >> sourceRangesOfNodesEquivalentTo: nodesToLookFor  [
	
	^self sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: EquivalentNodesFinder noSelection
]

{ #category : 'method node' }
CompiledMethod >> sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep [
	
	^self methodNodeForLookingEquivalentNodes sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep 
]

{ #category : 'debugger support' }
CompiledMethod >> startpcsToBlockExtents [
	"Answer a Dictionary of startpc to Interval of blockExtent, using the
	 identical numbering scheme described in and orchestrated by
	 BlockNode>>analyseArguments:temporaries:rootNode:.  This is
	 used in part to find the temp names for any block in a method, as
	 needed by the debugger.  The other half is to recompile the method,
	 obtaining the temp names for each block extent.  By indirecting through
	 the blockExtent instead of using the startpc directly we decouple the
	 debugger's access to temp names from the exact bytecode; insulating
	 debugging from minor changes in the compiler (e.g. changes in literal
	 pooling, adding prefix bytecodes, adding inst vars to CompiledMethod
	 in literals towards the end of the literal frame, etc).  If the recompilation
	 doesn't produce exactly the same bytecode at exactly the same offset
	 no matter; the blockExtents will be the same."
	| index |
	self flag: 'belongs in DebuggerMethodMap'.
	index := 0.
	^self
		blockExtentsInto: Dictionary new
		from: self initialPC
		to: self endPC
		scanner: (InstructionStream on: self)
		numberer: [| value | value := index. index := index + 2. value]
]

{ #category : 'file in/out' }
CompiledMethod >> storeDataOn: aDataStream [
	"Store myself on a DataStream.  I am a mixture of objects and raw data bytes.  Only use this for blocks.  Normal methodDictionaries should not be put out using ReferenceStreams.  Their fileOut should be attached to the beginning of the file."

	| byteLength lits |
	"No inst vars of the normal type"
	byteLength := self basicSize.
	aDataStream
		beginInstance: self class
		size: byteLength.
	lits := self numLiterals + 1.	"counting header"
	1 to: lits do:
		[:ii | aDataStream nextPut: (self objectAt: ii)].
	lits*Smalltalk wordSize+1 to: byteLength do:
		[:ii | aDataStream byteStream nextPut: (self basicAt: ii)].
			"write bytes straight through to the file"
]

{ #category : 'printing' }
CompiledMethod >> storeLiteralsOn: aStream forClass: aBehavior [
	"Store the literals referenced by the receiver on aStream, each terminated by a space."

	| literal |
	2 to: self numLiterals + 1 do:
		[:index |
		 aBehavior storeLiteral: (self objectAt: index) on: aStream.
		 aStream space]
]

{ #category : 'printing' }
CompiledMethod >> storeOn: aStream [

	^ self printOn: aStream
]

{ #category : 'printing' }
CompiledMethod >> symbolic [
	"Answer a String that contains a list of all the byte codes in a method 
	with a short description of each."

	^ String streamContents: [ :strm |
		self longPrintOn: strm ].
]

{ #category : 'printing' }
CompiledMethod >> symbolicLinesDo: aBlock [
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc |
	aStream := ReadWriteStream on: (String new: 64).

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(InstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line firstLine index |
			line := stream contents allButLast.
			firstLine := line lines first.
			firstLine size < line size ifTrue: [
				line := firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]
]

{ #category : 'debugger support' }
CompiledMethod >> tempsSubSequenceFrom: tempNamesStream [
	^Array streamContents:
		[:tsss|
		[tempNamesStream skipSeparators.
		 tempNamesStream atEnd
		 or: ['[]()' includes: tempNamesStream peek]] whileFalse:
			[tsss nextPut: (String streamContents:
							[:s|
							[s nextPut: tempNamesStream next.
							 tempNamesStream peek
								ifNil: [true]
								ifNotNil: [:peek| ' []()' includes: peek]] whileFalse])]]

	"thisContext method tempsSubSequenceFrom: 'les temps perdu(sont n''est pas la)' readStream"
	"thisContext method tempsSubSequenceFrom: ('les temps perdu(sont n''est pas la)' readStream skipTo: $(; yourself)"
]

{ #category : 'accessing' }
CompiledMethod >> trailer [

	| end trailer |
	end := self endPC.
	trailer := ByteArray new: self size - end.
	end + 1 to: self size do: [:i | 
		trailer at: i - end put: (self at: i)].
	^ trailer
]

{ #category : 'testing' }
CompiledMethod >> usesClosureBytecodes [
	"Answer whether the receiver was compiled using the closure compiler.
	 This is used to help DebuggerMethodMap choose which mechanisms to
	 use to inspect activations of the receiver.
	In Cuis all the subclasses of BytecodeEncoder #supportsClosures."
	
	^true
]

{ #category : 'evaluating' }
CompiledMethod >> valueWithReceiver: aReceiver arguments: anArray  [

	^self class receiver: aReceiver withArguments: anArray executeMethod: self
]

{ #category : 'private' }
CompiledMethod >> voidCogVMState [
	"Tell the VM to remove all references to any machine code form of the method.
	 This primitive must be called whenever a method is in use and modified.  This is
	 more aggressive (and *much* more costly) than flushCache since it must search
	 through all context objects, making sure that none have a (hidden) machine code pc
	 in the receiver.  Since modifying a method will likely change the generated machine code,
	 modifying a method (rather than redefining it) requires this more aggressive flush.

	If this is needed for a bunch of methods, and not just one, consider calling
	Smalltalk voidCogVMState
	(primitive 214)"

	<primitive: 215>
	^self flushCache
]

{ #category : 'accessing-pragmas & properties' }
CompiledMethod >> withPropertiesDo: withBlock [

	^self withPropertiesDo: withBlock ifSelector: [ :selector | nil ]
]

{ #category : 'accessing' }
CompiledMethod >> withPropertiesDo: withBlock ifSelector: notBlock [
	"Evaluate withBlock with our AdditionalMethodState,
	or evaluate notBlock with our selector
		(if we don't hold any Pragmas or properties in an AdditionalMethodState)"

	| penultimalLiteral |
	penultimalLiteral := self penultimateLiteral.
	^penultimalLiteral isMethodProperties 
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]
]

{ #category : 'scanning' }
CompiledMethod >> writesField: varIndex [
	"Answer whether the receiver stores into the instance variable indexed
	 by the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| varIndexCode scanner |
	self isQuick ifTrue: [^false].
	varIndexCode := varIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass instVarWriteScanBlockFor: varIndexCode using: scanner)
]

{ #category : 'testing' }
CompiledMethod >> writesInstanceVariable: aName [

	^self writesField: (self methodClass indexOfInstanceVariable: aName)
]

{ #category : 'scanning' }
CompiledMethod >> writesRef: literalAssociation  [
	"Answer whether the receiver stores into the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass bindingWriteScanBlockFor: litIndex using: scanner)
]

{ #category : 'literals' }
CompiledMethod >> xtraBindings [
	"Used for Workspace variables"
	^Array streamContents: [ :strm |
		self literalsDo: [ :literal |
			(literal class == Association and: [ literal key isString and: [ literal key first isLowercase ]]) ifTrue: [
				strm nextPut: literal ]]]
]

{ #category : 'file in/out' }
CompiledMethod >> zapSourcePointer [

	"clobber the source pointer since it will be wrong"
	0 to: 3 do: [ :i | self at: self size - i put: 0].

]
