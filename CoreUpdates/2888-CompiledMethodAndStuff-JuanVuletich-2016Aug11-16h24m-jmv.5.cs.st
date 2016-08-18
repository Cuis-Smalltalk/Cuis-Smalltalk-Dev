'From Cuis 4.2 of 25 July 2013 [latest update: #2880] on 11 August 2016 at 6:51:24 pm'!

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:28:32'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header >= 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:28:45'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header < 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 7/31/2016 16:11:49'!
createMethod: numberOfBytesForAllButTrailer trailer: bytes header: headerWord 
	| meth delta |
	meth _ self newMethod: numberOfBytesForAllButTrailer + bytes size header: headerWord.
	"copy the encoded trailer data"
	delta _ meth size - bytes size.
	1 to: bytes size do: [ :i |
		meth at: delta + i put: (bytes at: i)].
	^meth! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 18:21:24'!
newPreSpurBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	arguments. The remaining parts are not as yet determined."
	| largeBit primBits method |
	nTemps > 63 ifTrue:
		[^ self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 255 ifTrue:
		[^ self error: 'Cannot compile -- too many literals variables'].	
	largeBit := (nTemps + stackSize) > SmallFrame ifTrue: [1] ifFalse: [0].
	primBits := primitiveIndex <= 16r1FF
		ifTrue: [primitiveIndex]
		ifFalse: ["For now the high bit of primitive no. is in the 29th bit of header"
				primitiveIndex > 16r3FF ifTrue: [self error: 'prim num too large'].
				(primitiveIndex bitAnd: 16r1FF) + ((primitiveIndex bitAnd: 16r200) bitShift: 19)].
	method := self newMethod: numberOfBytes + trailer size
		header: (nArgs bitShift: 24) +
				(nTemps bitShift: 18) +
				(largeBit bitShift: 17) +
				(nLits bitShift: 9) +
				primBits.
	1 to: trailer size do:  "Copy the source code trailer to the end"
		[:i | method at: method size - trailer size + i put: (trailer at: i)].
	^ method! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 18:22:17'!
newSpurBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| method pc |
"15 | 63 | 32767"
	nArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 32767 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].

	method := self
				createMethod: numberOfBytes
				trailer: trailer
				header:    (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue:
		[pc := method initialPC.
		 method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method! !

!CompiledMethod class methodsFor: 'method encoding' stamp: 'eem 8/12/2014 12:39'!
headerFlagForEncoder: anEncoder
	anEncoder class == PrimaryBytecodeSetEncoderClass ifTrue:
		[^0].
	anEncoder class == SecondaryBytecodeSetEncoderClass ifTrue:
		[^SmallInteger minVal].
	self error: 'The encoder is not one of the two installed bytecode sets'! !


!BytecodeEncoder methodsFor: 'method generation' stamp: 'eem 8/10/2014 14:11'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	"Only for Spur!!"
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


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 8/8/2014 08:14'!
genCallPrimitive: primitiveIndex
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/11/2016 18:42:17'!
generatePreSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits literals stack method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	encoder supportsClosureOpcodes ifTrue:
		[self ensureClosureAnalysisDone.
		 encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"].
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	method := aCompiledMethodClass
				newBytes: blkSize
				trailerBytes: trailer 
				nArgs: arguments size
				nTemps: (encoder supportsClosureOpcodes
							ifTrue: [| locals |
									locals := arguments,
											  temporaries,
											  (primErrNode
												ifNil: [#()]
												ifNotNil: [{primErrNode}]).
									encoder
										noteBlockExtent: block blockExtent
										hasLocals: locals.
									locals size]
							ifFalse: [encoder maxTemp])
				nStack: 0
				nLits: (nLits := (literals := encoder allLiterals) size)
				primitive: primitive.
	nLits > 255 ifTrue:
		[^self error: 'Too many literals referenced'].
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: encoder].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method! !

!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/11/2016 18:42:31'!
generateSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := aCompiledMethodClass
					createMethod: blkSize
					trailer: trailer
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method! !


!ZBytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 8/4/2016 18:24:08'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
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


!ZEncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 8/8/2014 08:14'!
genCallPrimitive: primitiveIndex
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!Decompiler methodsFor: 'public access' stamp: 'jmv 8/11/2016 16:49:32'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| startpc end homeClass blockNode methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(homeClass := home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompile)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	constructor := self constructorForMethod: aBlock method.
	
	self withTempNames: method methodNode tempNames.

	self initSymbols: homeClass.
	startpc := aBlock startpc.
	end := aBlock endPC.
	stack := OrderedCollection new: method frameSize.
	lastJumpIfPcStack := OrderedCollection new.
	caseExits := OrderedCollection new.
	statements := OrderedCollection new: 20.
	super method: method pc: startpc - 5.
	blockNode := self blockTo: end.
	stack isEmpty ifFalse: [self error: 'stack not empty'].
	^blockNode statements first! !


!ZDecompiler methodsFor: 'public access' stamp: 'jmv 8/11/2016 16:49:27'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| startpc end homeClass blockNode methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(homeClass := home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompile)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	constructor := self constructorForMethod: aBlock method.
	
	self withTempNames: method methodNode tempNames.

	self initSymbols: homeClass.
	startpc := aBlock startpc.
	end := aBlock endPC.
	stack := OrderedCollection new: method frameSize.
	lastJumpIfPcStack := OrderedCollection new.
	caseExits := OrderedCollection new.
	statements := OrderedCollection new: 20.
	super method: method pc: startpc - 5.
	blockNode := self blockTo: end.
	stack isEmpty ifFalse: [self error: 'stack not empty'].
	^blockNode statements first! !


!Array methodsFor: 'converting' stamp: 'eem 6/23/2016 16:22'!
elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation."

	<primitive: 128 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[| maxRequired |
		 "In Spur, two-way become may involve making each pair of objects into a forwarder into a copy of the other.
		 So if become fails with #'insufficient object memory', garbage collect, and if necessary, grow memory."
		 maxRequired := (self detectSum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize])
						+ (otherArray detectSum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize]).
		 (Smalltalk garbageCollectMost < maxRequired
		  and: [Smalltalk garbageCollect < maxRequired]) ifTrue:
			[Smalltalk growMemoryByAtLeast: maxRequired].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'eem 6/23/2016 16:24'!
elementsForwardIdentityTo: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 The identityHashes remain with the pointers rather than with the objects so that
	 the objects in this array should still be properly indexed in any existing hashed
	 structures after the mutation."
	<primitive: 72 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[self error: 'The virtual machine is out-of-date.  Please upgrade.'].
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'eem 7/7/2016 11:08'!
elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 If copyHash is true, the identityHashes remain with the pointers rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes
	 of the objects in otherArray remain unchanged.  If you know what you're doing this
	 may indeed be what you want."
	<primitive: 249 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	self primitiveFailed! !


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 7/29/2016 16:04:24'!
headerDescription
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
	]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 7/31/2016 15:58:12'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."
	| flagByte source |
	flagByte := self last.
	flagByte < 252 ifTrue:
		["No source pointer -- decompile without temp names"
"		^ (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
		^ (Smalltalk clsDecompiler new decompile: selector in: class method: self)
			decompileString].

	"Situation normal; read the sourceCode from the file"
	source := [self getSourceFromFile]
				on: Error
		"An error can happen here if, for example, the changes file has been truncated by an aborted download.  The present solution is to ignore the error and fall back on the decompiler.  A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling into this error case, random source code will get returned."
				do: [ :ex | ex return: nil].
		
	^source
		ifNil: [
			"Source code not available. Decompile blind (no temps)"
"			 (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
			 (Smalltalk clsDecompiler new decompile: selector in: class method: self)
				decompileString]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 7/31/2016 15:36:54'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles == nil or: [(file _ SourceFiles at: fileIndex) == nil]) ifTrue:
		[^ self ].

	Smalltalk assureStartupStampLogged.
	file setToEnd.

	preambleBlock value: file.  "Write the preamble"
	remoteString _ RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.

	file nextChunkPut: ' '.
	InMidstOfFileinNotification signal ifFalse: [file flush].
	self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
	self setSourcePosition: remoteString position inFile: fileIndex! !

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 8/11/2016 16:47:06'!
copyWithTrailerBytes: bytes
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start _ self initialPC.
	end _ self endPC.
	copy _ CompiledMethod newMethod: end - start + 1 + bytes size
				header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	1 to: bytes size do: [:i | copy at: end + i put: (bytes at: i)].
	^ copy! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 8/11/2016 18:23:45'!
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	arguments. The remaining parts are not as yet determined."
	^ Smalltalk isSpur ifTrue: [
		self
			newSpurBytes: numberOfBytes
			trailerBytes: trailer
			nArgs: nArgs
			nTemps: nTemps
			nStack: stackSize
			nLits: nLits
			primitive: primitiveIndex ]
	ifFalse: [
		self
			newPreSpurBytes: numberOfBytes
			trailerBytes: trailer
			nArgs: nArgs
			nTemps: nTemps
			nStack: stackSize
			nLits: nLits
			primitive: primitiveIndex ]! !


!Compiler methodsFor: 'private' stamp: 'jmv 7/31/2016 15:29:48'!
evaluateCue: aCue ifFail: failBlock
	"Compiles the cue source into a parse tree, then generates code into
	a method. Finally, the compiled method is invoked from here via 	withArgs:executeMethod:, hence the system no longer creates Doit method
	litter on errors."

	| methodNode method value |
	methodNode := self compileCue: aCue noPattern: true ifFail: [^failBlock value].

	method := methodNode generate.

	value := cue receiver
				withArgs: (cue context ifNil: [#()] ifNotNil: [{cue context}])
				executeMethod: method.
	^ value
! !


!ZCompiler methodsFor: 'public access' stamp: 'jmv 7/31/2016 15:30:27'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: logFlag
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class := (aContext == nil ifTrue: [receiver] ifFalse: [aContext receiver]) class.
	self from: textOrStream class: class context: aContext notifying: aRequestor.
	methodNode := self translate: sourceStream noPattern: true ifFail: [^failBlock value].

	method := methodNode generate.

	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	logFlag ifTrue:
		[toLog := ((requestor respondsTo: #selection)  
			and:[(itsSelection := requestor selection) notNil
			and:[(itsSelectionString := itsSelection asString) isEmptyOrNil not]])
				ifTrue:[itsSelectionString]
				ifFalse:[sourceStream contents].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: aContext].

	"Evaluate now."
	value := receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method.

	^ value! !


!MethodNode methodsFor: 'code generation' stamp: 'jmv 7/31/2016 16:07:51'!
generate
	"The receiver is the root of a parse tree. Answer a CompiledMethod."

	^self generate: #(0 0 0 0)! !

!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/11/2016 18:43:28'!
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	^ Smalltalk isSpur 
		ifTrue: [ self generateSpur: trailer using: aCompiledMethodClass ]
		ifFalse: [ self generatePreSpur: trailer using: aCompiledMethodClass ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/31/2016 15:29:24'!
compileSelectionFor: anObject in: evalContext
	| methodNode |
	methodNode _ [
	Smalltalk clsCompiler new
		sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
		
		compileNoPattern: self selectionAsStream
		in: anObject class
		context: evalContext
		notifying: self
		ifFail: [ ^ nil ]]
		on: OutOfScopeNotification
		do: [ :ex |
			ex resume: true ].
	^ methodNode generate! !


!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 7/31/2016 15:44:00'!
sourceText
	^methodNode sourceText! !


!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'private' stamp: 'jmv 7/31/2016 15:44:35'!
privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := methodNode blockExtentsToTempsMap.
		 blockExtentsToTempRefs ifNil:
			["an empty method.  shouldn't be able to step into here but it
			  can happen in weird circumstances (i.e. with MethodWrapper)."
			blockExtentsToTempRefs := Dictionary new.
			blockExtentsToTempRefs
				at: (theContextsStartpcsToBlockExtents at: aContext startpc)
				put: {}].
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs outerTemps |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer|
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]! !

!methodRemoval: ZMethodNode #generateWithTempNames!
ZMethodNode removeSelector: #generateWithTempNames!
!methodRemoval: ZMethodNode #schematicTempNamesString!
ZMethodNode removeSelector: #schematicTempNamesString!
!methodRemoval: ZBytecodeEncoder #printSchematicTempNamesOn:blockExtents:fromIndex:!
ZBytecodeEncoder removeSelector: #printSchematicTempNamesOn:blockExtents:fromIndex:!
!methodRemoval: ZBytecodeEncoder #schematicTempNamesString!
ZBytecodeEncoder removeSelector: #schematicTempNamesString!
!methodRemoval: MethodNode #generateWithTempNames!
MethodNode removeSelector: #generateWithTempNames!
!methodRemoval: MethodNode #schematicTempNamesString!
MethodNode removeSelector: #schematicTempNamesString!
!methodRemoval: EncoderForV3PlusClosures #generateMethodOfClass:trailer:from:!
EncoderForV3PlusClosures removeSelector: #generateMethodOfClass:trailer:from:!
!methodRemoval: EncoderForV3 #generateMethodOfClass:trailer:from:!
EncoderForV3 removeSelector: #generateMethodOfClass:trailer:from:!
!methodRemoval: BytecodeEncoder #computePreSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
BytecodeEncoder removeSelector: #computePreSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
!methodRemoval: BytecodeEncoder #computeSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
BytecodeEncoder removeSelector: #computeSpurMethodHeaderForNumArgs:numTemps:numLits:primitive:!
!methodRemoval: BytecodeEncoder #generateMethodOfClass:trailer:from:!
BytecodeEncoder removeSelector: #generateMethodOfClass:trailer:from:!
!methodRemoval: BytecodeEncoder #printSchematicTempNamesOn:blockExtents:fromIndex:!
BytecodeEncoder removeSelector: #printSchematicTempNamesOn:blockExtents:fromIndex:!
!methodRemoval: BytecodeEncoder #schematicTempNamesString!
BytecodeEncoder removeSelector: #schematicTempNamesString!
!methodRemoval: CompiledMethod class #newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:flag:!
CompiledMethod class removeSelector: #newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:flag:!
!methodRemoval: CompiledMethod class #newBytes:trailerBytesArray:nArgs:nTemps:nStack:nLits:primitive:!
CompiledMethod class removeSelector: #newBytes:trailerBytesArray:nArgs:nTemps:nStack:nLits:primitive:!
!methodRemoval: CompiledMethod class #newBytes:trailerBytesTrailer:nArgs:nTemps:nStack:nLits:primitive:!
CompiledMethod class removeSelector: #newBytes:trailerBytesTrailer:nArgs:nTemps:nStack:nLits:primitive:!
!methodRemoval: CompiledMethod class #primitive:numArgs:numTemps:stackSize:literals:bytecodes:trailer:!
CompiledMethod class removeSelector: #primitive:numArgs:numTemps:stackSize:literals:bytecodes:trailer:!
!methodRemoval: CompiledMethod #blockExtentsToTempsMap!
CompiledMethod removeSelector: #blockExtentsToTempsMap!
!methodRemoval: CompiledMethod #copyWithTempNames:!
CompiledMethod removeSelector: #copyWithTempNames:!
!methodRemoval: CompiledMethod #copyWithTempsFromMethodNode:!
CompiledMethod removeSelector: #copyWithTempsFromMethodNode:!
!methodRemoval: CompiledMethod #copyWithTrailerBytesArray:!
CompiledMethod removeSelector: #copyWithTrailerBytesArray:!
!methodRemoval: CompiledMethod #copyWithTrailerBytesTrailer:!
CompiledMethod removeSelector: #copyWithTrailerBytesTrailer:!
!methodRemoval: CompiledMethod #decompileWithTemps!
CompiledMethod removeSelector: #decompileWithTemps!
!methodRemoval: CompiledMethod #holdsTempNames!
CompiledMethod removeSelector: #holdsTempNames!
!methodRemoval: CompiledMethod #qCompress:!
CompiledMethod removeSelector: #qCompress:!
!methodRemoval: CompiledMethod #qDecompressFrom:!
CompiledMethod removeSelector: #qDecompressFrom:!
!methodRemoval: CompiledMethod #tempNamesString!
CompiledMethod removeSelector: #tempNamesString!
!classRemoval: #CompiledMethodTrailer!
Smalltalk removeClassNamed: #CompiledMethodTrailer!
