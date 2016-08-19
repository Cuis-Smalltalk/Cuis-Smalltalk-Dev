'From Cuis 4.2 of 25 July 2013 [latest update: #2886] on 16 August 2016 at 11:16:40 am'!

!ZInstructionPrinter methodsFor: 'instruction decoding' stamp: 'jmv 8/16/2016 10:47:44'!
callPrimitive: index
	"Print the callPrimitive bytecode."

	self print: 'callPrimitive: ' , index printString! !


!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'jmv 8/16/2016 10:45:39'!
sizeCallPrimitive: primitiveIndex
	"Only for Spur!!"
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/16/2016 10:46:36'!
genCallPrimitive: primitiveIndex
	"Only for Spur!!"
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/16/2016 10:59:22'!
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	^ Smalltalk isSpur 
		ifTrue: [ self generateSpur: trailer using: aCompiledMethodClass ]
		ifFalse: [ self generatePreSpur: trailer using: aCompiledMethodClass ]! !

!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/16/2016 11:00:52'!
generatePreSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

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

!MethodNode methodsFor: 'code generation' stamp: 'jmv 8/16/2016 11:00:45'!
generateSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

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


!ZBytecodeEncoder methodsFor: 'opcode sizing' stamp: 'jmv 8/16/2016 10:45:33'!
sizeCallPrimitive: primitiveIndex
	"Only for Spur!!"
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !

!ZBytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 8/16/2016 10:51:00'!
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


!ZEncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 8/16/2016 10:46:40'!
genCallPrimitive: primitiveIndex
	"Only for Spur!!"
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/16/2016 11:00:56'!
generatePreSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

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
	stack := ZParseStack new init.
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

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/16/2016 11:00:48'!
generateSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

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
	stack := ZParseStack new init.
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

