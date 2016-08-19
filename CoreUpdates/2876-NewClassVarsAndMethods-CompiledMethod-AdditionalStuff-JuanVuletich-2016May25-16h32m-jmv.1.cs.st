'From Cuis 4.2 of 25 July 2013 [latest update: #2774] on 25 May 2016 at 4:33:04.809215 pm'!
!classDefinition: #CompiledMethod category: #'Kernel-Methods'!
ByteArray variableByteSubclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'LargeFrame SmallFrame SecondaryBytecodeSetEncoderClass PrimaryBytecodeSetEncoderClass '
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!CompiledMethod methodsFor: 'accessing' stamp: 'eem 8/5/2014 20:52'!
encoderClass
	"Answer the encoder class that encoded the bytecodes in this method.
	 The sign flag bit is used by the VM to select a bytecode set.  This formulation
	 may seem odd but this has to be fast, so no property probe unless needed."

	^self header >= 0
		ifTrue: 
			[PrimaryBytecodeSetEncoderClass]
		ifFalse:
			[PrimaryBytecodeSetEncoderClass == SecondaryBytecodeSetEncoderClass
				ifTrue: "Support for testing prior to installing another set"
					[(self propertyValueAt: #encoderClass) ifNil: [SecondaryBytecodeSetEncoderClass]]
				ifFalse:
					[SecondaryBytecodeSetEncoderClass]]! !

!CompiledMethod methodsFor: 'testing' stamp: 'md 11/21/2003 12:15'!
isCompiledMethod

	^ true! !

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 5/24/2016 23:52'!
copyWithTrailerBytesArray: bytes
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

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 5/24/2016 23:53'!
copyWithTrailerBytesTrailer: trailer
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start := self initialPC.
	end := self endPC.
	copy := trailer createMethod: end - start + 1 class: self class header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	copy postCopy.! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 5/23/2016 22:04'!
newBytes: numberOfBytes trailerBytesArray: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
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

!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 5/23/2016 22:05'!
newBytes: numberOfBytes trailerBytesTrailer: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| largeBit primBits |
	nTemps > 63 ifTrue:
		[^ self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 255 ifTrue:
		[^ self error: 'Cannot compile -- too many literals'].	
	largeBit := (nTemps + stackSize) > SmallFrame ifTrue: [1] ifFalse: [0].
	primBits := primitiveIndex <= 16r1FF
		ifTrue: [primitiveIndex]
		ifFalse: ["For now the high bit of primitive no. is in the 29th bit of header"
				primitiveIndex > 16r3FF ifTrue: [self error: 'prim num too large'].
				(primitiveIndex bitAnd: 16r1FF) + ((primitiveIndex bitAnd: 16r200) bitShift: 19)].

	^trailer
		createMethod: numberOfBytes
		class: self
		header: (nArgs bitShift: 24) +
				(nTemps bitShift: 18) +
				(largeBit bitShift: 17) +
				(nLits bitShift: 9) +
				primBits! !

!CompiledMethod class methodsFor: 'method encoding' stamp: 'eem 8/12/2014 12:39'!
headerFlagForEncoder: anEncoder
	anEncoder class == PrimaryBytecodeSetEncoderClass ifTrue:
		[^0].
	anEncoder class == SecondaryBytecodeSetEncoderClass ifTrue:
		[^SmallInteger minVal].
	self error: 'The encoder is not one of the two installed bytecode sets'! !

!classDefinition: #CompiledMethod category: #'Kernel-Methods'!
ByteArray variableByteSubclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'LargeFrame PrimaryBytecodeSetEncoderClass SecondaryBytecodeSetEncoderClass SmallFrame'
	poolDictionaries: ''
	category: 'Kernel-Methods'!
