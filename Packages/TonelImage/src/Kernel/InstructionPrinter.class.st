"
My instances can print the object code of a CompiledMethod in symbolic format. They print into an instance variable, stream, and uses oldPC to determine how many bytes to print in the listing. The variable method  is used to hold the method being printed.
"
Class {
	#name : 'InstructionPrinter',
	#superclass : 'InstructionClient',
	#instVars : [
		'method',
		'scanner',
		'stream',
		'oldPC',
		'innerIndents',
		'indent',
		'printPC',
		'indentSpanOfFollowingJump'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'printing' }
InstructionPrinter class >> on: aMethod [
	^self new method: aMethod.
	
]

{ #category : 'printing' }
InstructionPrinter class >> printClass: class  [
	"Create a file whose name is the argument followed by '.bytes'. Store on 
	the file the symbolic form of the compiled methods of the class."

	DirectoryEntry fileOutsDirQuery: (class name , '.bytes') writeStreamDo: [ :fileStream |
		class selectorsDo: [ :sel | 
			fileStream newLine; nextPutAll: sel; newLine.
			(self on: (class compiledMethodAt: sel)) printInstructionsOn: fileStream ]].

	"
	InstructionPrinter printClass: Parser.
	"
]

{ #category : 'instruction decoding' }
InstructionPrinter >> blockReturnConstant: value  [
	"Print the Return Constant From Block bytecode."

	self print: 'blockReturn: ', value printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> blockReturnTop [
	"Print the ReturnTopOfStack bytecode."
	self print: 'blockReturn'
]

{ #category : '*BytecodeSets-SistaV1-decoding' }
InstructionPrinter >> branchIfInstanceOf: behaviorOrArrayOfBehavior distance: distance [
	"If the object on top of stack has the type -or one of the type- present in the literal
	 (the literal is a behavior or an array of behavior), then pop it. Else jumps by the distance."
	self print: 'branchIfInstanceOf: ', behaviorOrArrayOfBehavior printString, ' distance: ', distance printString
]

{ #category : '*BytecodeSets-SistaV1-decoding' }
InstructionPrinter >> branchIfNotInstanceOf: behaviorOrArrayOfBehavior distance: distance [
	"If the object on top of stack has the type -or one of the type- present in the literal
	 (the literal is a behavior or an array of behavior), then pop it. Else jumps by the distance."
	self print: 'branchIfNotInstanceOf: ', behaviorOrArrayOfBehavior printString, ' distance: ', distance printString
]

{ #category : '*BytecodeSets-instruction decoding' }
InstructionPrinter >> callInlinePrimitive: index [
	"Print the callInlinePrimitive: bytecode.  This is the m = 1, ss = 0 case in SistaV1:	**	248 below.

	SistaV1:	**	248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
								m=1 means inlined primitive, no hard return after execution. 
								ss defines the unsafe operation set used to encode the operations. 
								(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are as yet used)
	 V3/Spur:		139		10001011	i i i i i i i i	jjjjjjjj		Call Primitive #iiiiiiii + (jjjjjjjj * 256)"

	self print: 'callInlinePrimitive: ' , ((#((1000 ' class')
										(1001 ' pointer numSlots')
										(1002 ' pointer basicSize')
										(1003 ' byte8Type format numBytes')
										(1004 ' short16Type format numSho')
										(1005 ' word32Type format numWord')
										(1006 ' doubleWord64Type format n')

										(1010 ' ensure number of bytes available')
										(1011 ' fixed pointer basicNew')

										(1020 ' identityHash (non-immediate, non-Behavior, has hash)')
										(1021 ' identityHash (SmallInteger)')
										(1022 ' identityHash (Character)')
										(1023 ' identityHash (SmallFloat64)')
										(1024 ' identityHash (Behavior, has hash?)')

										(1030 ' immediateAsInteger (Character)')
										(1031 ' immediateAsInteger (SmallFloat64)')

										(2000 ' SmallInteger #+')
										(2001 ' SmallInteger #-')
										(2002 ' SmallInteger #*')
										(2003 ' SmallInteger #/')
										(2004 ' SmallInteger #//')
										(2005 ' SmallInteger #\\')
										(2006 ' SmallInteger #quo:')

										(2011 ' Variable-sized pointers new (new:). Array, etc')
										(2012 ' Variable-sized byte new (new:). ByteArray, ByteString, etc')
										(2013 ' Variable-sized 16-bit new (new:). DoubleByteArray, etc')
										(2014 ' Variable-sized 32-bit new (new:). Bitmap, FloatArray, etc')
										(2015 ' Variable-sized 64-bit new (new:). DoubleWordArray, etc')

										(2016 ' SmallInteger #bitAnd:')
										(2017 ' SmallInteger #bitOr:')
										(2018 ' SmallInteger #bitXor:')
										(2019 ' SmallInteger #bitShiftLeft:')
										(2019 ' SmallInteger #bitShiftRight:')

										(2032 ' SmallInteger #>')
										(2033 ' SmallInteger #<')
										(2034 ' SmallInteger #>=')
										(2035 ' SmallInteger #<=')
										(2036 ' SmallInteger #=')
										(2037 ' SmallInteger #~=')

										(2064 ' Pointer Object>>at:')
										(2065 ' Byte Object>>at:')
										(2066 ' 16-bit Word Object>>at:')
										(2067 ' Word Object>>at:')
										(2068 ' DoubleWord Object>>at:')
										(2069 ' QuadWord Object>>at:')

										(3000 ' Pointer Object>>at:put:')
										(3001 ' Byte Object>>at:put:')
										(3002 ' Word Object>>at:put:')
										(3003 ' DoubleWord Object>>at:put')
										(3004 ' QuadWord Object>>at:put:'))
											detect: [:tuple| tuple first = index]
											ifNone: [{index printString}]) last)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> callPrimitive: index [
	"Print the callPrimitive bytecode."

	self print: 'callPrimitive: ' , index printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> directedSuperSend: selector "<Symbol>" numArgs: numArgs "<SmallInteger>" [
	self print: 'directedSuperSend: ' , (self stringForSelector: selector numArgs: numArgs)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> doDup [
	"Print the Duplicate Top of Stack bytecode."
	self print: 'dup'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> doNop [
	"Print the Do Nothing Operation bytecode."
	self print: 'nop'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> doPop [
	"Print the Remove Top of Stack bytecode."
	self print: 'pop'
]

{ #category : 'accessing' }
InstructionPrinter >> indent [

	^ indent ifNil: [0]
]

{ #category : 'initialization' }
InstructionPrinter >> indent: numTabs [

	indent := numTabs
]

{ #category : 'instruction decoding' }
InstructionPrinter >> jump: offset [
	"Print the Unconditional Jump bytecode."

	self print: 'jumpTo: ' , (scanner pc + offset) printString.
	indentSpanOfFollowingJump ifTrue:
		[indentSpanOfFollowingJump := false.
		 innerIndents atAll: (scanner pc to: scanner pc + offset - 1) put: (innerIndents at: scanner pc - 1) + 1]
]

{ #category : 'instruction decoding' }
InstructionPrinter >> jump: offset if: condition  [
	"Print the Conditional Jump bytecode."

	self print: 
		(condition
			ifTrue: ['jumpTrue: ']
			ifFalse: ['jumpFalse: '])
			, (scanner pc + offset) printString
]

{ #category : 'accessing' }
InstructionPrinter >> method [
	^method.
]

{ #category : 'accessing' }
InstructionPrinter >> method: aMethod [
	method :=  aMethod.
	printPC := true.
	indentSpanOfFollowingJump := false
]

{ #category : 'instruction decoding' }
InstructionPrinter >> methodReturnConstant: value  [
	"Print the Return Constant bytecode."
	self print: 'return: ' , value printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> methodReturnReceiver [
	"Print the Return Self bytecode."
	self print: 'returnSelf'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> methodReturnTop [
	"Print the Return Top of Stack bytecode."
	self print: 'returnTop'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> popIntoLiteralVariable: anAssociation  [
	"Print the Removes the Top of the Stack and Stores it into a Literal Variable
	bytecode."
	self print: 'popIntoLit: ' , anAssociation key
]

{ #category : 'instruction decoding' }
InstructionPrinter >> popIntoReceiverVariable: offset  [
	"Print the Remove Top Of Stack And Store Into Instance Variable bytecode."

	self print: 'popIntoRcvr: ' , (self stringForReceiverVariableOffset: offset)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self print: 'popIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> popIntoTemporaryVariable: offset  [
	"Print the Removes the Top of the Stack and Stores it into a Temporary Variable
	bytecode."
	self print: 'popIntoTemp: ' , offset printString
]

{ #category : 'printing' }
InstructionPrinter >> print: instruction  [
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	printPC ifTrue: [stream print: oldPC; space].
	stream tab: (innerIndents at: oldPC).
	stream nextPut: $<.
	oldPC to: scanner pc - 1 do: 
		[:i | 
		code := (method at: i) printStringBase: 16.
		stream nextPut: 
			(code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
		stream nextPut: code last; space].
	stream skipBack.
	stream nextPut: $>.
	stream space.
	stream nextPutAll: instruction.
	stream newLine.
	oldPC := scanner pc.
	"(InstructionPrinter compiledMethodAt: #print:) symbolic."

]

{ #category : 'initialization' }
InstructionPrinter >> printInstructionsOn: aStream  [
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream."
	
	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self]
]

{ #category : 'initialization' }
InstructionPrinter >> printInstructionsOn: aStream do: aBlock [
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream. Evaluate aBlock with the receiver, the scanner and
	 the stream after each instruction."

	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self.
		 aBlock value: self value: scanner value: stream]
]

{ #category : 'accessing' }
InstructionPrinter >> printPC [
	^printPC
]

{ #category : 'accessing' }
InstructionPrinter >> printPC: aBoolean [
	printPC := aBoolean
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushActiveContext [
	"Print the Push the Active Context on the Top of its Own Stack bytecode."
	self print: 'pushThisContext: '
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	self print: 'closureNumCopied: ', numCopied printString
			, ' numArgs: ', numArgs printString
			, ' bytes ', scanner pc printString
			, ' to ', (scanner pc + blockSize - 1) printString.
	innerIndents
		atAll: (scanner pc to: scanner pc + blockSize - 1)
		put: (innerIndents at: scanner pc - 1) + 1
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushConsArrayWithElements: numElements  [
	self print: 'pop ', numElements printString, ' into (Array new: ', numElements printString, ')'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushConstant: obj [
	"Print the Push Constant, obj, on Top Of Stack bytecode."

	self print: (String streamContents:
				[:s |
				s nextPutAll: 'pushConstant: '.
				obj isVariableBinding
					ifFalse: [obj printOn: s]
					ifTrue: [obj key
						ifNotNil: [s nextPutAll: '##'; nextPutAll: obj key]
						ifNil: [s nextPutAll: '###'; nextPutAll: obj value soleInstance name]]]).

	(obj is: #CompiledMethod) ifTrue:
		[obj longPrintOn: stream indent: self indent + 2.
		^self].
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushLiteralVariable: anAssociation [
	"Print the Push the Contents of anAssociation on the Top of the Stack bytecode."
	self print: 'pushLit: ' , anAssociation key
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushNewArrayOfSize: numElements  [
	self print: 'push: (Array new: ', numElements printString, ')'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushReceiver [
	"Print the Push the Active Context's Receiver on the Top of the Stack bytecode."
	self print: 'self'
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushReceiverVariable: offset [
	"Print the Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

	self print: 'pushRcvr: ' , (self stringForReceiverVariableOffset: offset)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex  [
	self print: 'pushTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> pushTemporaryVariable: offset [
	"Print the Push the Contents of the Temporary Variable whose Index is the
	argument, offset, on the Top of the Stack bytecode."
	self print: 'pushTemp: ' , offset printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> send: selector super: supered numArgs: numArgs [
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered
				ifTrue: ['superSend: ']
				ifFalse: ['send: '])
			, (self stringForSelector: selector numArgs: numArgs)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> storeIntoLiteralVariable: anAssociation  [
	"Print the Store the Top of the Stack into a Literal Variable of the Method bytecode."
	self print: 'storeIntoLit: ' , anAssociation key
]

{ #category : 'instruction decoding' }
InstructionPrinter >> storeIntoReceiverVariable: offset  [
	"Print the Store Top Of Stack Into Instance Variable Of Method bytecode."

	self print: 'storeIntoRcvr: ' , (self stringForReceiverVariableOffset: offset)
]

{ #category : 'instruction decoding' }
InstructionPrinter >> storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex  [
	self print: 'storeIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
]

{ #category : 'instruction decoding' }
InstructionPrinter >> storeIntoTemporaryVariable: offset  [
	"Print the Store the Top of the Stack into a Temporary Variable of the Method
	bytecode."
	self print: 'storeIntoTemp: ' , offset printString
]

{ #category : 'printing' }
InstructionPrinter >> stringForReceiverVariableOffset: offset [
	| tmpStream |
	tmpStream := WriteStream on: (String new: 16).
	offset printOn: tmpStream.
	method methodClass ifNotNil:
		[:class|
		class isBehavior ifTrue:
			[(class instVarNameForIndex: offset + 1) ifNotNil:
				[:instVarName|
				tmpStream nextPutAll: ' "'; nextPutAll: instVarName; nextPut: $"]]].
	^tmpStream contents
]

{ #category : 'printing' }
InstructionPrinter >> stringForSelector: selector numArgs: numArgs [
	^(selector isSymbol and: [selector numArgs = numArgs])
		ifTrue: [selector]
		ifFalse: [selector printString
				, (numArgs = 1
					ifTrue: [' (1 arg)']
					ifFalse: [' (', numArgs printString, ' args)'])]
]

{ #category : 'instruction decoding' }
InstructionPrinter >> trapIfNotInstanceOf: behaviorOrArrayOfBehavior [
	"If the top of stack is not an instance of either the argument, or, if the argument is an Array,
	  any of the elements of the argument, send the class trap message to the current context."
	self print: 'trapIfNotInstanceOf: ', behaviorOrArrayOfBehavior printString
]
