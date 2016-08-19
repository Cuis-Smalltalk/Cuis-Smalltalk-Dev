'From Cuis 4.2 of 25 July 2013 [latest update: #2898] on 8 August 2016 at 3:33:01 pm'!

!ZSyntaxErrorNotification commentStamp: 'nice 9/18/2013 22:16' prior: 0!
A ZSyntaxErrorNotification is an Exception occuring when compiling a Smalltalk source code with incorrect syntax.
Note that in interactive mode, this exception is not raised because the Compiler will interact directly with source code editor.
The defaultAction is to raise a SyntaxError pop up window so as to enable interactive handling even in non interactive mode.

Instance Variables
	category:		<String | nil>
	code:		<String | Text | Stream>
	doitFlag:		<Boolean>
	errorMessage:		<String>
	inClass:		<Behavior>
	location:		<Integer>
	newSource:		<String | Text | Stream | nil>

category
	- the category in which the method will be classified

code
	- the source code to be compiled or evaluated

doitFlag
	- true if this is a doIt (code to evaluate), false if this is a method (code of a method to be compiled)

errorMessage
	- contains information about the syntax error

inClass
	- target class in which to compile the method

location
	- position in the source code where the syntax error occured

newSource
	- eventually hold a source code replacement typically passed by the SyntaxError window
!


!ZReparseAfterSourceEditing commentStamp: '<historical>' prior: 0!
A ReparseAfterSourceEditing is a Notification used to restart the syntax parsing phase of a compilation after a source code edition.!


!ZScanner commentStamp: 'ul 4/3/2011 02:04' prior: 0!
I scan a string or text, picking out Smalltalk syntactic tokens. I look one character ahead. I put each token found into the instance variable, token, and its type (a Symbol) into the variable, tokenType. At the end of the input stream, I pretend to see an endless sequence of special characters called doIts.

Instance Variables
	aheadChar:		<Character>
	buffer:		<WriteStream>
	currentComment:		<OrderedCollection>
	hereChar:		<Character>
	mark:		<Integer>
	source:		<ReadStream>
	token:		<Symbol|String|NumberCharacter|Boolean|nil>
	tokenType:		<Symbol>
	typeTable:		<Array>

aheadChar
	- the next character in the input stream

buffer
	- a reusable WriteStream on a String which is used for building strings. Shouldn't be used from multiple methods without resetting.

currentComment
	- an OrderedCollection of strings which contain all comments between the current token and the previous token or the beginning of the source.

hereChar
	- the current character

mark
	- the position of the current token in the source stream

source
	- the input stream of characters

token
	- the current token

tokenType
	- the type of the current token. The possible token types are: #binary, #character, #colon, #doIt, #keyword, #leftArrow, #leftBrace, #leftBracket, #leftParenthesis, #literal, #period, #rightBrace, #rightBracket, #rightParenthesis, #semicolon, #string, #upArrow, #verticalBar, #word, #xBinary, #xColon, #xDelimiter, #xDigit, #xDollar, #xDoubleQuote, #xLetter, #xLitQuote, #xSingleQuote, #xUnderscore

typeTable
	- an array that maps each an evaluable tokenType to each character with asciiValue between 0 and 255!

!classDefinition: #ZParseNode category: #'Compiler-ParseNodes'!
Object subclass: #ZParseNode
	instanceVariableNames: 'comment pc '
	classVariableNames: 'Bfp BtpLong CodeBases CodeLimits DblExtDoAll Dup EndMethod EndRemote Jmp JmpLimit JmpLong LdFalse LdInstType LdLitIndType LdLitType LdMinus1 LdNil LdSelf LdSuper LdTempType LdThisContext LdTrue LoadLong NodeFalse NodeNil NodeSelf NodeSuper NodeThisContext NodeTrue Pop Send SendLimit SendLong SendLong2 SendPlus SendType ShortStoP StdLiterals StdSelectors StdVariables Store StorePop '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ZEncoderForV3 commentStamp: 'eem 5/17/2014 11:58' prior: 0!
I add behaviour to ZEncoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book.

	0-15 		0000iiii 	Push Receiver Variable #iiii
	16-31 		0001iiii 	Push Temporary Location #iiii
	32-63 		001iiiii 		Push Literal Constant #iiiii
	64-95 		010iiiii 		Push Literal Variable #iiiii
	96-103 	01100iii 	Pop and Store Receiver Variable #iii
	104-111 	01101iii 	Pop and Store Temporary Location #iii
	112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message
	124-125 	0111110i 	Return Stack Top From (Message, Block) [i]
	(126-127 unassigned)
	128 		10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	131 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	133 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments
	134 		10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	135 		10000111 	Pop Stack Top
	136 		10001000 	Duplicate Stack Top
	137 		10001001 	Push Active Context
	(138-143 unassigned)
	144-151 	10010iii 		Jump iii + 1 (i.e., 1 through 8)
	152-159 	10011iii 		Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj
	168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj
	176-191 	1011iiii 		Send Arithmetic Message #iiii
	192-207 	1100iiii 		Send Special Message #iiii
	208-223 	1101iiii 		Send Literal Selector #iiii With No Arguments
	224-239 	1110iiii 		Send Literal Selector #iiii With 1 Argument
	240-255 	1111iiii 		Send Literal Selector #iiii With 2 Arguments
!

!classDefinition: #ZSpecialSelectorNode category: #'Compiler-ParseNodes'!
ZSelectorNode subclass: #ZSpecialSelectorNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ZSpecialSelectorNode commentStamp: '<historical>' prior: 0!
A SpecialSelectorNode is a subclass of SelectorNode that handles the special selectors, a high static and/or dynamic frequency set of selectors that are assigned their own bytecodes.  Special selectors both save space in the literal frame and allow an interpreter to implement these sends directly for certain classes of receiver and argument, for example the SmallIntegers, a technique known as static receiver prediction.!


!ZInstructionClient methodsFor: 'instruction decoding' stamp: 'eem 8/8/2014 07:50'!
callPrimitive: pimIndex
	"V3PlusClosures:	139 10001011	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 NewsqueakV4:		249 11111001	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 SistaV1:			248 11111000 iiiiiiii mjjjjjjj  Call Primitive #iiiiiiii + ( jjjjjjj * 256)
							m=1 means inlined primitive, no hard return after execution."! !


!ZBlockLocalTempCounter class methodsFor: 'instance creation' stamp: 'eem 11/6/2012 15:23'!
tempCountForBlockStartingAt: startpc in: method
	^self new
		tempCountForBlockAt: (method encoderClass pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method)
		in: method! !


!ZBytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 10/23/2012 16:50'!
sizeCallPrimitive: primitiveIndex
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !

!ZBytecodeEncoder methodsFor: 'opcode sizing' stamp: 'ClementBera 4/2/2015 15:37'!
sizePushNClosureTemps: numTemps
	^self sizeOpcodeSelector: #genPushNClosureTemps: withArguments: {numTemps}! !

!ZBytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 4/3/2013 12:48'!
sizePushTempLong: tempIndex
	^self sizeOpcodeSelector: #genPushTempLong: withArguments: {tempIndex}! !

!ZBytecodeEncoder methodsFor: 'opcode sizing' stamp: 'eem 8/5/2014 18:29'!
sizeSendSpecial: specialSelectorIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSpecial:numArgs: withArguments: {specialSelectorIndex. nArgs}! !

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


!ZBytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/14/2012 12:09'!
callPrimitiveCode
	"Answer the call primitive bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil! !

!ZBytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 15:17'!
pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method
	"Answer the pc of the push closure bytecode whose block starts at startpc in method."
	^self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 11:02'!
pushClosureBytecodeSize
	"Answer the size of the push closure bytecode, if there is one."
	^self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 3/20/2013 16:46'!
unusedBytecode
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^nil! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:55'!
bindingReadScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:35'!
bindingWriteScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:48'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:31'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:31'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 4/9/2015 16:46'!
markerOrNilFor: aMethod
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	^self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:51'!
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:51'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."
	
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 18:53'!
bytecodeSize: aByte
	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:39'!
extensionsAt: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:41'!
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct jump decoder for the instruction set."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:40'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch instruction interpretation through the encoder
	 to select the correct instruction set decoder."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:43'!
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:32'!
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:32'!
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends
	 the range of the following bytecode."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:31'!
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:18'!
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
isRealSendAt: pc in: method
	"Answer whether the bytecode at pc is a real message-send, not blockCopy:."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:28'!
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 9/2/2014 14:08'!
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:29'!
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:34'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:37'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:39'!
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 21:04'!
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'scanning' stamp: 'eem 12/5/2012 15:45'!
method: method readsField: varIndex
	"Answer if method loads the instance variable indexed by varIndex."
	^self subclassResponsibility! !

!ZBytecodeEncoder class methodsFor: 'scanning' stamp: 'eem 12/5/2012 15:45'!
method: method writesField: varIndex
	"Answer if method stores into the instance variable indexed by varIndex."
	^self subclassResponsibility! !


!ZEncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 4/3/2013 12:50'!
genPushTempLong: tempIndex
	"See BlueBook page 596"
	(tempIndex >= 0 and: [tempIndex < 64]) ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63! !

!ZEncoderForV3 methodsFor: 'bytecode generation' stamp: 'eem 10/15/2012 16:10'!
genSendSpecial: specialSelectorIndex numArgs: nArgs
	"See BlueBook page 596"
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		176-191 	1011iiii 	Send Arithmetic Message #iiii
		192-207 	1100iiii 	Send Special Message #iiii"
	stream nextPut: specialSelectorIndex + 175! !


!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:34'!
bindingReadScanBlockFor: bindingLitIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 64
	   and: [b <= 95
			ifTrue: [b - 64 = bindingLitIndex]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte - 192 = bindingLitIndex]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 128 and: 159)
						 and: [scanner thirdByte = bindingLitIndex]]]]]]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:34'!
bindingWriteScanBlockFor: bindingLitIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index bindingLitIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	  (b = 129 or: [b = 130])
		ifTrue: [scanner followingByte - 192 = bindingLitIndex]
		ifFalse:
			[b = 132
			 and: [scanner followingByte >= 224
			 and: [scanner thirdByte = bindingLitIndex]]]]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:49'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	^[:b| false]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:30'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b < 16
		ifTrue: [b = varIndexCode]
		ifFalse:
			[b = 128
				ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
				ifFalse:
					[b = 132
					 and: [(scanner followingByte between: 64 and: 95)
					 and: [scanner thirdByte = varIndexCode]]]]]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 10:30'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 96
	   and: [b <= 103
			ifTrue: [b - 96 = varIndexCode]
			ifFalse:
				[(b = 129 or: [b = 130])
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 160 and: 223)
						 and: [scanner thirdByte = varIndexCode]]]]]]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 4/9/2015 16:48'!
markerOrNilFor: aMethod
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	| e |
	^(((e := aMethod endPC) = 19 or: [e = 20])
	  and: [aMethod numLiterals = 3
	  and: [(aMethod at: 17) = 16r70	"push self"
	  and: [(aMethod at: 18) = 16rD0]]])	"send <literal 1>"
		ifTrue: [aMethod literalAt: 1]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:52'!
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	^[:instr |
	   instr = 16r85
	   or: [instr = 16r84
		and: [scanner followingByte between: 16r20 and: 16r3F]]]! !

!ZEncoderForV3 class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:52'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^false! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/17/2014 17:12'!
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
		[^#(2 2 2 2 3 2 2 1 1 1 nil nil nil nil nil nil) at: bytecode - 127].
	^nil! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:39'!
extensionsAt: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 10/8/2014 13:41'!
extensionsFor: pc in: aCompiledMethod into: trinaryBlock
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:32'!
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretV3JumpIfCond! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:28'!
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct jump interpreter for the instruction set."
	^anInstructionStream interpretV3Jump! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:20'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextV3InstructionFor: aClient! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:43'!
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."

	^(method at: pc) = 125! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:34'!
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 152 and: 159) or: [bytecode between: 172 and: 175]! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:33'!
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."
	^(method at: pc) between: 168 and: 171! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/17/2014 17:59'!
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends the
	 range of the following bytecode.  The Smalltalk-80/V3 sets don't use extensions."
	^false! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:36'!
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 144 and: 151) or: [bytecode between: 160 and: 167]! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:28'!
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	^(method at: pc) = 135	"135 		10000111 	Pop Stack Top"! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:01'!
isRealSendAt: pc in: method
	"Answer whether the bytecode at pc is a real message-send, not blockCopy:."

	| byte |
	byte := method at: pc.
	^(byte >= 176			"special send or short send"
	   and: [byte ~= 200	"special selector blockCopy:"
			or: [(Smalltalk specialSelectorAt: 200 - 175) ~~ #blockCopy:]])
	 or: [byte >= 131
		 and: [byte <= 134	"long sends"	
		 and: [byte ~= 132	"double extended do anything"
			or: [(method at: pc + 1) // 32 <= 1]]]]! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:11'!
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return."

	^(method at: pc) between: 120 and: 125! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 9/2/2014 14:10'!
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	^(method at: pc) = 124! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:00'!
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	| byte |
	byte := method at: pc.
	^byte >= 176			"special send or short send"
	 or: [byte >= 131
		 and: [byte <= 134	"long sends"	
		 and: [byte ~= 132	"double extended do anything"
			or: [(method at: pc + 1) // 32 <= 1]]]]! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:14'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 132)
		and: [byte <= 111				"96 103	storeAndPopReceiverVariableBytecode"
										"104 111	storeAndPopTemporaryVariableBytecode"
			or: [byte >= 129			"129		extendedStoreBytecode"
				and: [byte <= 130		"130		extendedStoreAndPopBytecode"
					or: [byte = 132		"132		doubleExtendedDoAnythingBytecode"
						and: [(method at: pc+1) >= 160]]]]]! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:46'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 111)	"96 103	storeAndPopReceiverVariableBytecode"
									"104 111	storeAndPopTemporaryVariableBytecode"
	  or: [byte = 130]				"130		extendedStoreAndPopBytecode"! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 11:28'!
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	^method at: pc! !

!ZEncoderForV3 class methodsFor: 'instruction stream support' stamp: 'eem 8/5/2014 21:03'!
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	| byte byte2 |
	byte := method at: pc.
	byte < 131 ifTrue: [^anInstructionStream].
	byte >= 176
		ifTrue: 
			["special byte or short send"
			byte >= 208
				ifTrue: [^method literalAt: (byte bitAnd: 15) + 1]
				ifFalse: [^Smalltalk specialSelectorAt: byte - 176 + 1]]
		ifFalse: 
			[byte <= 134 ifTrue: 
				[byte2 := method at: pc + 1.
				 byte = 131 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 132 ifTrue: [byte2 < 64 ifTrue: [^method literalAt: (method at: pc + 2) + 1]].
				 byte = 133 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 134 ifTrue: [^method literalAt: byte2 \\ 64 + 1]]].
	^anInstructionStream! !

!ZEncoderForV3 class methodsFor: 'scanning' stamp: 'eem 12/5/2012 16:08'!
method: method readsField: varIndex
	"Answer if method loads the instance variable indexed by varIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available.
	0-15 		0000iiii 	Push Receiver Variable #iiii
	128 		10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)"
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	method isReturnField ifTrue:
		[^method returnField = varIndexCode].
	^(scanner := InstructionStream on: method) scanFor:
		[:b|
		b < 16
			ifTrue: [b = varIndexCode]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 64 and: 95)
						 and: [scanner thirdByte = varIndexCode]]]]]! !

!ZEncoderForV3 class methodsFor: 'scanning' stamp: 'eem 12/5/2012 16:10'!
method: method writesField: varIndex
	"Answer if method stores into the instance variable indexed by varIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available.
	96-103 	01100iii 	Pop and Store Receiver Variable #iii
	129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)"
	| varIndexCode scanner |
	method isQuick ifTrue: [^false].
	varIndexCode := varIndex - 1.
	^(scanner := InstructionStream on: method) scanFor:
		[:b|
		b >= 96
		and: [b <= 103
				ifTrue: [b - 96 = varIndexCode]
				ifFalse:
					[(b = 129 or: [b = 130])
						ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 160 and: 223)
							 and: [scanner thirdByte = varIndexCode]]]]]]! !

!ZEncoderForV3 class methodsFor: 'bytecode decoding' stamp: 'eem 3/20/2013 16:46'!
unusedBytecode
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^126! !


!ZEncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 8/8/2014 08:14'!
genCallPrimitive: primitiveIndex
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !

!ZEncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'eem 5/29/2015 15:21'!
genPushNClosureTemps: numTemps
	numTemps timesRepeat: [self genPushSpecialLiteral: nil]! !


!ZEncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 8/8/2014 09:14'!
callPrimitiveCode
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	^139! !

!ZEncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 15:19'!
pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method
	"Answer the pc of the push closure bytecode whose block starts at startpc in method.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^startpc - 4! !

!ZEncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 11/6/2012 11:03'!
pushClosureBytecodeSize
	"Answer the size of the push closure bytecode.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^4! !

!ZEncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 8/8/2014 08:05'!
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

!ZEncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/12/2014 18:19'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextV3ClosuresInstructionFor: aClient! !

!ZEncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:33'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 142)
		and: [byte <= 111				"96 103	storeAndPopReceiverVariableBytecode"
										"104 111	storeAndPopTemporaryVariableBytecode"
			or: [byte >= 129			"129		extendedStoreBytecode"
				and: [byte <= 130		"130		extendedStoreAndPopBytecode"
					or: [(byte = 132	"132		doubleExtendedDoAnythingBytecode"
						and: [(method at: pc+1) >= 160])
					or: [byte = 141		"141		storeRemoteTempLongBytecode"
					or: [byte = 142		"142		storeAndPopRemoteTempLongBytecode"]]]]]]! !

!ZEncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 12:45'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 111)	"96 103	storeAndPopReceiverVariableBytecode"
									"104 111	storeAndPopTemporaryVariableBytecode"
	  or: [byte = 130				"130		extendedStoreAndPopBytecode"
	  or: [byte = 142]]				"142		storeAndPopRemoteTempLongBytecode"! !

!ZEncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 12:49'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	^[ :bc | bc = 143]! !

!ZEncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'eem 5/17/2014 11:52'!
supportsClosures
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^true! !


!ZLeafNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Default is to do nothing.
	 Subclasses may need to override."! !

!ZLeafNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:34'!
sizeCodeForLoad: encoder forValue: forValue
	"Default is to do nothing.
	 Subclasses may need to override."
	^0! !


!ZLiteralNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 13:34'!
reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue:
		[index := key isVariableBinding "true if sending value[:] to a special binding"
					ifTrue: [encoder sharableLitIndex: key]
					ifFalse: [encoder litIndex: key].
		 code := self code: index type: 0 - code]! !


!ZSpecialSelectorNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:31'!
emitCode: stack args: nArgs encoder: encoder super: supered
	"Generate a special selector send.
	 A super send of a special selector must be handled like a normal send."
	supered
		ifTrue:
			[super emitCode: stack args: nArgs encoder: encoder super: supered]
		ifFalse:
			[stack pop: nArgs.
			 encoder genSendSpecial: code numArgs: nArgs]! !

!ZSpecialSelectorNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:32'!
sizeCode: encoder args: nArgs super: supered
	"Size a special selector send.
	 A super send of a special selector must be handled like a normal send."
	^supered
		ifTrue: [super sizeCode: encoder args: nArgs super: supered]
		ifFalse: [encoder sizeSendSpecial: code numArgs: nArgs]! !


!ZVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Do nothing"! !


!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:37'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	"If a normal literal variable (not sending value:), do nothing.
	 If for value (e.g. v := Binding := expr) do nothing; the work will be done in emitCodeForStore:encoder:.
	 If not for value then indeed load.  The rest of the work will be done in  emitCodeForStorePop:encoder:."
	(writeNode isNil or: [forValue]) ifTrue: [^self].
	encoder genPushLiteral: index.
	stack push: 1! !

!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:37'!
sizeCodeForLoad: encoder forValue: forValue
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding and: [forValue not]])
		ifTrue: [encoder sizePushLiteral: index]
		ifFalse: [0]! !


!ZTempVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:29'!
emitCodeForLoad: stack forValue: forValue encoder: encoder
	remoteNode ~~ nil ifTrue:
		[remoteNode emitCodeForLoadFor: self stack: stack encoder: encoder]! !

!ZTempVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForLoad: encoder forValue: forValue
	^remoteNode
		ifNil: [0]
		ifNotNil: [remoteNode sizeCodeForLoadFor: self encoder: encoder]! !


!ZMessageNode methodsFor: 'printing' stamp: 'nice 2/3/2011 20:57'!
printRepeatOn: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array new)
		on: aStream indent: level! !

!ZMessageNode methodsFor: 'code generation' stamp: 'nice 2/3/2011 21:12'!
emitCodeForRepeat: stack encoder: encoder value: forValue 
	" L1: ... Jmp(L1)"
	| loopSize |
	loopSize := sizes at: 1.
	receiver emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]! !

!ZMessageNode methodsFor: 'code generation' stamp: 'eem 8/5/2014 18:25'!
sizeCodeForRepeat: encoder value: forValue 
	"L1: ... Jmp(L1) nil (nil for value only);"
	| loopSize |
	"We assume long backward branches are always maximal size branches."
	loopSize := (receiver sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	sizes := Array with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 11/13/2012 09:55'!
transformRepeat: encoder
	"answer true if this #repeat message can be optimized"
	
	^(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0)
	   and: [receiver noteOptimizedIn: self.
			true]! !


!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/8/2016 13:34:16'!
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	^ Smalltalk isSpur 
		ifTrue: [ self generateSpur: trailer using: aCompiledMethodClass ]
		ifFalse: [ self generatePreSpur: trailer using: aCompiledMethodClass ]! !

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/4/2016 16:33:55'!
generate: trailer using: aCompiledMethodClass ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]])
		ifFalse: [^ self].
	v := block code.
	v < 0
		ifTrue: [^ self].
	v = LdSelf
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3)
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType))
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]! !

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/4/2016 16:44:27'!
generatePreSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

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

!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 8/4/2016 16:44:40'!
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


!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'eem 3/12/2013 13:30'!
decodeLiteralVariableValueDereferenceWithReceiver: receiver selector: selector arguments: arguments
	| varNode |
	(receiver notNil "cascades"
	 and: [receiver isLiteralNode
	 and: [receiver key isVariableBinding]]) ifFalse:
		[^nil].
	varNode := self codeAnyLitInd: receiver key.
	selector = #value ifTrue:
		[^varNode].
	^selector = #value: ifTrue:
		[self codeAssignTo: varNode value: arguments first]! !


!ZDecompiler methodsFor: 'control' stamp: 'jmv 8/8/2016 11:01:29'!
doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block |
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := ZBlockLocalTempCounter tempCountForBlockStartingAt: pc in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 'arg', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 'temp', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[self interpretNextInstructionFor: self.
		 stack removeLast].
	tempVarCount := tempVarCount + numArgs + numLocalTemps.
	block := self blockTo: jump.
	stack addLast: ((constructor
						codeArguments: (tempVars copyFrom: 1 to: numArgs)
						temps: (tempVars copyFrom: blockTempsOffset + 1 to: blockTempsOffset + numLocalTemps)
						block: block)
							pc: startpc;
							yourself).
	tempVars := savedTemps.
	tempVarCount := savedTempVarCount.
	numLocalTemps := savedNumLocalTemps! !

!ZDecompiler methodsFor: 'instruction decoding' stamp: 'nice 4/5/2013 22:21'!
pushTemporaryVariable: offset

	stack addLast: (offset >= tempVars size
		ifTrue:
			["Handle the case of chained LiteralVariableBinding assigments"
			stack at: (offset + 1 - tempVars size)]
		ifFalse:
			["A regular argument or temporary"
			tempVars at: offset + 1])! !

!ZDecompiler methodsFor: 'private' stamp: 'jmv 8/8/2016 11:12:11'!
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| leaveOnStack initStmt toDoStmt limitStmt |
	leaveOnStack := false.
	(stack notEmpty
	 and: [stack last isAssignmentNode])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast.
			 statements removeLast; addLast: toDoStmt.
			 leaveOnStack := true]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue:
		[leaveOnStack ifTrue:
			[stack addLast: statements removeLast].
			 ^self].
	limitStmt := statements last.
	((limitStmt isMemberOf: ZAssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: ZAssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[leaveOnStack ifTrue:
						[stack addLast: statements removeLast].
					^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]! !

!ZDecompiler methodsFor: 'public access' stamp: 'jmv 8/8/2016 11:06:12'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
"Comparar contra Decompiler, etc..."
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method primitive ~= 0 and: [self skipCallPrimitive; willStore]) ifTrue:
				[pc := pc + (method encoderClass bytecodeSize: self firstByte).
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method primitive > 0 ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen! !

!ZDecompiler methodsFor: 'public access' stamp: 'jmv 7/31/2016 15:34:37'!
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

!ZCompiler methodsFor: 'public access' stamp: 'jmv 8/8/2016 11:24:14'!
sourceStreamGetter: aSymbol
	"Cuis specific. Do not remove!!"
	sourceStreamGetter _ aSymbol! !


!ZScanner methodsFor: 'expression types' stamp: 'jmv 8/4/2016 19:10:06'!
scanLitVec
	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]] whileFalse:
		[tokenType == #leftParenthesis
			ifTrue: 
				[self scanToken; scanLitVec]
			ifFalse: 
				[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
					ifTrue: 
						[self scanLitWord.
						token == #true ifTrue: [token := true].
						token == #false ifTrue: [token := false].
						token == #nil ifTrue: [token := nil]]
					ifFalse:
						[(token == #- 
						  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
							[self scanToken.
							 token := token negated]]].
		s nextPut: token.
		self scanToken].
	token := s contents! !

!ZScanner methodsFor: 'expression types' stamp: 'ul 3/27/2011 22:17'!
scanStringStruct

	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]]
		whileFalse: 
			[tokenType == #leftParenthesis
				ifTrue: 
					[self scanToken; scanStringStruct]
				ifFalse: 
					[tokenType == #word ifFalse:
						[^self error: 'only words and parens allowed']].
			s nextPut: token.
			self scanToken].
	token := s contents! !

!ZScanner methodsFor: 'public access' stamp: 'ul 3/27/2011 23:37'!
scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: (ReadStream on: stringOrArray asString).
	strm := WriteStream on: (Array new: 10).
	[tokenType == #doIt]
		whileFalse: 
			[tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"! !

!ZScanner methodsFor: 'public access' stamp: 'ul 3/27/2011 22:16'!
scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: (ReadStream on: sourceString asString).
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll! !

!ZScanner methodsFor: 'public access' stamp: 'ul 3/27/2011 23:39'!
typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings. This methiod accepts _ (underscore) as an assignment token
	 irrespective of whether the system prefers := as the assignment token."
	| s |
	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asString).
	s := WriteStream on: (Array new: 16).
	[tokenType == #doIt] whileFalse:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
			[self scanToken.
			 token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'ul 3/27/2011 23:32'!
xColon
	"Allow := for assignment"
	
	aheadChar == $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol! !

!ZScanner methodsFor: 'multi-character scans' stamp: 'jmv 8/8/2016 11:41:21'!
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar == $' 
		and: [aheadChar == $' 
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar = ZScanner doItCharacterValue asCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched string quote']].
	self step.
	token := buffer contents.
	tokenType := #string! !


!ZParseNode class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:59:33'!
initClassCachedState
	"
	ParseNode initialize. VariableNode initialize
	"
	LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
	SendType := 5.
	"Back in the day the following constants corresponded to bytecodes.
	 Now they're just unique values that need to be eliminated when and if
	 things like code:type: are cleaned up."
	CodeBases := #(0 16 32 64 208 ).
	CodeLimits := #(16 16 32 32 16 ).
	LdSelf := 112.
	LdTrue := 113.
	LdFalse := 114.
	LdNil := 115.
	LdMinus1 := 116.
	LoadLong := 128.
	LdSuper := 133.
	Pop := 135.
	LdThisContext := 137.
	EndRemote := 125.
	SendPlus := 176.
	Send := 208.
	
	"((ParseNode class >> #initialize) literals select: [:l| l isVariableBinding and: [(ParseNode classPool includesAssociation: l) and: [(self systemNavigation allCallsOn: l localTo: ParseNode) size = 1]]]) sort: [:a :b| a key <= b key]"

	"ParseNode classPool associations select: [:a| (self systemNavigation allCallsOn: a localTo: ParseNode) isEmpty]"! !

!ZParseNode class methodsFor: 'cached class state' stamp: 'jmv 7/31/2016 20:10:23'!
releaseClassCachedState

	LdInstType _ LdTempType _ LdLitType _ LdLitIndType _ SendType _ nil.
	CodeBases _ CodeLimits _ nil.
	LdSelf _ LdTrue _ LdFalse _ nil.
	LdNil _ LdMinus1 _ LoadLong _ nil.
	LdSuper _ Pop _ nil.
	LdThisContext _ EndRemote _ nil.
	SendPlus _ Send _ nil! !


!ZEncoder methodsFor: 'encoding' stamp: 'cwp 12/27/2012 11:40'!
encodeSelector: aSelector

	^self
		name: aSelector
		key: aSelector
		class: ZSelectorNode
		type: SendType
		set: selectorSet! !


!ZAssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:30'!
emitCodeForEffect: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: false encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStorePop: stack encoder: encoder! !

!ZAssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:30'!
emitCodeForValue: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: true encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStore: stack encoder: encoder! !

!ZAssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForEffect: encoder

	^(variable sizeCodeForLoad: encoder forValue: false)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStorePop: encoder)! !

!ZAssignmentNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 12:35'!
sizeCodeForValue: encoder

	^(variable sizeCodeForLoad: encoder forValue: true)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStore: encoder)! !


!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'eem 7/8/2015 17:06'!
emitCodeForEvaluatedClosureValue: stack encoder: encoder
	| position |
	position := stack position.
	stack position: arguments size + temporaries size + copiedValues size.
	encoder genPushNClosureTemps: temporaries size.
	self
		reindexingLocalsDo: [self emitCodeForEvaluatedValue: stack encoder: encoder]
		encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		 pc := encoder methodStreamPosition].
	stack position: position! !

!ZBlockNode methodsFor: 'code generation (closures)' stamp: 'eem 5/29/2015 15:28'!
sizeCodeForEvaluatedClosureValue: encoder
	"The closure value primitives push the arguments and the copied values.
	 The compiler guarantees that any copied values come before all local temps.
	 So on closure activation we only need to push nils for the remaining temporaries."
	^(encoder sizePushNClosureTemps: temporaries size)
	+ (self
		reindexingLocalsDo: [self sizeCodeForEvaluatedValue: encoder]
		encoder: nil "don't store temps yet")
	+ (self returns ifTrue: [0] ifFalse: [encoder sizeReturnTopToCaller])! !


!ZVariableNode methodsFor: 'testing' stamp: 'jmv 8/4/2016 16:22:14'!
index
	"If index is nil, this code attempts to reconstruct the index from its encoding in code."
	index ifNotNil:
		[^index].
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[self assert: index = (code \\ 256).
		^code \\ 256].
	code >= (CodeBases at: self varNodeType) ifTrue:
		[self assert: index = (code - (CodeBases at: self varNodeType)).
		^code - (CodeBases at: self varNodeType)].
	self assert: index = (code - self varNodeType).
	^code - self varNodeType! !

!ZVariableNode methodsFor: 'tiles' stamp: 'jmv 8/4/2016 16:23:28'!
variableGetterBlockIn: aContext

	| temps tempIndex ivars |
	(self varNodeType = 4 and: [self key isVariableBinding]) ifTrue: [
		^[self key value]
	].
	aContext ifNil: [^nil].
	self isSelfPseudoVariable ifTrue: [^[aContext receiver]].
	self varNodeType = 1 ifTrue: [
		ivars := aContext receiver class allInstVarNames.
		tempIndex := ivars indexOf: self name ifAbsent: [^nil].
		^[aContext receiver instVarAt: tempIndex]
	].
	self varNodeType = 2 ifTrue: [
		temps := aContext tempNames.
		tempIndex := temps indexOf: self name ifAbsent: [^nil].
		^[aContext tempAt: tempIndex]
	].
	^nil
! !


!ZVariableNode class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 13:03:07'!
initClassCachedState
	"
	ZVariableNode initialize. ZDecompiler initialize
	"
	| encoder |
	encoder := ZEncoder new.
	StdVariables := Dictionary new: 16.
	encoder
		fillDict: StdVariables
		with: ZVariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary new: 64.
	encoder
		fillDict: StdSelectors
		with: ZSpecialSelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (1 to: Smalltalk specialSelectorSize) asArray.
	StdLiterals := ZLiteralDictionary new: 16.
	encoder
		fillDict: StdLiterals
		with: ZLiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !


!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/12/2013 16:05'!
emitCodeForStore: stack encoder: encoder
	| exprOffset |
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop.
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver.  If they
	 did we could generate much simpler code, e.g.
		encoder genPushLiteral: index.
		stack push: 1.
		writeNode emitCode: stack args: 1 encoder: encoder super: false"
	exprOffset := stack position - 1.
	encoder genPushLiteral: index.
	stack push: 1.
	encoder genPushTempLong: exprOffset.
	stack push: 1.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop! !

!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'nice 4/5/2013 02:20'!
emitCodeForStorePop: stack encoder: encoder
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop! !

!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'eem 4/3/2013 13:14'!
sizeCodeForStore: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialWriteBinding]) ifFalse:
		[^encoder sizeStoreLiteralVar: index].
	writeNode := encoder encodeSelector: #value:.
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop."
	^(encoder sizePushLiteral: index)
	  + (encoder sizePushTempLong: 0) "we don't know yet, hence long, sigh..."
	  + (writeNode sizeCode: encoder args: 1 super: false)
	  + encoder sizePop! !

!ZLiteralVariableNode methodsFor: 'code generation' stamp: 'nice 4/5/2013 02:29'!
sizeCodeForStorePop: encoder
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding])
		ifTrue: [	writeNode := encoder encodeSelector: #value:.
				^ (writeNode sizeCode: encoder args: 1 super: false)
	  			+ encoder sizePop]
		ifFalse: [encoder sizeStorePopLiteralVar: index]! !


!ZTempVariableNode methodsFor: 'code generation (closures)' stamp: 'topa 7/6/2015 17:22'!
definingScope: scopeBlock "<ZBlockNode>"
	definingScope = scopeBlock ifTrue: [^ self]. "No need to bail"
	definingScope ifNotNil:
		[self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock! !


!ZMessageNode methodsFor: 'code generation' stamp: 'nice 2/20/2012 20:50'!
emitCodeForToDo: stack encoder: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt limitInit test block incStmt blockSize |
	initStmt := arguments at: 4.
	limitInit := arguments at: 7.
	test := arguments at: 5.
	block := arguments at: 3.
	incStmt := arguments at: 6.
	blockSize := sizes at: 1.
	loopSize := sizes at: 2.
	limitInit == nil
		ifFalse: [limitInit emitCodeForEffect: stack encoder: encoder].
		
	"This will return the receiver of to:do: which is the initial value of the loop"
	forValue
		ifTrue: [initStmt emitCodeForValue: stack encoder: encoder.]
		ifFalse: [initStmt emitCodeForEffect: stack encoder: encoder].
	test emitCodeForValue: stack encoder: encoder.
	self emitCodeForBranchOn: false dist: blockSize pop: stack encoder: encoder.
	pc := encoder methodStreamPosition.
	block emitCodeForEvaluatedEffect: stack encoder: encoder.
	incStmt emitCodeForEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.! !

!ZMessageNode methodsFor: 'code generation' stamp: 'nice 2/20/2012 20:49'!
sizeCodeForToDo: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := Array with: blockSize with: loopSize.
	^initSize
	+ loopSize! !

!ZMessageNode methodsFor: 'code generation' stamp: 'eem 11/5/2012 16:02'!
sizeCodeForWhile: encoder value: forValue 
	"L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only);
	justStmt, wholeLoop, justJump."
	| cond stmt stmtSize loopSize branchSize |
	cond := receiver.
	stmt := arguments at: 1.
	"We assume long backward branches are always maximal size branches."
	stmtSize := (stmt sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	branchSize := self
					sizeCode: encoder
					forBranchOn: selector key == #whileFalse:  "Btp for whileFalse"
					dist: stmtSize.
	loopSize := (cond sizeCodeForEvaluatedValue: encoder) + branchSize + stmtSize.
	sizes := Array with: stmtSize with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:20'!
transformBoolean: encoder
	^self
		checkBlock: (arguments at: 1)
		as: 'argument'
		from: encoder
		maxArgs: 0! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:22'!
transformCase: encoder

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: ZBraceNode) ifFalse: [^false].
	(caseNode blockAssociationCheck: encoder) ifFalse: [^false].
	(arguments size = 1
	 or: [self checkBlock: arguments last as: 'otherwise arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].
	 caseNode elements do:
		[:messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self].
	 arguments size = 2 ifTrue:
		[arguments last noteOptimizedIn: self].
	 ^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:23'!
transformIfFalseIfTrue: encoder
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:25'!
transformIfNil: encoder

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := ZSelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := Array
						with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self)
						with: (arguments first noteOptimizedIn: self).
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:25'!
transformIfNilIfNotNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder maxArgs: 0)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := ZAssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := ZMessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 4/30/2012 16:26'!
transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := ZAssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := ZSelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := ZMessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:23'!
transformIfTrueIfFalse: encoder
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 6/19/2015 08:22'!
transformToDo: encoder
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange blockRange limitIsAssignedTo |
	block := arguments last.
	"First check for valid arguments"
	(block notNil
	 and: [block isBlockNode
	 and: [block numberOfArguments = 1
	 and: [block firstArgument isVariableReference "As with debugger remote vars"]]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	(limit := arguments at: 1) isVariableReference ifTrue:
		[limitIsAssignedTo := false.
		 block nodesDo:
			[:node|
			(node isAssignmentNode and: [node variable = limit]) ifTrue:
				[limitIsAssignedTo := true]].
		 limitIsAssignedTo ifTrue:
			[^false]].
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := ZSelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	blockRange := encoder rawSourceRanges at: block ifAbsent: [1 to: 0].
	blockVar := block firstArgument.
	initStmt := ZAssignmentNode new variable: blockVar value: receiver.
	limit isVariableReference | limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := ZAssignmentNode new
							variable: limit
							value: arguments first].
	test := ZMessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: {limit}
				precedence: precedence
				from: encoder
				sourceRange: (myRange first to: blockRange first).
	incStmt := ZAssignmentNode new
				variable: blockVar
				value: (ZMessageNode new
							receiver: blockVar selector: #+
							arguments: {increment}
							precedence: precedence
							from: encoder
							sourceRange: (myRange last to: (myRange last max: blockRange last)))
				from: encoder
				sourceRange: (myRange last to: (myRange last max: blockRange last)).
	arguments := {limit. increment. block. initStmt. test. incStmt. limitInit}.
	block noteOptimizedIn: self.
	^true! !

!ZMessageNode methodsFor: 'macro transformations' stamp: 'eem 8/5/2014 18:24'!
transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := ZSelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((ZBlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]! !

!ZMessageNode methodsFor: 'initialization' stamp: 'jmv 8/4/2016 15:20:55'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !


!ZMessageNode class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:52:31'!
initClassCachedState
	"
	MessageNode initialize
	"
	MacroSelectors := 
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:
			repeat ).
	MacroTransformers := 
		#(	transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase:
			transformIfNil: transformIfNil:  transformIfNilIfNotNil: transformIfNotNilIfNil:
			transformRepeat: ).
	MacroEmitters := 
		#(	emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForToDo:encoder:value: emitCodeForToDo:encoder:value:
			emitCodeForCase:encoder:value: emitCodeForCase:encoder:value:
			emitCodeForIfNil:encoder:value: emitCodeForIfNil:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForRepeat:encoder:value:).
	MacroSizers := 
		#(	sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value:
			sizeCodeForToDo:value: sizeCodeForToDo:value:
			sizeCodeForCase:value: sizeCodeForCase:value:
			sizeCodeForIfNil:value: sizeCodeForIfNil:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForRepeat:value:).
	MacroPrinters := 
		#(	printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent:
			printIfNil:indent: printIfNil:indent: printIfNilNotNil:indent: printIfNilNotNil:indent:
			printRepeatOn:indent:)! !


!ZMethodNode methodsFor: 'code generation' stamp: 'jmv 7/31/2016 16:07:58'!
generate
	"The receiver is the root of a parse tree. Answer a CompiledMethod."

	^self generate: #(0 0 0 0)! !

!ZMethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:00'!
generate: trailer 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod! !

!ZMethodNode methodsFor: 'code generation' stamp: 'eem 9/13/2011 15:01'!
generate: trailer ifQuick: methodBlock
	^self generate: trailer using: CompiledMethod ifQuick: methodBlock! !

!ZMethodNode methodsFor: 'printing' stamp: 'jmv 8/4/2016 16:46:58'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', ((interpreterClass classPool at: #PrimitiveTable) at: primitive + 1), '" ']! !


!ZDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 8/8/2016 10:55:48'!
codeMessage: receiver selector: selector arguments: arguments
	| symbol |
	symbol := selector key.
	(ZBraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	(self
		decodeLiteralVariableValueDereferenceWithReceiver: receiver
		selector: symbol
		arguments: arguments) ifNotNil: [:node| ^node].
	(self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	^ZMessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence! !


!ZDecompilerConstructorForClosures methodsFor: 'constructor' stamp: 'jmv 8/8/2016 10:59:08'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: []) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := ZMethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode! !

!methodRemoval: ZVariableScopeFinder #visitFutureNode:!
ZVariableScopeFinder removeSelector: #visitFutureNode:!
!methodRemoval: ZParseNodeEnumerator #visitFutureNode:!
ZParseNodeEnumerator removeSelector: #visitFutureNode:!
!methodRemoval: ZParseNodeVisitor #visitFutureNode:!
ZParseNodeVisitor removeSelector: #visitFutureNode:!
!methodRemoval: ZMethodNode #asColorizedSmalltalk80Text!
ZMethodNode removeSelector: #asColorizedSmalltalk80Text!
!methodRemoval: ZMethodNode #decompileText!
ZMethodNode removeSelector: #decompileText!
!methodRemoval: ZMethodNode #generateWithTempNames!
ZMethodNode removeSelector: #generateWithTempNames!
!methodRemoval: ZMethodNode #schematicTempNamesString!
ZMethodNode removeSelector: #schematicTempNamesString!
!methodRemoval: ZMessageNode #checkBlock:as:from:!
ZMessageNode removeSelector: #checkBlock:as:from:!
!methodRemoval: ZTempVariableNode #emitCodeForLoad:encoder:!
ZTempVariableNode removeSelector: #emitCodeForLoad:encoder:!
!methodRemoval: ZTempVariableNode #sizeCodeForLoad:!
ZTempVariableNode removeSelector: #sizeCodeForLoad:!
!methodRemoval: ZLiteralVariableNode #emitCodeForLoad:encoder:!
ZLiteralVariableNode removeSelector: #emitCodeForLoad:encoder:!
!methodRemoval: ZLiteralVariableNode #sizeCodeForLoad:!
ZLiteralVariableNode removeSelector: #sizeCodeForLoad:!
!methodRemoval: ZVariableNode #emitCodeForLoad:encoder:!
ZVariableNode removeSelector: #emitCodeForLoad:encoder:!
!methodRemoval: ZLeafNode #emitCodeForLoad:encoder:!
ZLeafNode removeSelector: #emitCodeForLoad:encoder:!
!methodRemoval: ZLeafNode #sizeCodeForLoad:!
ZLeafNode removeSelector: #sizeCodeForLoad:!
!methodRemoval: ZBytecodeEncoder #printSchematicTempNamesOn:blockExtents:fromIndex:!
ZBytecodeEncoder removeSelector: #printSchematicTempNamesOn:blockExtents:fromIndex:!
!methodRemoval: ZBytecodeEncoder #schematicTempNamesString!
ZBytecodeEncoder removeSelector: #schematicTempNamesString!
!methodRemoval: ZParseNode #isFutureNode!
ZParseNode removeSelector: #isFutureNode!
!methodRemoval: ZCompiler class #couldEvaluate:!
ZCompiler class removeSelector: #couldEvaluate:!
!methodRemoval: ZCompiler class #format:in:notifying:decorated:!
ZCompiler class removeSelector: #format:in:notifying:decorated:!
!methodRemoval: ZCompiler #compiledMethodFor:in:to:notifying:ifFail:logged:!
ZCompiler removeSelector: #compiledMethodFor:in:to:notifying:ifFail:logged:!
!methodRemoval: ZCompiler #format:in:notifying:decorated:!
ZCompiler removeSelector: #format:in:notifying:decorated:!
!methodRemoval: ZCompiler #parse:in:notifying:!
ZCompiler removeSelector: #parse:in:notifying:!
!methodRemoval: ZCompiler #parserClass:!
ZCompiler removeSelector: #parserClass:!
!methodRemoval: ZCompiler #translate:noPattern:ifFail:parser:!
ZCompiler removeSelector: #translate:noPattern:ifFail:parser:!
!methodRemoval: ZDecompiler #methodRefersOnlyOnceToTemp:!
ZDecompiler removeSelector: #methodRefersOnlyOnceToTemp:!
!classDefinition: #ZParseNode category: #'Compiler-ParseNodes'!
Object subclass: #ZParseNode
	instanceVariableNames: 'comment pc'
	classVariableNames: 'CodeBases CodeLimits EndRemote LdFalse LdInstType LdLitIndType LdLitType LdMinus1 LdNil LdSelf LdSuper LdTempType LdThisContext LdTrue LoadLong NodeFalse NodeNil NodeSelf NodeSuper NodeThisContext NodeTrue Pop Send SendPlus SendType StdLiterals StdSelectors StdVariables'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ZAssignmentNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation (new scheme)')
('testing' isAssignmentNode isComplex)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value: variable:value:from: variable:value:from:sourceRange:)
('code generation' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
!


!ZLiteralVariableNode reorganize!
('visiting' accept:)
('code generation (new scheme)')
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('testing' assignmentCheck:at:)
!


!ZMessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent: test)
('testing' canCascade ensureCanCascade: isComplex isMessage isMessage:receiver:arguments: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: selector:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

ZParseNode initialize.
ZVariableNode initialize.
ZMessageNode initialize.
ZDecompiler initialize.
ZCompiler initialize.
ZScanner initialize.!

