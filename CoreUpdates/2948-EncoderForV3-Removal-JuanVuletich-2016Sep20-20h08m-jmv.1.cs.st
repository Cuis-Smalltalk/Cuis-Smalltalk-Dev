'From Cuis 4.2 of 25 July 2013 [latest update: #2946] on 20 September 2016 at 8:09:15 pm'!
!classDefinition: #EncoderForV3PlusClosures category: #'Compiler-Kernel'!
BytecodeEncoder subclass: #EncoderForV3PlusClosures
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!EncoderForV3PlusClosures commentStamp: 'jmv 9/20/2016 19:42:38' prior: 0!
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book.

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

An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation.
	138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)
	139   10001011 kkkkkkkk jjjjjjjj 	Invoke primitive number jjjjjjjjkkkkkkkk
	140   10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	141   10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	142   10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii
This is an exact duplicate of EncoderForLongFormV3PlusClosures.
Could be a trait (or in Newspeak, a Mixin).
For now we impose upon you to synchronise any and all changes between these two classes.!

!methodRemoval: EncoderForV3PlusClosures class #supportsClosures!
EncoderForV3PlusClosures class removeSelector: #supportsClosures!
!methodRemoval: BytecodeEncoder class #supportsClosures!
BytecodeEncoder class removeSelector: #supportsClosures!
!methodRemoval: MethodContext class #myEnvFieldIndex!
MethodContext class removeSelector: #myEnvFieldIndex!
!methodRemoval: MethodContext #isClosureContext!
MethodContext removeSelector: #isClosureContext!
!methodRemoval: MethodContext #isMethodContext!
MethodContext removeSelector: #isMethodContext!
!methodRemoval: ContextPart #isClosureContext!
ContextPart removeSelector: #isClosureContext!
!methodRemoval: InstructionStream #interpretNextV3InstructionFor:!
InstructionStream removeSelector: #interpretNextV3InstructionFor:!

!InstructionStream reorganize!
('testing' willBlockReturn willJump willJumpIfFalse willJumpIfTrue willJustPop willReallyStore willReturn willReturnTopFromMethod willSend willStore willStorePop)
('decoding' atEnd interpret interpretJump interpretJumpIfCond interpretNextInstructionFor: skipCallPrimitive)
('scanning' addSelectorTo: firstByte followingByte followingBytecode followingPc fourthByte method nextByte nextInstruction pc peekInstruction previousPc scanFor: secondByte selectorToSendOrSelf skipBackBeforeJump thirdByte)
('private' interpretExtension:in:for: method:pc: nextPc: pc: unusedBytecode:at:)
('debugger access' abstractPC debuggerMap)
('decoding - private - v3 plus closures' interpretNextV3ClosuresInstructionFor: interpretV3ClosuresExtension:in:for: interpretV3Extension:in:for: interpretV3Jump interpretV3JumpIfCond)
!

!classRemoval: #EncoderForV3!
Smalltalk removeClassNamed: #EncoderForV3!
