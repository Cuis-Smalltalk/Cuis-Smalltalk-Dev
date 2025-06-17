'From Cuis7.5 [latest update: #7268] on 17 June 2025 at 5:20:33 pm'!

!EncoderForSistaV1 commentStamp: 'eem 12/22/2024 19:07' prior: 0!
EncoderForSistaV1 encodes a bytecode set for Smalltalk that lifts limits on the number of literals, branch distances, the number of temporary variables, and provides extended push integer and push character bytecodes.  The bytecode set also supports creating FullBlockClosures, closures whose method is separate from their home method's.  Bytecodes are ordered by length to make decoding easier.  Bytecodes marked with an * are extensible via a prefix extension bytecode.

N.B.  Extension bytecodes can only come before extensible bytecodes, and only if valid (one cannot extend a bytecode extensible by Ext A with an Ext B).  An extensible bytecode consumes (and zeros) its extension(s).  Hence the hidden implicit variables holding extensions are always zero except after a valid sequence of extension bytecodes.  The implication is that a bytecode interpreter should maintain the extension values in static variables initialized to zero at start-up, and live only from the start of a sequence of extension bytecodes to the end of the extended bytecode immediately following.

While the bytecode set lifts limits, it still assumes there are no more than 65535 literals (as of 2020 the CompiledCode header word imposes a 32,767 limit on number of literals), and no more than 256 stack slots (used for arguments, temporaries, and stack contents) in a Context.

EncoderForSistaV1 also includes an extended set of bytecodes for Sista, the Speculative Inlining Smalltalk Architecture, a project by Clément Bera and Eliot Miranda.  Scorch is an optimizer that exists in the Smalltalk image, /not/ in the VM,  and optimizes by substituting normal bytecoded methods by optimized bytecoded methods that may use special bytecodes for which the Cogit can generate faster code.  These bytecodes eliminate overheads such as bounds checks or polymorphic code (indexing Array, ByteArray, String etc).  But the bulk of the optimization performed is in inlining blocks and sends for the common path.  This bytecode set therefore differs from a normal Smalltalk set in providing a set of inlined primitives that do not validate their arguments that the compiler generates only when it can prove that the primitives' arguments are valid.

The basic scheme is that the Cogit generates code containing performance counters.  When these counters trip, a callback into the image is performed, at which point Scorch analyses some portion of the stack, looking at performance data for the methods on the stack, and optimises based on the stack and performance data.  Execution then resumes in the optimized code.

The Sista Cogit (e.g. SistaStackToRegisterMappingCogit) adds counters to conditional branches.  Each branch has an executed and a taken count.  On execution the executed count is decremented and if the count goes below zero the VM sends a message at a special index in the specialObjectsArray (as of writing, conditionalCounterTrippedOn:).  Then if the branch is taken the taken count is decremented.  The two counter values allow the Sista optimizer to collect basic block execution paths and to know what are the "hot" paths through execution that are worth agressively optimizing.  Since conditional branches are about 1/6 as frequent as sends, and since they can be used to determine the hot path through code, they are a better choice to count than, for example, method or block entry.

The VM provides a primitive that fills an Array with the state of the counters, and the state of each linked send in a method.  The optimizer obtains the branch and send data for a method via this primitive.

Instance Variables (inherited)

Here is the list of bytecodes.  An asterisk implies the bytecode takes either extA or extB extensions. Two asterisks imply it takes both extA and extB extensions.  A number in parentheses is a note.  See the notes at the end of the table.

1 Byte Bytecodes
	code (note)	binary				name
	0-15		0000 iiii 				Push Receiver Variable #iiii
	16-31		0001 iiii				Push Literal Variable #iiii
	32-63		001 iiiii				Push Literal #iiiii
	64-71		01000 iii				Push Temp #iii
	72-75		010010 ii			Push Temp #ii + 8
	76			01001100			Push Receiver
	77			01001101			Push true
	78			01001110			Push false
	79			01001111			Push nil
	80			01010000			Push 0
	81			01010001			Push 1
*	82			01010010			Push thisContext, (then Extend B = 1 => push thisProcess)
	83			01010011			Duplicate Stack Top
	84-87		010101 ii			UNASSIGNED
	88-91		010110 ii			Return Receiver/true/false/nil
	92			01011100			Return top
	93			01011101			BlockReturn nil
*	94			01011110			BlockReturn Top [* return from enclosing block N, N = Extend A, then jump by Ext B ]
*	95			01011111			Nop
	96-111		0110 iiii				Send Arithmetic Message #iiii (+ - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:)
	112-119	01110 iii				Send Special Message #iii + 0 (at: at:put: size next nextPut: atEnd == class)
	120-127	01111 iii				Send Special Message #iii + 8 (~~ value value: do: new new: x y)
	128-143	1000 iiii				Send Literal Selector #iiii With 0 Argument
	144-159	1001 iiii				Send Literal Selector #iiii With 1 Arguments
	160-175	1010 iiii				Send Literal Selector #iiii With 2 Arguments
	176-183	10110 iii				Jump iii + 1 (i.e., 1 through 8)
	184-191	10111 iii				Pop and Jump 0n True iii +1 (i.e., 1 through 8)
	192-199	11000 iii				Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	200-207	11001 iii				Pop and Store Receiver Variable #iii
	208-215	11010 iii				Pop and Store Temporary Variable #iii
	216			11011000			Pop Stack Top
	217	(5)		11011001			Unconditional trap
	218-219	1101101 i			UNASSIGNED
	220-223	110111 ii			UNASSIGNED

2 Byte Bytecodes
*	224			11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A) A is an unsigned extension.
*	225			11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B) B is a signed extension.
*	226			11100010	iiiiiiii		Push Receiver Variable #iiiiiiii (+ Extend A * 256)
*	227			11100011	iiiiiiii		Push Literal Variable #iiiiiiii (+ Extend A * 256)
*	228			11100100	iiiiiiii		Push Literal #iiiiiiii (+ Extend A * 256)
	229			11100101	iiiiiiii		Push Temporary Variable #iiiiiiii
	230			11100110	iiiiiiii		UNASSIGNED (was pushNClosureTemps)
	231			11100111	jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)
*	232			11101000	iiiiiiii		Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, d=0, s=1)
*	233			11101001	iiiiiiii		Push Character #iiiiiiii (+ Extend A * 256)
**	234			11101010	iiiiijjj		Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
**	235	(1)		11101011	iiiiijjj		ExtendB < 64
										ifTrue: [Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments]
										ifFalse: [Send To Superclass of Stacked Class Literal Selector #iiiii (+ Extend A * 32) with jjj (+ (Extend B bitAnd: 63) * 8) Arguments]
	236			11101100	iiiiiiii		UNASSIGNED
*	237			11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, d=0, s=1)
**	238	(4)		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0)
**	239	(4)		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)
**	240	(3)		11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256) 
**	241	(3)		11110001	iiiiiiii		Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256) 
	242			11110010	iiiiiiii		Pop and Store Temporary Variable #iiiiiiii
**	243	(3)		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256) 
**	244	(3)		11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256) 
	245			11110110	iiiiiiii		Store Temporary Variable #iiiiiiii
	246-247	1111011 i	xxxxxxxx	UNASSIGNED

3 Byte Bytecodes
	248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
												m=1 means inlined primitive, no hard return after execution. 
												ss defines the unsafe operation set used to encode the operations. 
												(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are as yet unused)
												Lowcode inlined primitives may have extensions.
*	249		11111001 	xxxxxxxx	siyyyyyy		Push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1
**	250		11111010 	eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions
	251		11111011 	kkkkkkkk	sjjjjjjj		Push Temp At kkkkkkkk In Temp Vector At: jjjjjjj, s = 1 implies remote inst var access instead of remote temp vector access 
*	252	(3)	11111100 	kkkkkkkk	sjjjjjjj		Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access 
*	253	(3)	11111101 	kkkkkkkk	sjjjjjjj		Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access
**	254	(5)	11111110	kkkkkkkk	jjjjjjjj		branch If Not Instance Of Behavior/Array Of Behavior literal kkkkkkkk (+ Extend A * 256, where Extend A >= 0) distance jjjjjjjj (+ Extend B * 256, where Extend B >= 0 and <= 127)
**	254	(5)	11111110	kkkkkkkk	jjjjjjjj		branch If Instance Of Behavior/Array Of Behavior literal kkkkkkkk (+ Extend A * 256, where Extend A >= 0) distance jjjjjjjj (+ (Extend B bitAnd: 127) * 256, where Extend B >= 128 and <= 255)
*	255		11111111	xxxxxxxx	jjjjjjjj		UNASSIGNED

(1) Bytecode 235 is a super send bytecode that starts the lookup in the superclass of some class.  It has two forms, "normal" and "directed". In the normal form, the class is the value of the method's methodClassAssociation which must be the last literal.  In the directed form the class is the class on top of stack.

(2) The Call Primitive Bytecode (see below) specifies either a primitive in the primitive table (m=0) or an inlined primitive (m=1). Non-inlined primitives from the primitive table have index (jjjjjjj * 256) + iiiiiiii and return from the method if they succeed.  This bytecode is only valid as the first bytecode of a method.  Inline primitives have index (jjjjjjj * 256) + iiiiiiii, cannot fail, and do not return when they succeed, yielding a result (typically on top of stack after popping their arguments, but possibly in a byte data stack, for example for unboxed floating-point primitives).

(3) ExtB lowest bit implies no store check is needed, ExtB second bit implies the object may be a context, ExtB third bit implies no immutability/read-only check is needed, other bits in the extension are unused.

(4) ExtA = 1 implies no mustBeBoolean trampoline is needed, other bits in the extension are unused

(5) these are Scorch/Sista bytecodes generated by an optimizing compiler and not used in normal Smalltalk code.


The CallPrimitive bytecode is divided into two halves, those for normal primtiives, occurring at the beginning of a method, and those for inline primitives, anywhere within the body of a method.  This is a three byte bytecode, the first byte being 248, and the second byte being a big-endian 16-bit primitive index. If the top bit of the first byte of the primitive index is 1 then this is a normal primitive invocation.  If it is zero then the remaining 15 bits define 32k primitives, organized as four 8k "pages".  The first page is used for and reserved by the Sista optimizing compiler.  The second page is usd for and reserved by the Lowcode FFI marshalling primitive set.  The other two sets are unspecified and unused.

Here is the specification of the Sista unsafe instructions (unsafe operations, set 00). The lowcode set uses external specifications.
We sort the inline primitive operations by arity.  Nullary primitives occupy the 0-999 range. Unary primitives occupy the 1-1999 range, up until 8 args. 8191 instructions can be encoded in each unsafe operation set, instructions from 0 to 7 arguments can have 1000 different instructions each, while 8 args instructions can have 192 different instructions.

Sista defines the following inlined primitives (CallPrimitive iiiiiiii 100jjjjj, n = jjjjjiiiiiiii)
1000	class
1001	pointer numSlots
1002	pointer basicSize
1003	byte8Type format numBytes (includes CompiledMethod)
1004	short16Type format numShorts
1005	word32Type format numWords
1006	doubleWord64Type format numDoubleWords
	
1010	ensure number of bytes available.
1011	fixed-sized new. (objects with 0 to n inst vars)
	
1020 	identityHash (non-immediate, non-Behavior)
1021	identityHash (SmallInteger)
1022	identityHash (Character)
1023	identityHash (SmallFloat64)
1024	identityHash (Behavior, has hash?)

1030 	immediateAsInteger (Character)
1031 	immediateAsInteger (SmallFloat64)
1035 	immediateAsFloat 	(Smallinteger)
	
2000	SmallInteger #+.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2001	SmallInteger #-.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2002	SmallInteger #*.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2003	SmallInteger #/.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2004	SmallInteger #//.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2005	SmallInteger #\\.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2006	SmallInteger #quo:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)

2011	Variable-sized pointers new (new:). Array, etc.
2012	Variable-sized byte new (new:). ByteArray, ByteString, etc.
2013	Variable-sized 16-bit new (new:). DoubleByteArray, etc.
2014	Variable-sized 32-bit new (new:). Bitmap, FloatArray, etc.
2015	Variable-sized 64-bit new (new:). DoubleWordArray, etc.

2016	SmallInteger #bitAnd:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2017	SmallInteger #bitOr:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2018	SmallInteger #bitXor:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2019	SmallInteger #bitShiftLeft:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)
2020	SmallInteger #bitShiftRight:.  Both arguments are SmallIntegers and the result fits in a SmallInteger (* depends on word size)

2032	SmallInteger #>.  Both arguments are SmallIntegers
2033	SmallInteger #<.  Both arguments are SmallIntegers
2034	SmallInteger #>=.  Both arguments are SmallIntegers
2035	SmallInteger #<=.  Both arguments are SmallIntegers
2036	SmallInteger #=.  Both arguments are SmallIntegers
2037	SmallInteger #~=.  Both arguments are SmallIntegers

2064	Pointer Object>>at:.				The receiver is guaranteed to be a pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger
2065	Byte Object>>at:.				The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger.
2066	16-bit Word Object>>at:.		The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger.
2067	32-bit DoubleWord Object>>at:.	The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger or a LargePositiveInteger.
2068	64-bit QuadWord Object>>at:.	The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger or a LargePositiveInteger.

The following instructions can have the ExtB check flag (See (3)).
3000	Pointer Object>>at:put:.			The receiver is guaranteed to be a pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger
3001	Byte Object>>at:put:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 8 bits.
3002	Word Object>>at:put:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 16 bits.
3003	DoubleWord Object>>at:put:.		The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 32 bits.
3004	QuadWord Object>>at:put:.		The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 64 bits.
			
3021	Byte Object >> equals:length:	The receiver and the arguments are both byte objects and have both the same size (length). The length argument is a smallinteger. Answers true if all fields are equal, false if not. Comparison is bulked to word comparison.

4000	Pointer Object>> fillFrom:to:with: The receiver is a Pointer object. the middle two arguments are smallintegers. Last argument is any object. Fills the object in between the two indexes with last argument. Receiver is guaranteed to be mutable. The pointer accesses are raw (no inst var check). If ExtB is set to 1, no store check is present. Else a single store check is done for the bulk operation. Answers the receiver.
	
5000	Pointer Object>> replaceFrom:to:with:startingAt: Src and dest are pointer objects. ScrPos, scrLast and destLast are smallintegers. Receiver is guaranteed to be mutable.  Both ranges are in-bounds. The pointer accesses are raw (no inst var check). As for the normal primitive, the copy is linear. Answers the receiver.


Lowcode defines inlined primitives for the range CallPrimitive iiiiiiii 101jjjjj, n = jjjjjiiiiiiii.!


!EncoderForV3PlusClosures commentStamp: 'jmv 9/20/2016 19:42:38' prior: 0!
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book.

	0-15 		0000iiii 			Push Receiver Variable #iiii
	16-31 		0001iiii 			Push Temporary Location #iiii
	32-63 		001iiiii 			Push Literal Constant #iiiii
	64-95 		010iiiii 			Push Literal Variable #iiiii
	96-103 		01100iii 			Pop and Store Receiver Variable #iii
	104-111 	01101iii 			Pop and Store Temporary Location #iii
	112-119 	01110iii 			Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	120-123 	011110ii 		Return (receiver, true, false, nil) [ii] From Message
	124-125 	0111110i 		Return Stack Top From (Message, Block) [i]
	(126-127 unassigned)
	128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments
	134 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	135 		10000111 		Pop Stack Top
	136 		10001000 		Duplicate Stack Top
	137 		10001001 		Push Active Context
	(138-143 unassigned)
	144-151 	10010iii 			Jump iii + 1 (i.e., 1 through 8)
	152-159 	10011iii 			Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj
	168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj
	176-191 	1011iiii 			Send Arithmetic Message #iiii
	192-207 	1100iiii 			Send Special Message #iiii
	208-223 	1101iiii 			Send Literal Selector #iiii With No Arguments
	224-239 	1110iiii 			Send Literal Selector #iiii With 1 Argument
	240-255 	1111iiii 			Send Literal Selector #iiii With 2 Arguments

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

