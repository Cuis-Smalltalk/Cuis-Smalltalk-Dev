'From Cuis7.3 [latest update: #6983] on 21 January 2025 at 1:27:04 pm'!
'From Cuis7.3 [latest update: #6983] on 21 January 2025 at 12:10:20 pm'!
!classDefinition: #CompiledMethod category: #'Kernel-Methods'!
ByteArray variableByteSubclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'LargeFrame PrimaryBytecodeSetEncoderClass SecondaryBytecodeSetEncoderClass SmallFrame PreferredBytecodeSetEncoderClass '
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!CompiledMethod commentStamp: '<historical>'!
My instances are methods suitable for interpretation by the virtual machine.  This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields. The pointer fields are used for literals and metadata, and the bytes are used for bytecodes and a variety of encoded informaton such as source code, source code position, etc.  The first part of a CompiledMethod object is pointers, the second part is bytes.  CompiledMethod inherits from ByteArray to avoid duplicating some of ByteArray's methods, not because a CompiledMethod is a ByteArray.

Instance variables: *indexed* (no named inst vars)

Class variables:
SmallFrame							- the number of stack slots in a small frame Context
LargeFrame							- the number of stack slots in a large frame Context
PreferredBytecodeSetEncoderClass	- the encoder class that is used to create new methods (either PrimaryBytecodeSetEncoderClass or SecondaryBytecodeSetEncoderClass)
PrimaryBytecodeSetEncoderClass		- the encoder class that defines the primary instruction set
SecondaryBytecodeSetEncoderClass	- the encoder class that defines the secondary instruction set
	
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
!

!classDefinition: #EncoderForSistaV1 category: #'Compiler-Kernel'!
BytecodeEncoder subclass: #EncoderForSistaV1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!EncoderForSistaV1 commentStamp: 'eem 12/22/2024 19:07'!
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
	code	(note)	binary			name
	0-15		0000 iiii 			Push Receiver Variable #iiii
	16-31		0001 iiii			Push Literal Variable #iiii
	32-63		001 iiiii				Push Literal #iiiii
	64-71		01000 iii			Push Temp #iii
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
	96-111		0110 iiii			Send Arithmetic Message #iiii (+ - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:)
	112-119	01110 iii			Send Special Message #iii + 0 (at: at:put: size next nextPut: atEnd == class)
	120-127	01111 iii			Send Special Message #iii + 8 (~~ value value: do: new new: x y)
	128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
	144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
	160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments
	176-183	10110 iii			Jump iii + 1 (i.e., 1 through 8)
	184-191	10111 iii			Pop and Jump 0n True iii +1 (i.e., 1 through 8)
	192-199	11000 iii			Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	200-207	11001 iii			Pop and Store Receiver Variable #iii
	208-215	11010 iii			Pop and Store Temporary Variable #iii
	216		11011000			Pop Stack Top
	217	(5)	11011001			Unconditional trap
	218-219	1101101 i			UNASSIGNED
	220-223	110111 ii			UNASSIGNED

2 Byte Bytecodes
*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A) A is an unsigned extension.
*	225		11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B) B is a signed extension.
*	226		11100010	iiiiiiii		Push Receiver Variable #iiiiiiii (+ Extend A * 256)
*	227		11100011	iiiiiiii		Push Literal Variable #iiiiiiii (+ Extend A * 256)
*	228		11100100	iiiiiiii		Push Literal #iiiiiiii (+ Extend A * 256)
	229		11100101	iiiiiiii		Push Temporary Variable #iiiiiiii
	230		11100110	iiiiiiii		UNASSIGNED (was pushNClosureTemps)
	231		11100111	jkkkkkkk	Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)
*	232		11101000	iiiiiiii		Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, d=0, s=1)
*	233		11101001	iiiiiiii		Push Character #iiiiiiii (+ Extend A * 256)
**	234		11101010	iiiiijjj		Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
**	235	(1)	11101011	iiiiijjj	ExtendB < 64
										ifTrue: [Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments]
										ifFalse: [Send To Superclass of Stacked Class Literal Selector #iiiii (+ Extend A * 32) with jjj (+ (Extend B bitAnd: 63) * 8) Arguments]
	236		11101100	iiiiiiii		UNASSIGNED
*	237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, d=0, s=1)
**	238	(4)	11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0)
**	239	(4)	11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)
**	240	(3)	11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256) 
**	241	(3)	11110001	iiiiiiii		Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256) 
	242		11110010	iiiiiiii		Pop and Store Temporary Variable #iiiiiiii
**	243	(3)	11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256) 
**	244	(3)	11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256) 
	245		11110110	iiiiiiii		Store Temporary Variable #iiiiiiii
	246-247	1111011 i	xxxxxxxx	UNASSIGNED

3 Byte Bytecodes
	248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
								m=1 means inlined primitive, no hard return after execution. 
								ss defines the unsafe operation set used to encode the operations. 
								(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are as yet unused)
								Lowcode inlined primitives may have extensions.
*	249		11111001 	xxxxxxxx	siyyyyyy	Push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1
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

2064	Pointer Object>>at:.		The receiver is guaranteed to be a pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger
2065	Byte Object>>at:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger.
2066	16-bit Word Object>>at:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger.
2067	32-bit DoubleWord Object>>at:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger or a LargePositiveInteger.
2068	64-bit QuadWord Object>>at:.	The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The result is a SmallInteger or a LargePositiveInteger.

The following instructions can have the ExtB check flag (See (3)).
3000	Pointer Object>>at:put:.			The receiver is guaranteed to be a pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger
3001	Byte Object>>at:put:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 8 bits.
3002	Word Object>>at:put:.			The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 16 bits.
3003	DoubleWord Object>>at:put:.	The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 32 bits.
3004	QuadWord Object>>at:put:.		The receiver is guaranteed to be a non-pointer object.  The 0-relative (1-relative?) index is an in-range SmallInteger.  The argument is a SmallInteger.  The primitive stores the least significant 64 bits.
			
3021	Byte Object >> equals:length:	The receiver and the arguments are both byte objects and have both the same size (length). The length argument is a smallinteger. Answers true if all fields are equal, false if not. Comparison is bulked to word comparison.

4000	Pointer Object>> fillFrom:to:with: The receiver is a Pointer object. the middle two arguments are smallintegers. Last argument is any object. Fills the object in between the two indexes with last argument. Receiver is guaranteed to be mutable. The pointer accesses are raw (no inst var check). If ExtB is set to 1, no store check is present. Else a single store check is done for the bulk operation. Answers the receiver.
	
5000	Pointer Object>> replaceFrom:to:with:startingAt: Src and dest are pointer objects. ScrPos, scrLast and destLast are smallintegers. Receiver is guaranteed to be mutable.  Both ranges are in-bounds. The pointer accesses are raw (no inst var check). As for the normal primitive, the copy is linear. Answers the receiver.


Lowcode defines inlined primitives for the range CallPrimitive iiiiiiii 101jjjjj, n = jjjjjiiiiiiii.!

!BraceNode commentStamp: 'eem 12/24/2024 16:24:49'!
Used for compiling and decompiling brace constructs.

These now compile into either a fast short form for 8 elements or less (see maxElementsForConsArray)
that uses the pushConsArrayWithElements: nElements bytecode
or a long form of indefinfite length:
	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.

The erstwhile brace assignment form is no longer supported.!

!ParseNodeEnumerator commentStamp: 'eem 3/19/2019 11:58'!
ParseNodeEnumerator implements ParseNode>>nodesDo:.  It can be used to enumerate an entire tree via
	aParseNode accept: (ParseNodeEnumerator ofBlock: aBlock)
or selectively, excluding the node and subnodes for which selectBlock answers false, via
	aParseNode accept: (ParseNodeEnumerator
							ofBlock: aBlock
							select: selectBlock)
Instance Variables
	theBlock:			<BlockClosure>
	theSelectBlock:		<BlockClosure | nil>

theBlock
	- the block that is evaluated with the parse nodes the receiver visits.

theSelectBlock
	- an optional block used to select blocks to visit and descend into.

Here's a doIt that generates and compiles the visiting methods:

self superclass selectors do:
	[:s|
	self compile: (String streamContents:
		[:str| | arg |
		arg := 'a', (s allButFirst: 5) allButLast.
		str nextPutAll: s, ' ', arg; crtab;
			nextPutAll: '(theSelectBlock isNil or: [theSelectBlock value: '; nextPutAll: arg; nextPutAll: ']) ifFalse:'; crtab;
			tab: 2; nextPutAll: '[^nil].'; crtab;
			nextPutAll: 'theBlock value: '; nextPutAll: arg; nextPut: $.; crtab;
			nextPutAll: '^super '; nextPutAll: s, ' ', arg])]!

!classDefinition: #ParseNodeWithPrecedingStatementEnumerator category: #'Compiler-Support'!
ParseNodeEnumerator subclass: #ParseNodeWithPrecedingStatementEnumerator
	instanceVariableNames: 'precedingStatement'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!ParseNodeWithPrecedingStatementEnumerator commentStamp: 'eem 3/19/2019 11:55'!
A ParseNodeWithPrecedingStatementEnumerator is a ParseNodeEnumerator that accepts a binary block in ofBlock:, and hence enumerates statement nodes with their preceding statement, or nil if they are the first.

Instance Variables
	precedingStatement:		<ParseNode | nil>

precedingStatement
	- the preceding statement node, if any
!

!classDefinition: #NarrowerVariableScopeFinder category: #'Compiler-Support'!
VariableScopeFinder subclass: #NarrowerVariableScopeFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!NarrowerVariableScopeFinder commentStamp: 'eem 4/3/2017 11:59'!
A NarrowerVariableScopeFinder is used to find a smaller scope for an already declared variable.!

!classDefinition: #SimulationSideEffectWarning category: #'Kernel-Exceptions'!
Warning subclass: #SimulationSideEffectWarning
	instanceVariableNames: 'primitiveIndex context method receiver arguments suppressed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Exceptions'!
!SimulationSideEffectWarning commentStamp: 'ct 3/3/2024 19:17'!
I am signaled to notify the client of the simulator (i.e., a sender of Context>>step) about potential side effects of the next instruction to be executed that would escape the control of the simulator. For example, I am signaled before the simulated code starts another process. See Context>>doPrimitive:method:receiver:args:, my messageText, and Parser>>simulationGuard for more information.!

!classDefinition: #ImplicitLiteralInstructionClientHook category: #'Kernel-Methods'!
Object subclass: #ImplicitLiteralInstructionClientHook
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!ImplicitLiteralInstructionClientHook commentStamp: 'eem 9/16/2021 19:35'!
ImplicitLiteralInstructionClientHook is a hook to allow clients to intercept bytecodes referencing implicit literals, such as special selector sends which have an implicit selector in Smalltalk specialSelectors.  It implements the messages sent by InstructionStream's interpretNextXXXInstructionFor: methods for bytecodes that reference implicit literals as sends of the relevant explciit literal methods.  This allows clients that want to to intercept implicit literal messages while leaving existing clients unchanged.

The class also supports CompiledCode>>abstractBytecodeMessagesFrom:to:do: et al which depend on MessageNotUnderstood to collect bytecode messages.  Use of instances of this class hides the existence of pushSpecialConstant: and sendSpecial:numArgs: since these are understood, but their sends of pushConstant: and send:super:numArgs: are not.

Instance Variables!

!classDefinition: #InstructionClient category: #'Kernel-Methods'!
ImplicitLiteralInstructionClientHook subclass: #InstructionClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!BlockLocalTempCounter commentStamp: 'eem 1/11/2018 08:30'!
I am a support class for the decompiler that is used to find the number of local temps in a block by finding out what the stack offset is at the end of a block.  I am necessary because in the EncoderForV3PlusClosures bytecode set the only way to initialize block-local temporaries is with pushConstant: nil bytecodes, but such bytecodes are ambiguous with a pushConstant: nil used to pass nil as a parameter or answer it as a result.  By scanning through to the end of the block these can be disambiguated by tracking the stack depth.!

!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack tempReadCounts '
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag OtherwiseFlag '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!Decompiler commentStamp: '<historical>'!
I decompile a method in three phases:
	Reverser: postfix byte codes -> prefix symbolic codes (nodes and atoms)
	Parser: prefix symbolic codes -> node tree (same as the compiler)
	Printer: node tree -> text (done by the nodes)
	

instance vars:

	constructor <DecompilerConstructor> an auxiliary knowing how to generate Abstract Syntax Tree (node tree)
	method <CompiledMethod> the method being decompiled
	instVars <Array of: String> the instance variables of the class implementing method
	tempVars <String | (OrderedCollection of: String)> hold the names of temporary variables (if known)
		NOTE: POLYMORPHISM WILL BE RESOLVED IN #initSymbols:
	constTable <Collection of: ParseNode> parse node associated with byte encoded constants (nil true false 0 1 -1 etc...)
	stack <OrderedCollection of: (ParseNode | String | Integer) > multipurpose...
	statements <OrderedCollection of: ParseNode> the statements of the method being decompiled 
	lastPc <Integer>
	exit <Integer>
	caseExits <OrderedCollection of: Integer> - stack of exit addresses that have been seen in the branches of caseOf:'s
	lastJumpPc <Integer>
	lastReturnPc <Integer>
	limit <Integer>
	hasValue <Boolean>
	blockStackBase <Integer>
	numLocaltemps <Integer | Symbol> - number of temps local to a block; also a flag indicating decompiling a block
	blockStartsToTempVars <Dictionary key: Integer value: (OrderedCollection of: String)>
	tempVarCount <Integer> number of temp vars used by the method
	lastJumpIfPcStack <OrderedCollection of: Integer> the value of program counter just before the last encountered conditional jumps
	tempReadCounts <Dictionary key: String value: Integer> the count of reads of temporaries, used to transform whiole lloops to to:[by:]do: loops accurately!


!Object methodsFor: 'testing' stamp: 'eem 1/4/2025 18:34:20'!
isCompiledCode

	^ false! !


!CompiledMethod methodsFor: 'accessing' stamp: 'eem 2/19/2012 10:59'!
clearSignFlag
	"Clear the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifTrue:
		[self objectAt: 1 put: self header - SmallInteger minVal]! !


!CompiledMethod methodsFor: 'accessing' stamp: 'eem 2/19/2012 10:59'!
setSignFlag
	"Set the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifFalse:
		[self objectAt: 1 put: self header + SmallInteger minVal]! !


!CompiledMethod methodsFor: 'accessing' stamp: 'eem 2/19/2012 10:59'!
signFlag
	"Answer the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	^self header < 0! !


!CompiledMethod methodsFor: 'testing' stamp: 'eem 1/4/2025 18:35:02'!
isCompiledBlock

	^false! !


!CompiledMethod methodsFor: 'testing' stamp: 'eem 1/4/2025 18:34:53'!
isCompiledCode

	^true! !


!CompiledMethod methodsFor: 'testing' stamp: 'eem 1/4/2025 18:35:10'!
isCompiledMethod

	^true! !


!CompiledMethod class methodsFor: 'method encoding' stamp: 'eem 10/9/2020 11:22'!
primaryBytecodeSetEncoderClass
	^PrimaryBytecodeSetEncoderClass! !


!CompiledMethod class methodsFor: 'method encoding' stamp: 'eem 10/9/2020 11:22'!
secondaryBytecodeSetEncoderClass
	^SecondaryBytecodeSetEncoderClass! !


!CompiledMethod class methodsFor: 'accessing' stamp: 'eem 12/26/2024 13:45:51'!
preferredBytecodeSetEncoderClass
	^PreferredBytecodeSetEncoderClass! !


!CompiledMethod class methodsFor: 'preferences' stamp: 'mt 10/15/2020 13:45:18.47243'!
preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass
	"Set the class that determines the bytecode set used to compile methods with.
			[| nPrimary nSecondary |
			nPrimary := nSecondary := 0.
			self allSubInstancesDo:
				[:cm|
				cm header >= 0
					ifTrue: [nPrimary := nPrimary + 1]
					ifFalse: [nSecondary := nSecondary + 1]].
			{nPrimary. nSecondary}]"
	| nPrimary nSecondary |
	aBytecodeEncoderSubclass ifNil: [ "Use default value."
		PreferredBytecodeSetEncoderClass := nil.
		^ self preferredBytecodeSetEncoderClass: self preferredBytecodeSetEncoderClass].
	self assert: (aBytecodeEncoderSubclass includesBehavior: BytecodeEncoder).
	(aBytecodeEncoderSubclass == PrimaryBytecodeSetEncoderClass
	 or: [aBytecodeEncoderSubclass == SecondaryBytecodeSetEncoderClass]) ifTrue:
		[PreferredBytecodeSetEncoderClass := aBytecodeEncoderSubclass.
		 ^self].
	nPrimary := nSecondary := 0.
	self allSubInstancesDo:
		[:cm|
		 cm header >= 0
			ifTrue: [nPrimary := nPrimary + 1]
			ifFalse: [nSecondary := nSecondary + 1]].
	nPrimary = 0 ifTrue:
		[self installPrimaryBytecodeSet: aBytecodeEncoderSubclass.
		 ^self preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass].
	nSecondary = 0 ifTrue:
		[self installSecondaryBytecodeSet: aBytecodeEncoderSubclass.
		 ^self preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass].
	self error: 'Cannot set preferred bytecode set.  Both of the current sets appear to be in use.'! !


!ParseNode methodsFor: 'visiting' stamp: 'eem 1/3/2019 15:46'!
nodesWithPrecedingStatementsDo: aBinaryBlock
	self accept: (ParseNodeWithPrecedingStatementEnumerator ofBlock: aBinaryBlock)! !


!ParseNode methodsFor: 'private' stamp: 'mt 9/4/2019 09:21'!
ifNilTemporary

	^ nil! !


!BytecodeEncoder methodsFor: 'testing' stamp: 'eem 1/10/2018 13:56'!
supportsFullBlocks
	"Answer if the instruction set supports full closures (closure creation from
	 specfic methods instead of bytecodes embedded in an outer home method)."

	^self subclassResponsibility! !


!BytecodeEncoder methodsFor: 'special literal encodings' stamp: 'eem 12/26/2024 11:32:36'!
isSpecialLiteralForPush: literal

	^self subclassResponsibility! !


!BytecodeEncoder methodsFor: 'special literal encodings' stamp: 'eem 12/26/2024 11:31:27'!
isSpecialLiteralForReturn: literal
	^literal == false
	  or: [literal == true
	  or: [literal == nil]]! !


!BytecodeEncoder methodsFor: 'accessing' stamp: 'eem 12/24/2024 16:40:27'!
nextPC
	"Answer the pc to store in a node for source range identification when the node is associated with its following pc."
	^stream position + 1! !


!BytecodeEncoder methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 10:56'!
genPushNClosureTemps: numTemps
	"To reduce the number of bytecodes required, the embedded
	 block closure implementation uses explicit push nil instructions
	 to create block-local temps.  In bytecode sets supporting
	 FullBlockClosure/CompiledBlock this isn't needed and the
	 number of temps is derived from the block method header."
	numTemps timesRepeat: [self genPushSpecialLiteral: nil]! !


!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 4/6/2017 14:12'!
pushNilCode
	"Answer the pushNil bytecode."
	^self subclassResponsibility! !


!BytecodeEncoder class methodsFor: 'bytecode decoding' stamp: 'eem 8/8/2014 19:23'!
stackDeltaForPrimitive: primitiveIndex in: method
	"This is the default implementation.  Subclasses with inline primitives will need to override."
	^0! !


!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 7/5/2022 16:05'!
isReturnSelfFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return self from method."

	self subclassResponsibility! !


!BytecodeEncoder class methodsFor: 'instruction stream support' stamp: 'eem 6/3/2024 10:25'!
isSuperSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send to super."

	self subclassResponsibility! !


!BytecodeEncoder class methodsFor: 'testing' stamp: 'eem 10/9/2021 17:28'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:. For performance, this method summarizes specializations from all known bytecode encoders. It is not meant to be refined per bytecode encoder."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."
	
	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral asInteger <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:28'!
genBranchPopFalse: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["192-199	11000 iii			Pop and Jump 0n False iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 191 + distance.
		 ^self].
	^self genBranchPopFalseLong: distance! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:33'!
genBranchPopFalseLong: distance
	"239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)	"
	| distanceMod256 |
	(distance < 0 or: [distance > 32767]) ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 32767].
	distanceMod256 := (distance < 0 or: [distance > 255])
							ifTrue:
								[self genUnsignedSingleExtendB: (distance bitShift: -8).
								 distance bitAnd: 255]
							ifFalse: [distance].
	stream
		nextPut: 239;
		nextPut: distanceMod256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:31'!
genBranchPopTrue: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["184-191	10111 iii			Pop and Jump 0n True iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 183 + distance.
		 ^self].
	^self genBranchPopTrueLong: distance! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:33'!
genBranchPopTrueLong: distance
	"238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0)"
	| distanceMod256 |
	(distance < 0 or: [distance > 32767]) ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 32767].
	(distance > 0 and: [distance < 9]) ifTrue:
		["184-191	10111 iii			Pop and Jump 0n True iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 183 + distance.
		 ^self].
	distanceMod256 := (distance < 0 or: [distance > 255])
							ifTrue:
								[self genUnsignedSingleExtendB: (distance bitShift: -8).
								 distance bitAnd: 255]
							ifFalse: [distance].
	stream
		nextPut: 238;
		nextPut: distanceMod256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 7/25/2016 14:10'!
genCallInlinePrimitive: primitiveIndex
	"	248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
								m=1 means inlined primitive, no hard return after execution. 
								ss defines the unsafe operation set used to encode the operations. 
								(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are not used)"
	"N.B. We could have made CallPrimitive a 2-byte code taking an extension, but that would
	 complicate the VM's determination of the primitive number and the primitive error code
	 store since the extension, being optional, would make the sequence variable length."
	(primitiveIndex < 1 or: [primitiveIndex > 32767]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 32767].
	stream
		nextPut: 248;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8) + 128! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 10:11'!
genCallPrimitive: primitiveIndex
	"248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
								m=1 means inlined primitive, no hard return after execution. 
								ss defines the unsafe operation set used to encode the operations. 
								(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are not used)"
	"N.B. We could have made CallPrimitive a 2-byte code taking an extension, but that would
	 complicate the VM's determination of the primitive number and the primitive error code
	 store since the extension, being optional, would make the sequence variable length."
	(primitiveIndex < 1 or: [primitiveIndex > 32767]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 32767].
	stream
		nextPut: 248;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:57'!
genDup
	"83			01010011			Duplicate Stack Top"
	stream nextPut: 83! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:59'!
genJump: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["176-183	10110 iii			Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 175 + distance.
		 ^self].
	"237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	^self genJumpLong: distance! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:59'!
genJumpLong: distance
	"237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	(distance between: -32768 and: 32767) ifFalse:
		[^self outOfRangeError: 'index' index: distance range: -32768 to: 32767].
	(distance < 0 or: [distance > 255]) ifTrue:
		[self genSignedSingleExtendB: (distance bitShift: -8)].
	stream
		nextPut: 237;
		nextPut: (distance bitAnd: 255)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 16:27'!
genNop
	"95			01011111			Nop"
	stream nextPut: 95! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 17:01'!
genPop
	"216		11011000			Pop Stack Top"
	stream nextPut: 216! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 1/4/2025 17:20:44'!
genPushCharacter: aCharacterOrCode
	"233		11101001	i i i i i i i i	Push Character #iiiiiiii (+ Extend A * 256)"
	"Why restrict the range to 16 bits when we could encode arbitrary 32-bit Characters?
	 Well, 16 bits requires 4 bytes (extA + byte, 233 + byte) and so beyond this range we
	 lose space verses a single-byte pushLiteral and a 4 byte Character literal on 32-bits.
	 And generating the same bytecode on 64-bit and 32-bit versions is important if we
	 want to be able to load binary code from one to the other (e.g. via Fuel)."
	| code |
	code := aCharacterOrCode isInteger ifTrue: [aCharacterOrCode] ifFalse: [aCharacterOrCode codePoint].
	(code < 0 or: [code > 65535]) ifTrue:
		[^self outOfRangeError: 'character' index: code range: 0 to: 65535].
	(code > 255) ifTrue:
		[self genUnsignedSingleExtendA: (code bitShift: -8)].
	stream
		nextPut: 233;
		nextPut: (code bitAnd: 255)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/7/2014 17:01'!
genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	"250		11111010 eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ Ext A // 16 * 8) Num Args kkk (+ Ext A \\ 16 * 8) BlockSize jjjjjjjj (+ Ext B * 256). ee = num extensions"
	"Including numExtensions makes decoding the bytecode quicker since it obviates having to scan from the beginning of a method."
	| numExtensions numCopiedMod8 numArgsMod8 extA |
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 127]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 127].
	(numArgs < 0 or: [numArgs > 127]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 127].
	extA := numExtensions := 0.
	(numArgsMod8 := numArgs) > 7 ifTrue:
		[extA := numArgs // 8.
		 numArgsMod8 := numArgsMod8 \\ 8].
	(numCopiedMod8 := numCopied) > 7 ifTrue:
		[extA := extA + (numCopied // 8 * 16).
		 numCopiedMod8 := numCopiedMod8 \\ 8].
	extA ~= 0 ifTrue:
		[self genUnsignedSingleExtendA: extA.
		 numExtensions := 1].
	jumpSize > 255 ifTrue:
		[numExtensions := numExtensions + 1.
		 self genUnsignedSingleExtendB: jumpSize // 256].
	stream
		nextPut: 250;
		nextPut: (numExtensions bitShift: 6) + (numCopiedMod8 bitShift: 3) + numArgsMod8;
		nextPut: (jumpSize bitAnd: 16rFF)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/1/2015 10:32'!
genPushConsArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'size' index: size range: 0 to: 127].
	"231		11100111	jkkkkkkk	Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	stream
		nextPut: 231;
		nextPut: size + 128! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 3/23/2017 16:37'!
genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied
	"By default the closure will have an outer context and the receiver will be fetched from the current context"
	self genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied receiverOnStack: false ignoreOuterContext: false! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 3/23/2017 16:48'!
genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied receiverOnStack: receiverOnStack ignoreOuterContext: ignoreOuterContext
	"*	249		11111001 	xxxxxxxx	siyyyyyy	push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1"
	| extendedIndex |
	(numCopied < 0 or: [numCopied > 64]) ifTrue:
		[self outOfRangeError: 'num copied' index: numCopied range: 1 to: 64].
	(compiledBlockLiteralIndex < 0 or: [compiledBlockLiteralIndex > 32767]) ifTrue:
		[^self outOfRangeError: 'index' index: compiledBlockLiteralIndex range: 0 to: 32767].
	(extendedIndex := compiledBlockLiteralIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 249;
		nextPut: extendedIndex;
		nextPut: receiverOnStack asBit << 7 + (ignoreOuterContext asBit << 6) + numCopied! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 10/12/2012 15:12'!
genPushInstVar: instVarIndex
	(instVarIndex between: 0 and: 15) ifTrue:
		["0-15 	0000iiii 	Push Receiver Variable #iiii"
		 stream nextPut: 0 + instVarIndex.
		 ^self].
	self genPushInstVarLong: instVarIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:28'!
genPushInstVarLong: instVarIndex
	"226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex < 0 or: [instVarIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 65535].
	instVarIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instVarIndex // 256].
	stream
		nextPut: 226;
		nextPut: instVarIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 09:22'!
genPushInteger: anInteger
	"80			01010000				Push 0
	 81			01010001				Push 1
	 232		11101000	i i i i i i i i	Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	"Why restrict the range to 16 bits when we could encode arbitrarily large integers?
	 Well, 16 bits requires 4 bytes (extB + byte, 78 + byte) and so beyond this range we lose space
	 verses a single-byte pushLiteral and a 4 byte integer literal on 32-bits.  And generating the same
	 bytecode on 64-bit and 32-bit is important if we want to be able to load binary code from one to
	 the other (e.g. via Fuel)."
	anInteger = 0 ifTrue:
		[stream nextPut: 80.
		 ^self].
	anInteger = 1 ifTrue:
		[stream nextPut: 81.
		 ^self].
	(anInteger < -32768 or: [anInteger > 32767]) ifTrue:
		[^self outOfRangeError: 'integer' index: anInteger range: -32768 to: 32767].
	(anInteger < 0 or: [anInteger > 255]) ifTrue:
		[self genSignedSingleExtendB: (anInteger bitShift: -8)].
	stream
		nextPut: 232;
		nextPut: (anInteger bitAnd: 255)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 10:25'!
genPushLiteral: literalIndex
	| extendedIndex |
	(literalIndex < 0 or: [literalIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 65535].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	"228		11100100	i i i i i i i i	Push Literal #iiiiiiii (+ Extend A * 256)"
	(extendedIndex := literalIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 228;
		nextPut: extendedIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 10/18/2012 14:51'!
genPushLiteralVar: literalIndex
	| extendedIndex |
	(literalIndex < 0 or: [literalIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 65535].
	literalIndex < 16 ifTrue: 
		["16-31		0001 i i i i		Push Literal Variable #iiii"
		 stream nextPut: 16 + literalIndex.
		 ^self].
	"227		11100011	i i i i i i i i	Push Literal Variable #iiiiiiii (+ Extend A * 256)"
	(extendedIndex := literalIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 227;
		nextPut: extendedIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 11/4/2012 15:01'!
genPushNewArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'size' index: size range: 0 to: 127].
	"231		11100111	jkkkkkkk	Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	stream
		nextPut: 231;
		nextPut: size! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 10/18/2012 14:43'!
genPushReceiver
	"76			01001100		Push Receiver"
	stream nextPut: 76! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/4/2016 18:02'!
genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"251		11111011 	kkkkkkkk	sjjjjjjj		Push Temp At kkkkkkkk In Temp Vector At: jjjjjjj, s = 1 implies remote inst var access instead of remote temp vector access"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 127].
	stream
		nextPut: 251;
		nextPut: tempIndex;
		nextPut: tempVectorIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 1/2/2018 13:16'!
genPushSpecialLiteral: aLiteral
	"77			01001101			Push true
	 78			01001110			Push false
	 79			01001111			Push nil
	 80			01010000			Push 0
	 81			01010001			Push 1
	 232		11101000	iiiiiiii		Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)
	 233		11101001	i i i i i i i i	Push Character #iiiiiiii (+ Extend B * 256)"
	| index |
	aLiteral isInteger ifTrue:
		[aLiteral == 0 ifTrue:
			[stream nextPut: 80.
			 ^self].
		 aLiteral == 1 ifTrue:
			[stream nextPut: 81.
			 ^self].
		 ^self genPushInteger: aLiteral].
	aLiteral isCharacter ifTrue:
		[^self genPushCharacter: aLiteral].
	index := #(true false nil)
					indexOf: aLiteral
					ifAbsent: [^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 76 + index! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 09:52'!
genPushTemp: tempIndex
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 12 ifTrue: 
		["64-71		01000 i i i		Push Temporary Variable #iii
		   72-75	010010 i i		Push Temporary Variable #ii + 8"
		 stream nextPut: 64 + tempIndex.
		 ^self].
	"229		11100101	i i i i i i i i	Push Temporary Variable #iiiiiiii"
	stream
		nextPut: 229;
		nextPut: tempIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 7/28/2014 20:51'!
genPushThisContext
	"82			01010010			Push thisContext, (then e.g. Extend B 1 = push thisProcess)"
	stream nextPut: 82! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 1/5/2018 12:12'!
genReturnNilToCaller
	"93			01011101			BlockReturn nil [* return from enclosing block N, ExtA]"
	"If extended, the least significant bit of the extension determines if we return to the caller or not
	 and the most significant bits determine how many levels of the static chain to return from.
		ExtA = iiiiiiij
		iiiiiii=0,j=0	=>	return to caller
		iiiiiii=0,j=1	=>	illegal
		iiiiiii=1,j=0	=>	return to outerContext
		iiiiiii=1,j=1	=>	return to outerContext sender/return from outerContext
		iiiiiii=2,j=0	=>	return to outerContext outerContext
		iiiiiii=2,j=1	=>	return to outerContext outerContext sender/return from outerContext outerContext
		etc"

	stream nextPut: 93! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 09:56'!
genReturnReceiver
	"88-91		010110 ii			Return Receiver/true/false/nil"
	stream nextPut: 88! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 09:58'!
genReturnSpecialLiteral: aLiteral
	"88-91		010110 ii			Return Receiver/true/false/nil"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 88 + index! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:06'!
genReturnTop
	"92		1011100		Return Stack Top From Message"
	stream nextPut: 92! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/1/2015 09:59'!
genReturnTopToCaller
	"94		01011110		Return Stack Top From Block [* return from enclosing block N, ExtA]"
	"If extended, the least significant bit of the extension determines if we return to the caller or not
	 and the most significant bits determine how many levels of the static chain to return from.
		ExtA = iiiiiiij
		iiiiiii=0,j=0	=>	return to caller
		iiiiiii=0,j=1	=>	illegal
		iiiiiii=1,j=0	=>	return to outerContext
		iiiiiii=1,j=1	=>	return to outerContext sender/return from outerContext
		iiiiiii=2,j=0	=>	return to outerContext outerContext
		iiiiiii=2,j=1	=>	return to outerContext outerContext sender/return from outerContext outerContext
		etc"

	stream nextPut: 94! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 14:27'!
genSend: selectorLiteralIndex numArgs: nArgs
	| extendedIndex extendedNArgs |
	(selectorLiteralIndex < 0 or: [selectorLiteralIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'selectorLiteralIndex' index: selectorLiteralIndex range: 0 to: 65535].
	(nArgs < 0 or: [nArgs > 31]) ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!!!"].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue: 
	 	["128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
		  144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
		  160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 128 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(extendedIndex := selectorLiteralIndex) > 31 ifTrue:
		[self genUnsignedMultipleExtendA: extendedIndex // 32.
		 extendedIndex := extendedIndex \\ 32].
	(extendedNArgs := nArgs) > 7 ifTrue:
		[self genUnsignedSingleExtendB: extendedNArgs // 8.
		 extendedNArgs := extendedNArgs \\ 8].
	"234		11101010	i i i i i j j j	Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
	stream
		nextPut: 234;
		nextPut: extendedNArgs + (extendedIndex * 8)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 15:26'!
genSendDirectedSuper: selectorLiteralIndex numArgs: nArgs
	| extendedIndex |
	(selectorLiteralIndex < 0 or: [selectorLiteralIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'selectorLiteralIndex' index: selectorLiteralIndex range: 0 to: 65535].
	(nArgs < 0 or: [nArgs > 31]) ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!!!"].
	(extendedIndex := selectorLiteralIndex) > 31 ifTrue:
		[self genUnsignedMultipleExtendA: extendedIndex // 32.
		 extendedIndex := extendedIndex \\ 32].
	"Bit 6 of the ExtB byte is the directed send flag.  Bit 6 allows for future expansion to up to 255 args."
	self genUnsignedSingleExtendB: nArgs // 8 + 64.
	"235		11101011	iiiiijjj		Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
	stream
		nextPut: 235;
		nextPut: nArgs \\ 8 + (extendedIndex * 8)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:21'!
genSendSpecial: specialSelectorIndex numArgs: nArgs
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		96-111		0110 iiii			Send Arithmetic Message #iiii #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr:)
		112-119	01110 iii			Send Special Message #iii #(#at: #at:put: #size ? ? ? #'==' class ? value value: ? ? ? ? ?)"

	stream nextPut: specialSelectorIndex + 95! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/6/2017 15:26'!
genSendSuper: selectorLiteralIndex numArgs: nArgs
	| extendedIndex extendedNArgs |
	(selectorLiteralIndex < 0 or: [selectorLiteralIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'selectorLiteralIndex' index: selectorLiteralIndex range: 0 to: 65535].
	(nArgs < 0 or: [nArgs > 31]) ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!!!"].
	(extendedIndex := selectorLiteralIndex) > 31 ifTrue:
		[self genUnsignedMultipleExtendA: extendedIndex // 32.
		 extendedIndex := extendedIndex \\ 32].
	(extendedNArgs := nArgs) > 7 ifTrue:
		[self genUnsignedSingleExtendB: extendedNArgs // 8.
		 extendedNArgs := extendedNArgs \\ 8].
	"235		11101011	iiiiijjj		Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
	stream
		nextPut: 235;
		nextPut: extendedNArgs + (extendedIndex * 8)! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 10/22/2012 16:07'!
genSignedSingleExtendB: extendedIndex
	(extendedIndex between: -128 and: 127) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: -128 to: 127].
	"225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)"
	stream
		nextPut: 225;
		nextPut: (extendedIndex >= 0 ifTrue: [extendedIndex] ifFalse: [extendedIndex + 256]) ! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:27'!
genStoreInstVar: instVarIndex
	"243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	self genStoreInstVarLong: instVarIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:28'!
genStoreInstVarLong: instVarIndex
	"243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	(instVarIndex < 0 or: [instVarIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 65535].
	instVarIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instVarIndex // 256].
	stream
		nextPut: 243;
		nextPut: instVarIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:35'!
genStoreLiteralVar: literalIndex
	"244		11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256)"
	(literalIndex < 0 or: [literalIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 65535].
	literalIndex > 255 ifTrue: 
		[self genUnsignedSingleExtendA: literalIndex // 256].
	stream
		nextPut: 244;
		nextPut: literalIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:37'!
genStorePopInstVar: instVarIndex
	"200-207	11001 iii			Pop and Store Receiver Variable #iii
	 240		11110000	iiiiiiii	Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	(instVarIndex < 0 or: [instVarIndex > 7]) ifTrue:
		[^self genStorePopInstVarLong: instVarIndex].
	stream nextPut: 200 + instVarIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:37'!
genStorePopInstVarLong: instVarIndex
	"240		11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	(instVarIndex < 0 or: [instVarIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 65535].
	instVarIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instVarIndex // 256].
	stream
		nextPut: 240;
		nextPut: instVarIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:39'!
genStorePopLiteralVar: literalIndex
	"241		11110001	iiiiiiii		Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)"
	(literalIndex < 0 or: [literalIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 65535].
	literalIndex > 255 ifTrue: 
		[self genUnsignedSingleExtendA: literalIndex // 256].
	stream
		nextPut: 241;
		nextPut: literalIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/4/2016 18:01'!
genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"*	253	(3)	11111101 	kkkkkkkk	sjjjjjjj		Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 127].
	stream
		nextPut: 253;
		nextPut: tempIndex;
		nextPut: tempVectorIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 4/8/2014 10:43'!
genStorePopTemp: tempIndex
	"208-215	11010 iii			Pop and Store Temporary Variable #iii
	 242		11110010	iiiiiiii	Pop and Store Temporary Variable #iiiiiiii"
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue:
		[stream nextPut: 208 + tempIndex.
		 ^self].
	stream
		nextPut: 242;
		nextPut: tempIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/4/2016 18:02'!
genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"*252	(3)	11111100 	kkkkkkkk	sjjjjjjj		Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 127].
	stream
		nextPut: 252;
		nextPut: tempIndex;
		nextPut: tempVectorIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 4/1/2015 09:58'!
genStoreTemp: tempIndex
	"245		11110110	iiiiiiii		Store Temporary Variable #iiiiiiii"
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	stream
		nextPut: 245;
		nextPut: tempIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eem 8/1/2014 06:53'!
genTrapIfNotInstanceOf: literalIndex
	"*	236		11101100	iiiiiiii		Trap If Not Instance Of Behavior/Array Of Behavior #iiiiiiii (+ Extend A * 256, where Extend A >= 0)"

	| extendedIndex |
	(literalIndex < 0 or: [literalIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 65536].
	(extendedIndex := literalIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 236;
		nextPut: extendedIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'eliot 10/18/2012 14:47'!
genUnsignedMultipleExtendA: extendedIndex
	"224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)"
	extendedIndex > 255 ifTrue:
		[self genUnsignedMultipleExtendA: extendedIndex // 256].
	stream
		nextPut: 224;
		nextPut: extendedIndex \\ 256! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 7/25/2016 14:23'!
genUnsignedSingleExtendA: extendedIndex
	(extendedIndex between: 0 and: 255) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: 0 to: 255].
	"224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	ExtA is normally unsigned."
	stream
		nextPut: 224;
		nextPut: extendedIndex! !


!EncoderForSistaV1 methodsFor: 'bytecode generation' stamp: 'cb 7/25/2016 14:23'!
genUnsignedSingleExtendB: extendedIndex
	(extendedIndex between: 0 and: 255) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: 0 to: 255].
	"225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B).
	ExtB is normally signed"
	stream
		nextPut: 225;
		nextPut: extendedIndex! !


!EncoderForSistaV1 methodsFor: 'in-line primitive generation' stamp: 'eem 4/7/2014 16:38'!
genInlineSmallIntegerAdd
	^self genCallInlinePrimitive: 0! !


!EncoderForSistaV1 methodsFor: 'special literal encodings' stamp: 'eem 1/4/2025 17:20:28'!
isSpecialLiteralForPush: literal
	^literal isInteger
		ifFalse:
			[literal isCharacter
				ifFalse:
					[false == literal
					 or: [true == literal
					 or: [nil == literal]]]
				ifTrue:
					[literal codePoint between: 0 and: 65535]]
	 	ifTrue:
			[literal between: -32768 and: 32767]! !


!EncoderForSistaV1 methodsFor: 'accessing' stamp: 'eem 6/15/2016 14:37'!
maxIndexableLiterals
	"Answer the maximum number of literals supported by the receiver's
	 bytecode set."
	^65536! !


!EncoderForSistaV1 methodsFor: 'testing' stamp: 'eem 12/24/2024 16:35:45'!
supportsFullBlocks
	"Answer if the instruction set supports full closures (closure creation from
	 specfic methods instead of bytecodes embedded in an outer home method).

	N.B. While the Sista set *does* support full blocks we are not using them yet.
	So this implementation, unlike e.g. Squeak, answers false."

	^false! !


!EncoderForSistaV1 methodsFor: 'method generation' stamp: 'eem 8/1/2014 22:45'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	numTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	^SmallInteger minVal "sign bit is the flag for the alternative bytecode set"
	+ (numArgs bitShift: 24)
	+ (numTemps bitShift: 18)
	"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
	+ numLits
	+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:54'!
bindingReadScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	16-31		0001 i i i i				Push Literal Variable #iiii
	 *	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	227		11100011	i i i i i i i i	Push Literal Variable #iiiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b < 32 and: [b >= 16 and: [b - 16 = litVarIndex]])
	    or: [b = 227
			and: [scanner followingByte + prevext = litVarIndex]]]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:59'!
bindingWriteScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	241		11110001	iiiiiiii		Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)
	 *	244		11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b = 241 or: [b = 244])
	   and: [scanner followingByte + prevext = litVarIndex]]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 6/16/2018 16:29'!
createClosureScanBlock
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes.  Note that with this interface we can't answer
	 true for the extension in front of a push closure bytecode and so the interface may
	 have to change at some point."

	"*	224	11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	225	11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	**	249	11111001 	xxxxxxxx	siyyyyyy	push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1
	 **	250	11111010 	eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
	^[:b| b >= 249 and: [b <= 250]]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:56'!
extensionsAt: bcpc in: method into: aTrinaryBlock
	"If the bytecode at pc is an extension then evaluate aBinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not extended then evaluate aBinaryBlock with 0 and 0.
	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	225		11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)"
 
	| scanpc byte extByte extA extB |
	scanpc := bcpc.
	"There may be an extension (it could be a false positive).  We must scan as fast as possible..."
	extA := extB := 0.
	[byte := method at: scanpc.
	 byte >= 224 and: [byte <= 225]] whileTrue: 
		[extByte := method at: scanpc + 1.
		 scanpc := scanpc + 2.
		 byte = 224
			ifTrue:
				[extA := (extA bitShift: 8) + extByte]
			ifFalse:
				[extB := (extB = 0 and: [extByte > 127])
					ifTrue: [extByte - 256]
					ifFalse: [(extB bitShift: 8) + extByte]]].
	^aTrinaryBlock value: extA value: extB value: scanpc - bcpc


"Why use
	byte >= 224 and: [byte <= 225]
 and not
	(byte bitAnd: 16rFE) = 16rE0
 ?
 | n |
 n := 100000000.
 #(0 224) collect:
	[:byte|
	{ Time millisecondsToRun: [1 to: n do: [:i| (byte >= 224 and: [byte <= 225]) ifTrue: []]].
	   Time millisecondsToRun: [1 to: n do: [:i| (byte bitAnd: 16rFE) = 16rE0 ifTrue: []]] }] #(#(297 599) #(702 671))"! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:57'!
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	0-15		0000 i i i i 				Push Receiver Variable #iiii
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b < 16 and: [b = varIndexCode])
	    or: [b = 226
			and: [scanner followingByte + prevext = varIndexCode]]]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:59'!
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	200-207	11001 iii			Pop and Store Receiver Variable #iii
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	240		11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	*	243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b >= 200
	    and: [b < 208
	    and: [b - 200 = varIndexCode]])
	   or: [(b = 240 or: [b = 243])
		  and: [scanner followingByte + prevext = varIndexCode]]]! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 18:59'!
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretSistaV1JumpIfCond! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'eem 5/18/2014 19:00'!
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct unconditional jump decoder for the instruction set."
	^anInstructionStream interpretSistaV1Jump! !


!EncoderForSistaV1 class methodsFor: 'compiled method support' stamp: 'TraitTest 9/2/2019 17:07'!
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
	| expectedHeaderPlusLliteralSize e byte |
	expectedHeaderPlusLliteralSize := Smalltalk wordSize * 4.
	^(((e := aMethod endPC - expectedHeaderPlusLliteralSize) = 3 or: [e = 4]) 
	  and: [aMethod numLiterals = 3
	  and: [(aMethod at: expectedHeaderPlusLliteralSize + 1) = 16r4C "push self"
	  and: [(aMethod at: expectedHeaderPlusLliteralSize + 2) = 16r80 "send"
	  and: [(byte := aMethod at: expectedHeaderPlusLliteralSize + 3) = 16rD8 "pop" or: [byte = 16r5C "returnTop"]]]]])
		ifTrue: [aMethod literalAt: 1]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:00'!
bytecodeSize: bytecode
	"Answer the number of bytes in the bytecode."
	bytecode < 224 ifTrue: [^1].
	bytecode < 248 ifTrue: [^2].
	^3! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:00'!
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextSistaV1InstructionFor: aClient! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:01'!
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."
	"	93			01011101			BlockReturn nil
	 *	94			01011110			BlockReturn Top [* return from enclosing block N, N = Extend A, then jump by Ext B ]"
	^(self nonExtensionBytecodeAt: pc in: method) between: 93 and: 94! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:03'!
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."

	"	192-199	11000 iii				Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	 *	239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 192 and: [byte <= 199 or: [byte = 239]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:04'!
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."

	"	184-191	10111 iii				Pop and Jump 0n True iii +1 (i.e., 1 through 8)
	 *	238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0))"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 184 and: [byte <= 191 or: [byte = 238]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:17'!
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends
	 the range of the following bytecode."
	^bytecode >= 16rE0 and: [bytecode <= 16rE1]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:06'!
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."

	"	176-183	10110 iii				Jump iii + 1 (i.e., 1 through 8)
	 *	225		11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 *	237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 176 and: [byte <= 183 or: [byte = 237]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 13:29'!
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	^(method at: pc) = 216 "216		11011000			Pop Stack Top"! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:11'!
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."
	"	88-91		010110 ii			Return Receiver/true/false/nil
		92			01011100			Return top
		93			01011101			BlockReturn nil
	 *	94			01011110			BlockReturn Top [* return from enclosing block N, N = Extend A, then jump by Ext B ]"
	^(self nonExtensionBytecodeAt: pc in: method) between: 88 and: 94! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 7/5/2022 15:55'!
isReturnSelfFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return self from method."

	^(method at: pc) = 88! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 7/5/2022 15:47'!
isReturnTopFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return stack top from method."

	^(method at: pc) = 92! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 7/6/2024 18:53'!
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	"	96-111		0110 iiii			Send Arithmetic Message #iiii #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr:)
		112-119	01110 iii			Send Special Message #iii #(#at: #at:put: #size #next #nextPut: #atEnd #'==' class)
		120-127	01111 iii			Send Special Message #iii + 8 #(#'~~' #value #value: #do: #new #new: #x #y)
		128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
		144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
		160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments
	 **	234		11101010	iiiiijjj	Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
	 **	235		11101011	iiiiijjj	Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 96
	  and: [byte <= 175
		 or: [byte >= 234 and: [byte <= 235]]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:19'!
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	"	200-207	11001 iii						Pop and Store Receiver Variable #iii
		208-215	11010 iii						Pop and Store Temporary Variable #iii
	 *	224		11100000	aaaaaaaa			Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	240		11110000	iiiiiiii				Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	 *	241		11110001	iiiiiiii				Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)
		242		11110010	iiiiiiii				Pop and Store Temporary Variable #iiiiiiii
	 *	243		11110011	iiiiiiii				Store Receiver Variable #iiiiiii (+ Extend A * 256)
	 *	244		11110100	iiiiiiii				Store Literal Variable #iiiiiiii (+ Extend A * 256)
		245		11110110	iiiiiiii				Store Temporary Variable #iiiiiiii

		252		11111100 	kkkkkkkk	jjjjjjjj	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
		253		11111101 	kkkkkkkk	jjjjjjjj	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 200
	  and: [byte <= 215
		 or: [(byte between: 240 and: 245)
		 or: [(byte between: 252 and: 253)]]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:18'!
isStorePopAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	"	200-207	11001 iii						Pop and Store Receiver Variable #iii
		208-215	11010 iii						Pop and Store Temporary Variable #iii
	 *	224		11100000	aaaaaaaa			Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	240		11110000	iiiiiiii				Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	 *	241		11110001	iiiiiiii				Pop and Store Literal Variable #iiiiiiii (+ Extend A * 256)
		242		11110010	iiiiiiii				Pop and Store Temporary Variable #iiiiiiii

		253		11111101 	kkkkkkkk	jjjjjjjj	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 200
	  and: [byte <= 215
		 or: [(byte between: 240 and: 242)
		 or: [byte = 253]]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 6/3/2024 10:29'!
isSuperSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send to super."

	"**	235		11101011	iiiiijjj	Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"

	^235 = (self nonExtensionBytecodeAt: pc in: method)! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 4/6/2017 11:25'!
isSyntheticStoreAt: pc in: method for: anInstructionStream
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores.  N.B. pcPreviousTo:in:for: is slow, so filter
	 out any preceding bytecodes other than what looks like a pushNewArrayCode.  But the
	 pcPreviousTo:in:for: is still necessary, since the presence of a pcPreviousTo:in:for: in the
	 right place is potentially ambiguous, possibly part of a different bytecode sequence."

	^(self isTempStoreAt: pc in: method)
	  and: [pc - 2 >= method initialPC
	  and: [(method at: pc - 2) = self pushNewArrayCode
	  and: [(method at: pc - 1) <= 127
	  and: [pc - 2 = (self pcPreviousTo: pc in: method for: anInstructionStream)]]]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 3/19/2019 09:11'!
isTempStoreAt: pc in: method
	"Answer if the bytecode at pc is a store or store-pop into a temporary variable.
	 208-215	11010 iii			Pop and Store Temporary Variable #iii
	 242		11110010	iiiiiiii		Pop and Store Temporary Variable #iiiiiiii
	 245		11110110	iiiiiiii		Store Temporary Variable #iiiiiiii"

	| byte |
	byte := method at: pc.
	^byte >= 208
	  and: [byte <= 215
			or: [byte = 242 or: [byte = 245]]]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 12/26/2020 19:27'!
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^bytecode! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 12/26/2020 19:27'!
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them.

	 The complication is that for convenience we allow the pc to point to the
	 raw send bytecode after its extension(s), or at the extension(s) preceding it.
	96-111		0110 iiii			Send Arithmetic Message #iiii (+ - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:)
	112-119	01110 iii			Send Special Message #iii + 0 (at: at:put: size next nextPut: atEnd == class)
	120-127	01111 iii			Send Special Message #iii + 8 (~~ value value: do: new new: x y)
	128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
	144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
	160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments
	*	224	11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	225	11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	**	234	11101010	iiiiijjj		Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
	**	235	11101011	iiiiijjj	ExtendB < 64
										ifTrue: [Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments]
										ifFalse: [Send To Superclass of Stacked Class Literal Selector #iiiii (+ Extend A * 32) with jjj (+ (Extend B bitAnd: 63) * 8) Arguments"

	| byte |
	byte := method at: pc.
	byte < 96 ifTrue:
		[^anInstructionStream].
	byte <= 175 ifTrue: 
		["special byte or short send"
		 ^byte >= 128
			ifTrue: [method literalAt: (byte bitAnd: 15) + 1]
			ifFalse: [Smalltalk specialSelectorAt: byte - 95]].
	byte < 234 ifTrue: "need to check for either extension cuz order of extensions is not restricted. so extB could precede extA"
		[(byte >= 224 and: [byte <= 225]) ifTrue:
			[^self extensionsAt: pc in: method into:
				[:extA :extB :nExtBytes| | byteAfter index |
				byteAfter := method at: pc + nExtBytes.
				(byteAfter >= 234 and: [byteAfter <= 235])
					ifTrue:
						[index := ((method at: pc + nExtBytes + 1) bitShift: -3) + (extA bitShift: 5).
						 method literalAt: index + 1]
					ifFalse: [anInstructionStream]]].
		^anInstructionStream].
	byte > 235 ifTrue:
		[^anInstructionStream].
	"they could be extended..."
	^self extensionsFor: pc in: method into:
		[:extA :extB :nExtBytes| | index |
		 index := ((method at: pc + 1) bitShift: -3) + (extA bitShift: 5).
		 method literalAt: index + 1]! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'eem 5/18/2014 19:21'!
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."

	"*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 **	235		11101011	iiiiijjj		Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
					
	^[:instr | instr = 235]! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 5/18/2014 19:22'!
callPrimitiveCode
	"Answer the call primitive bytecode, if it exists in the encoder's bytecode set, or nil if not.
	 248	11111000 	iiiiiiii	mjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjj * 256) m=1 means inlined primitive, no hard return after execution."
	^248! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 3/29/2017 09:36'!
createClosureCode
	"Answer the create closure bytecode, if it exists in the encoder's bytecode set, or nil if not.
	 Actually this code is that for a closure whose bytecodes are nested within its home method's."

	^250! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 5/18/2014 19:23'!
pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method
	"Answer the pc of the push closure bytecode whose block starts at startpc in method.
	 May need to back up to include extension bytecodes."

	"*	224		11100000	aaaaaaaa			Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	225		11100001	bbbbbbbb			Extend B (Ext B = Ext B prev * 256 + Ext B)
	 **	250		11111010 	eeiiikkk		jjjjjjjj	Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
	| numExtensions |
	self assert: (method at: startpc - 3) = 250.
	numExtensions := (method at: startpc - 2) >> 6.
	^startpc - 3 - (numExtensions * 2)! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 5/18/2014 19:24'!
pushClosureBytecodeSize
	"Answer the size of the push closure bytecode.
	 **	250		11111010 	eeiiikkk		jjjjjjjj	Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
	^3! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 4/6/2017 11:26'!
pushNewArrayCode
	"231		11100111	jkkkkkkk	Push (Array new: kkkkkkk) (j = 0)
									&	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	^231! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 4/6/2017 14:15'!
pushNilCode
	"Answer the pushNil bytecode.
	 79			01001111			Push nil"
	^79! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 8/8/2014 19:40'!
stackDeltaForPrimitive: primitiveIndex in: method
	"Answer the stack delta for the callPrimitive: bytecode (see my class comment).
	 There is no delta for non-inlined primitives (its implicitly 0 - method numArgs).
	 Inlined primitives are grouped by the thousand by argument count, 32 args max ;-)."
	^primitiveIndex < 32678
		ifTrue: [0]
		ifFalse: [primitiveIndex - 32768 // 1000]! !


!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'eem 5/18/2014 19:24'!
unusedBytecode
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^223! !


!EncoderForSistaV1 class methodsFor: 'scanning' stamp: 'eem 7/6/2024 19:01'!
scanBlockOrNilForLiteral: aLiteral
	"Answer a block argument for CompiledMethod>>#scanFor: that answers
	 if the method refers to the literal implicitly via a special bytecode.
	 If the literal is not accessible via a special bytecode, answer nil."
	| value hi lo unextended |

	"96-111	0110 iiii			Send Arithmetic Message #iiii (+ - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:)
	 112-119	01110 iii			Send Special Message #iii + 0 (at: at:put: size next nextPut: atEnd == class)
	 120-127	01111 iii			Send Special Message #iii + 8 #(#'~~' #value #value: #do: #new #new: #x #y)"
	self flag: #deprecated. "mt: Use CompiledCode>>#implicitLiteralsDo:."
	aLiteral isSymbol ifTrue:
		[value := 96 + ((Smalltalk specialSelectors indexOf: aLiteral ifAbsent: [^nil]) // 2).
		 ^[:byte| byte = value]].

	"80			01010000			Push 0
	 81			01010001			Push 1
	 232		11101000	iiiiiiii	Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, d=0, s=1)"
	aLiteral isInteger ifTrue:
		[aLiteral >= 0 ifTrue:
			[aLiteral <= 1 ifTrue:
				[value := aLiteral + 80.
				 ^[:byte| byte = value]].
			 aLiteral <= 255 ifTrue:
				[unextended := true. "Don't be fooled by extended cases with the same least significant byte!!"
				 ^[:b1 :b2| | found |
					found := b1 = 232 and: [b2 = aLiteral and: [unextended]].
					unextended := b1 ~= 16rE1.
					found]]].
		 (aLiteral between: -32768 and: 32767) ifFalse: [^nil].
		 lo := aLiteral bitAnd: 255.
		 hi := (aLiteral bitShift: -8) bitAnd: 255.
		 ^[:b1 :b2 :b3 :b4| b1 = 16rE1 and: [b2 = hi and: [b3 = 232 and: [b4 = lo]]]]].

	"233		11101001	iiiiiiii	Push Character #iiiiiiii (+ Extend B * 256)"
	aLiteral isCharacter ifTrue:
		[(value := aLiteral asInteger) <= 255 ifTrue:
			[unextended := true. "Don't be fooled by extended cases with the same least significant byte!!"
			 ^[:b1 :b2| | found |
				found := b1 = 233 and: [b2 = value and: [unextended]].
				unextended := b1 ~= 16rE1.
				found]].
		 ^value <= 65535 ifTrue:
			[lo := value bitAnd: 255.
			 hi := (value bitShift: -8) bitAnd: 255.
			 [:b1 :b2 :b3 :b4| b1 = 16rE1 and: [b2 = hi and: [b3 = 233 and: [b4 = lo]]]]]].

	"77			01001101			Push true
	 78			01001110			Push false
	 79			01001111			Push nil
	 88-91		010110 ii			Return Receiver/true/false/nil
	 93			01011101			BlockReturn nil"
	aLiteral == true ifTrue:
		[^[:byte| byte = 77 or: [byte = 89]]].
	aLiteral == false ifTrue:
		[^[:byte| byte = 78 or: [byte = 90]]].
	aLiteral == nil ifTrue:
		[^[:byte| byte = 79 or: [byte = 91 or: [byte = 93]]]].
	^nil! !


!EncoderForSistaV1 class methodsFor: 'testing' stamp: 'eem 10/9/2021 17:28'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."
	
	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral asInteger <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!EncoderForV3PlusClosures methodsFor: 'testing' stamp: 'eem 4/6/2017 10:39'!
supportsFullBlocks
	"Answer if the instruction set supports full closures (closure creation from
	 specfic methods instead of bytecodes embedded in an outer home method)."
	
	^false! !


!EncoderForV3PlusClosures methodsFor: 'special literal encodings' stamp: 'eem 8/12/2017 15:20'!
isSpecialLiteralForPush: literal
	^literal isInteger
		ifFalse:
			[false == literal
			 or: [true == literal
			 or: [nil == literal]]]
		ifTrue: [literal between: -1 and: 2]! !


!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'eem 4/6/2017 14:13'!
pushNilCode
	"Answer the pushNil bytecode.
	 112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	^115! !


!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 7/5/2022 15:55'!
isReturnSelfFromMethodAt: pc in: method
	"Answer whether the bytecode at pc is a return self from method."

	^(method at: pc) = 120! !


!EncoderForV3PlusClosures class methodsFor: 'instruction stream support' stamp: 'eem 6/3/2024 10:25'!
isSuperSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send to super."

	"132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	 133 		10000101 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"

	| byte |
	byte := method at: pc.
	^byte = 133	"double extended do anything"
	  or: [byte = 132 and: [(method at: pc + 1) // 32 = 1]]! !


!EncoderForV3PlusClosures class methodsFor: 'scanning' stamp: 'mt 12/20/2023 13:51'!
scanBlockOrNilForLiteral: aLiteral
	"Answer a block argument for CompiledMethod>>#scanFor: that answers
	 if the method refers to the literal implicitly via a special bytecode.
	 If the literal is not accessible via a special bytecode, answer nil."
	| value |
	"176-191 	1011iiii 	Send Arithmetic Message #iiii
	 192-207 	1100iiii 	Send Special Message #iiii"
	self flag: #deprecated. "mt: Use CompiledCode>>#implicitLiteralsDo:."
	(aLiteral isSymbol or: [aLiteral isInteger]) ifTrue:
		[value := aLiteral isSymbol
					ifTrue: [176 + ((Smalltalk specialSelectors indexOf: aLiteral ifAbsent: [^nil]) // 2)]
					ifFalse: [(aLiteral between: -1 and: 2) ifFalse: [^nil].
							aLiteral + 117].
		 ^[:byte| byte = value]].
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	 120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	aLiteral == true ifTrue:
		[^[:byte| byte = 113 or: [byte = 121]]].
	aLiteral == false ifTrue:
		[^[:byte| byte = 114 or: [byte = 122]]].
	aLiteral == nil ifTrue:
		[^[:byte| byte = 115 or: [byte = 123]]].
	^nil! !


!EncoderForV3PlusClosures class methodsFor: 'testing' stamp: 'eem 10/9/2021 17:28'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."

	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isInteger ifTrue: [^ aLiteral between: -1 and: 2].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !


!AssignmentNode methodsFor: 'private' stamp: 'mt 9/4/2019 09:20'!
ifNilTemporary
	"(temp := object) == nil ifTrue: [...] ifFalse: [...]"
	
	^ self variable! !


!BraceNode methodsFor: 'closure analysis' stamp: 'eem 1/18/2020 17:33'!
deoptimize
	"Deoptimize the blocks in a caseOf:[otherwise:] that is being used in a cascade."
	elements do:
		[:aMessage|
		self assert: aMessage selector key == #->.
		aMessage receiver deoptimize.
		aMessage arguments first deoptimize]! !


!BlockNode methodsFor: 'accessing' stamp: 'eem 12/24/2024 16:46:15'!
ensureClosureCreationNode: ignored
	^self closureCreationNode! !


!MethodNode methodsFor: 'converting-private' stamp: 'eem 1/16/2025 17:12:45'!
preenIfNotNilNode: messageNode preenableNodes: preenableNodes
	"Transform a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| variable relevantNodes |
	self assert: (messageNode isMessageNode
				and: [messageNode macroPrinter == #printIfNilNotNil:indent:
				and: [messageNode receiver receiver isAssignmentNode]]).
	variable := messageNode receiver receiver variable.
	self assert: (variable isTemp and: [variable isRemote not]).
	relevantNodes := preenableNodes keys select:
						[:node| 
						node isMessageNode
						and: [messageNode macroPrinter == #printIfNilNotNil:indent:]].
	(variable isLocalToBlocksInIfNilIfNotNilNodes: relevantNodes in: self) ifFalse:
		[messageNode arguments do:
			[:argBlock|
			(argBlock arguments size = 1
			and: [argBlock firstArgument = variable]) ifTrue:
				[argBlock arguments: #()]].
		 variable beTemp.
		 ^self].
	messageNode arguments last arguments isEmpty
		ifTrue: [messageNode arguments last arguments: { variable }]
		ifFalse:
			[self assert: messageNode arguments last arguments asArray = { variable }.
			 variable := nil].
	messageNode receiver receiver: messageNode receiver receiver value.
	(temporaries includes: variable) ifTrue: "can't use removeAtIndex: cuz temporaries could be an Array"
		[temporaries := temporaries copyWithout: variable].
	variable ifNil: [^self].
	self nodesDo:
		[:node|
		((node == self or: [node isBlockNode])
		 and: [node temporaries anySatisfy: [:temp| temp = variable]]) ifTrue:
			[node temporaries: (node temporaries reject: [:temp| temp = variable])]]! !


!MethodNode methodsFor: 'converting-private' stamp: 'eem 12/29/2024 17:44:13'!
preenNilNodeFollowingNilIfNode: aNilIfMessageNode preenableNodes: preenableNodes
	self nodesDo:
		[:node| | statements indices |
		 (node isBlockNode
		  and: [(statements := node statements) includes: aNilIfMessageNode]) ifTrue:
			[indices := (2 to: statements size) reject:
							[:i|
							(statements at: i) == NodeNil
							and: [(statements at: i - 1) isNilIf]].
			 node statements: (({1}, indices) collect: [:i| statements at: i])]]! !


!MethodNode methodsFor: 'converting-private' stamp: 'eem 12/29/2024 17:44:18'!
preenTempsConflictingWithBlockNode: temps preenableNodes: preenableNodes
	"Push temps that conflict with other blocks down into their narrowest enclosing block scope."
	temps do:
		[:tempVar|
		(self accept: (NarrowerVariableScopeFinder new ofVariable: tempVar)) ifNotNil:
			[:enclosingScope |
			 self assert: enclosingScope isBlockNode.
			 self nodesDo:
				[:node|
				 ((node == self or: [node isBlockNode])
				  and: [node temporaries includes: tempVar]) ifTrue:
					[node temporaries: (node temporaries copyWithout: tempVar)]].
			 enclosingScope temporaries: enclosingScope temporaries, { tempVar }]]! !


!MethodNode methodsFor: 'converting-private' stamp: 'eem 12/29/2024 17:52:24'!
preenableNodes
	"Answer a Dictionary from node or sequence of nodes to preen method selector for nodes in
	 the tree that require post-processing after either a format or a decompile.  Such issues are
	 the variable for an ifNotNil: which is local to the ifNotNil: block but, due to the inlining of
	 ifNotNil: appears to be declared at the outer level, and, similarly, a temporary variable that
	 conflicts with one of the same name in a block when, were the variable declared local to
	 some inlined block it would no longer conflict.  The resulting dictionary is used to perform
	 the value with the key (node or array) and the dictionary as arguments to preen the tree."

	| preenableNodes priorBlocks priorVariables |
	preenableNodes := Dictionary new.
	priorBlocks := OrderedCollection new.
	priorVariables := Set new.
	self nodesWithPrecedingStatementsDo:
		[:node :precedingStatementOrNil| | variable temps |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not]]]]]]) ifTrue:
			[preenableNodes at: node put: #preenIfNotNilNode:preenableNodes:.
			 priorVariables add: variable].
		node isBlockNode ifTrue:
			[temps := OrderedCollection new.
			 node temporaries do:
				[:temp|
				 priorBlocks do:
					[:aBlock|
					 aBlock temporaries do:
						[:priorTemp|
						 (priorVariables includes: priorTemp) ifFalse:
							[priorTemp key = temp key ifTrue:
								[temps addLast: priorTemp]]]]].
			 temps isEmpty ifFalse:
				[preenableNodes at: temps put: #preenTempsConflictingWithBlockNode:preenableNodes:].
			 priorBlocks addLast: node].
		(node == NodeNil
		 and: [precedingStatementOrNil notNil
		 and: [precedingStatementOrNil isMessageNode
		 and: [precedingStatementOrNil isNilIf]]]) ifTrue:
			[preenableNodes at: precedingStatementOrNil put: #preenNilNodeFollowingNilIfNode:preenableNodes:]].
	^preenableNodes! !


!TempVariableNode methodsFor: 'testing' stamp: 'eem 12/29/2024 18:05:06'!
beTemp
	"This is for preening of ifNil:ifNotNil:"
	argType := nil! !


!TempVariableNode methodsFor: 'testing' stamp: 'eem 1/16/2025 11:58:00'!
isLocalToBlocksInIfNilIfNotNilNodes: ifNilIfNotNilMessages "<Collection of: MessageNode>" in: aMethodNode "<MethodNode>"
	"Answer if the receiver only occurs within the ifNotNil: blocks of the message nodes,
	 or is the target of the assignment that is the receiver of each node.
	 Assumes that all ifNilIfNotNilMessages are in fact subnodes of aMethodNode.

	Strategy:
		First visit all nodes outside of the message nodes. If the variable is referenced there answer false.
		Next visit the expressions of all assignments that are the receivers of the ifNilIfNotNil: messages.
		If the variable is referenced there answer false.
		If not found outside, answer true. This does not require the variable to be referenced within the
		block, but that is not necessary to produce a nicely preened tree."
	| ifNotNilBlocks ifNotNilAssignments finder |
	ifNotNilBlocks := Set new.
	ifNotNilAssignments := Set new.
	ifNilIfNotNilMessages do:
		[:messageNode|
		(messageNode receiver isMessageNode
		 and: [messageNode receiver selector key == #==
		 and: [messageNode receiver receiver isAssignmentNode
		 and: [messageNode receiver receiver variable = self]]]) ifTrue:
			[ifNotNilAssignments add: messageNode receiver receiver].
		ifNotNilBlocks add: (messageNode arguments at: (messageNode selector key keywords indexOf: #ifNotNil:))].
	finder := ParseNodeEnumerator
				ofBlock: [:node| node == self ifTrue: [^false]]
				select: [:node| ((node isBlockNode and: [ifNotNilBlocks includes: node])
								or: [node isAssignmentNode and: [ifNotNilAssignments includes: node]]) not].
	aMethodNode accept: finder.
	ifNotNilAssignments do:
		[:assignmentNode|
		assignmentNode value accept: finder].
	^true! !


!MessageNode methodsFor: 'private' stamp: 'mt 9/4/2019 09:27'!
ifNilTemporary

	^ self ifNilReceiver ifNilTemporary! !


!MessageNode methodsFor: 'decompiling' stamp: 'eem 7/11/2024 18:10'!
toDoFromWhileWithCounts: blockBodyTempCounts init: incrInit limit: limitInitOrNil
	"If the receiver, a whileTrue: loop, represents a to:[by:]do: loop
	 then answer the replacement to:[by:]do:, otherwise answer nil."
	| variable increment limit toDoBlock body test |
	self assert: (selector key == #whileTrue:
				and: [incrInit isAssignmentNode]).
	(limitInitOrNil notNil "limit should not be referenced within the loop"
	  and: [(blockBodyTempCounts at: limitInitOrNil variable ifAbsent: [0]) ~= 1]) ifTrue:
		[^nil].
	body := arguments last statements.
	(variable := incrInit variable) isTemp ifFalse:
		[^nil].
	(increment := body last toDoIncrement: variable) ifNil:
		[^nil].
	receiver statements size ~= 1 ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test should really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable is: limit]]]) ifTrue:
		[^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: {variable}.
	^MessageNode new
		receiver: incrInit value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: { limit. increment. toDoBlock }
		precedence: precedence! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'mt 10/9/2019 09:42'!
decodeIfNilWithReceiver: receiver selector: selector arguments: arguments tempReadCounts: tempReadCounts
	
	| node temp |
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
				
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
		
	"Like #to:(by:)do:, support only local temps."
	(((temp := receiver ifNilTemporary) isNil or: [tempReadCounts includesKey: temp]) or: [
		"What about 'object ifNotNil: [:o | ]', which as not read the blockArg? Just check that there is no remote vector pointing to it."
		tempReadCounts keys noneSatisfy:
			[:otherTemp |
				otherTemp isIndirectTempVector
					ifTrue: [otherTemp remoteTemps anySatisfy:
						[:remoteTemp |
						remoteTemp name = temp name]]
					ifFalse: [otherTemp name = temp name]]
			])
		ifFalse: [^ nil].
		
	node := (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3).

	"Reconfigure the message node to #ifNil:ifNotNil:. Note that original* instance variables keep their optimized format. See MessageNode >> #printIfNilNotNil:indent:."	
	node
		noteSpecialSelector: #ifNil:ifNotNil:;
		selector: (SelectorNode new key: #ifNil:ifNotNil:).
	
	temp ifNil: [^ node].
	temp isTemp ifFalse: [^ node].
	
	(arguments second isJust: NodeNil) not ifTrue:
		[temp beBlockArg.
		node arguments: {
			arguments first.
			arguments second copy arguments: { temp }; yourself }].
				
	^ node! !


!ParseNodeWithPrecedingStatementEnumerator methodsFor: 'initialize-release' stamp: 'eem 3/19/2019 11:50'!
ofBlock: aBlock
	"N.B. This enumerator visits a node before any of the node's children.
	 Hence, when enumerating statements in a block, we can ensure that
	 the second argument to the block, the preceding statement, is non-nil
	 only for top-level statements in the block by nilling out precedingStatement
	 once the block is evaluated. Perhaps stronger would be to capture its value
	 in a temporary and nil it before evaluating, but this is good enough."
	theBlock := [:node|
				aBlock value: node value: precedingStatement.
				precedingStatement := nil]! !


!ParseNodeWithPrecedingStatementEnumerator methodsFor: 'initialize-release' stamp: 'eem 12/24/2017 10:32'!
ofBlock: aBlock select: aSelectBlock
	self ofBlock: aBlock.
	theSelectBlock := aSelectBlock! !


!ParseNodeWithPrecedingStatementEnumerator methodsFor: 'visiting' stamp: 'eem 1/3/2019 15:46'!
visitBlockNode: aBlockNode
	| savedPrecedingStatement |
	(theSelectBlock isNil or: [theSelectBlock value: aBlockNode]) ifFalse:
		[^nil].
	theBlock value: aBlockNode.
	savedPrecedingStatement := precedingStatement.
	precedingStatement := nil.
	[aBlockNode statements do:
		[:statement|
		 statement accept: self.
		 precedingStatement := statement]] ensure:
		[precedingStatement := savedPrecedingStatement]! !


!NarrowerVariableScopeFinder methodsFor: 'visiting' stamp: 'eem 12/29/2024 18:34:35'!
visitTempVariableNode: aVariableNode
	^(theVariable is: aVariableNode) ifTrue: [theVariable]! !


!NarrowerVariableScopeFinder methodsFor: 'visiting' stamp: 'eem 4/3/2017 12:00'!
visitUndeclaredVariableNode: aVariableNode
	^nil! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 2/6/2022 20:32'!
arguments

	^ arguments! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 2/6/2022 20:04'!
context

	^ context! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 2/6/2022 20:32'!
method

	^ method! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 5/2/2021 16:01'!
primitive

	^ primitiveIndex! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 5/9/2021 20:14'!
suppress

	suppressed := true.! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 5/9/2021 20:14'!
suppressed

	^ suppressed ifNil: [self isSimulationGuard not]! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 2/6/2022 20:33'!
theReceiver

	^ receiver! !


!SimulationSideEffectWarning methodsFor: 'accessing' stamp: 'ct 5/9/2021 20:14'!
unsuppress

	suppressed := false.! !


!SimulationSideEffectWarning methodsFor: 'initialize-release' stamp: 'ct 2/6/2022 20:24'!
context: aContext method: aCompiledMethod receiver: rcvr arguments: args

	context := aContext.
	method := aCompiledMethod.
	receiver := rcvr.
	arguments := args.! !


!SimulationSideEffectWarning methodsFor: 'initialize-release' stamp: 'ct 2/6/2022 20:23'!
primitive: anInteger

	primitiveIndex := anInteger.! !


!SimulationSideEffectWarning methodsFor: 'priv handling' stamp: 'ct 3/2/2024 23:47'!
defaultAction

	self suppressed ifFalse: [super defaultAction].
	self flag: #forLater. "When we support explicit exception handler invocation (e.g., #resume, #retry) from the debugger, this exception should publish a #resume handler rather than relying on the weakly defined proceed semantics of the debugger."
	^ self defaultResumeValue! !


!SimulationSideEffectWarning methodsFor: 'defaults' stamp: 'ct 2/6/2022 20:34'!
defaultResumeValue

	^ true! !


!SimulationSideEffectWarning methodsFor: 'testing' stamp: 'ct 5/9/2021 20:43'!
isControlPrimitive
	"See StackInterpreter class>>#initializePrimitiveTable."

	^ self primitive between: 80 and: 89! !


!SimulationSideEffectWarning methodsFor: 'testing' stamp: 'ct 5/9/2021 20:43'!
isSimulationGuard
	"See Parser >> #simulationGuard."

	^ self primitive = 19! !


!SimulationSideEffectWarning methodsFor: 'printing' stamp: 'ct 3/3/2024 19:24'!
messageText

	^ messageText ifNil: [
		'The code being simulated is trying to control a process ({1}). {2}' translated format: {
			self context method reference.
			self isSimulationGuard
				ifTrue: ['If you proceed, your image may be locked. Continue at own risk, and better save your image before.' translated]
				ifFalse: ['Process controlling cannot be simulated. If you proceed, side effects may occur outside the observable area of the simulator.' translated]}]! !


!SimulationSideEffectWarning methodsFor: 'signaling' stamp: 'ct 2/6/2022 20:38'!
signalIfSkipped: skipBlock

	^ self signal ifFalse: skipBlock! !


!SimulationSideEffectWarning methodsFor: 'handling' stamp: 'ct 2/6/2022 20:37'!
skipPrimitive

	^ self resume: false! !


!SimulationSideEffectWarning class methodsFor: 'instance creation' stamp: 'ct 2/6/2022 20:22'!
forPrimitive: primitiveIndex

	^ self new primitive: primitiveIndex! !


!ImplicitLiteralInstructionClientHook methodsFor: 'instruction decoding - implicit literals' stamp: 'mt 12/20/2023 13:19'!
methodReturnSpecialConstant: value
	"This is a hook to allow clients to intercept implicit literal bytecodes"
	^self methodReturnConstant: value! !


!ImplicitLiteralInstructionClientHook methodsFor: 'instruction decoding - implicit literals' stamp: 'eem 9/12/2021 14:46'!
pushSpecialConstant: value
	"This is a hook to allow clients to intercept implicit literal bytecodes"
	^self pushConstant: value! !


!ImplicitLiteralInstructionClientHook methodsFor: 'instruction decoding - implicit literals' stamp: 'eem 9/12/2021 14:19'!
sendSpecial: selector numArgs: numArgs
	"This is a hook to allow clients to intercept special selector sends (which have an implicit selector)"
	^self send: selector super: false numArgs: numArgs! !


!BlockLocalTempCounter methodsFor: 'instruction decoding' stamp: 'eem 8/1/2014 07:11'!
blockReturnConstant: value
	"Return Constant From Block bytecode."
	scanner pc < blockEnd ifTrue:
		[self doJoin]! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 8/1/2014 07:21'!
blockReturnConstant: value 
	"Print the Return Constant From Block bytecode."

	self print: 'blockReturn: ', value printString! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 4/6/2015 12:00'!
directedSuperSend: selector "<Symbol>" numArgs: numArgs "<SmallInteger>"
	self print: 'directedSuperSend: ' , (self stringForSelector: selector numArgs: numArgs)! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 8/5/2014 11:38'!
trapIfNotInstanceOf: behaviorOrArrayOfBehavior
	"If the top of stack is not an instance of either the argument, or, if the argument is an Array,
	  any of the elements of the argument, send the class trap message to the current context."
	self print: 'trapIfNotInstanceOf: ', behaviorOrArrayOfBehavior printString! !


!InstructionPrinter methodsFor: 'printing' stamp: 'eem 1/1/2023 12:04'!
stringForReceiverVariableOffset: offset
	| tmpStream |
	tmpStream := WriteStream on: (String new: 16).
	offset printOn: tmpStream.
	method methodClass ifNotNil:
		[:class|
		class isBehavior ifTrue:
			[(class instVarNameForIndex: offset + 1) ifNotNil:
				[:instVarName|
				tmpStream nextPutAll: ' "'; nextPutAll: instVarName; nextPut: $"]]].
	^tmpStream contents! !


!InstructionPrinter methodsFor: 'printing' stamp: 'eem 3/15/2013 15:14'!
stringForSelector: selector numArgs: numArgs
	^(selector isSymbol and: [selector numArgs = numArgs])
		ifTrue: [selector]
		ifFalse: [selector printString
				, (numArgs = 1
					ifTrue: [' (1 arg)']
					ifFalse: [' (', numArgs printString, ' args)'])]! !


!InstructionPrinter methodsFor: '*BytecodeSets-SistaV1-decoding' stamp: 'cb 1/19/2017 14:10'!
branchIfInstanceOf: behaviorOrArrayOfBehavior distance: distance
	"If the object on top of stack has the type -or one of the type- present in the literal
	 (the literal is a behavior or an array of behavior), then pop it. Else jumps by the distance."
	self print: 'branchIfInstanceOf: ', behaviorOrArrayOfBehavior printString, ' distance: ', distance printString! !


!InstructionPrinter methodsFor: '*BytecodeSets-SistaV1-decoding' stamp: 'eem 7/11/2016 14:24'!
branchIfNotInstanceOf: behaviorOrArrayOfBehavior distance: distance
	"If the object on top of stack has the type -or one of the type- present in the literal
	 (the literal is a behavior or an array of behavior), then pop it. Else jumps by the distance."
	self print: 'branchIfNotInstanceOf: ', behaviorOrArrayOfBehavior printString, ' distance: ', distance printString! !


!InstructionPrinter methodsFor: '*BytecodeSets-instruction decoding' stamp: 'eem 5/14/2020 13:32'!
callInlinePrimitive: index
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
											ifNone: [{index printString}]) last)! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'nice 3/10/2022 09:50'!
interpretNext2ByteSistaV1Instruction: bytecode for: client extA: extA extB: extB startPC: startPC
	"Send to the argument, client, a message that specifies the next instruction.
	 This method handles the two-byte codes.
	 For a table of the bytecode set, see EncoderForV1's class comment."

	| byte method |
	method := self method.
	byte := self method at: pc.
	pc := pc + 1.
	"We do an inline quasi-binary search on bytecode"
	bytecode < 234 ifTrue: "pushes"
		[bytecode < 231 ifTrue:
			[bytecode < 229 ifTrue:
				[| literal |
				 bytecode = 226 ifTrue:
					[^client pushReceiverVariable: (extA bitShift: 8) + byte].
				 literal := method literalAt: (extA bitShift: 8) + byte + 1.
				 bytecode = 227 ifTrue:
					[^client pushLiteralVariable: literal].
				 ^client pushConstant: literal].
			bytecode = 229 ifTrue:
				[^client pushTemporaryVariable: byte].
			^self unusedBytecode: client at: startPC].
		bytecode = 231 ifTrue:
			[^byte < 128
				ifTrue: [client pushNewArrayOfSize: byte]
				ifFalse: [client pushConsArrayWithElements: byte - 128]].
		bytecode = 232 ifTrue:
			[^client pushSpecialConstant: ((extB < 128 ifTrue: [extB] ifFalse: [extB - 256]) bitShift: 8) + byte].
		^client pushSpecialConstant: (Character value: ((extB bitAnd: 16rFF) bitShift: 8) + byte)].
	bytecode < 240 ifTrue: "sends, trap and jump"
		[bytecode < 236 ifTrue: "sends"
			[(bytecode = 235 and: [extB >= 64]) ifTrue:
				[^client
					directedSuperSend: (method literalAt: (extA bitShift: 5) + (byte // 8) + 1)
					numArgs: (extB - 64 bitShift: 3) + (byte \\ 8)].
			 ^client
				send: (method literalAt: (extA bitShift: 5) + (byte // 8) + 1)
				super: bytecode = 235
				numArgs: (extB bitShift: 3) + (byte \\ 8)].
		 bytecode = 236 ifTrue:
			[^client callMappedInlinedPrimitive: byte].
		bytecode = 237 ifTrue:
			[^client jump: (extB bitShift: 8) + byte].
		 ^client jump: (extB bitShift: 8) + byte if: bytecode = 238].
	bytecode < 243 ifTrue:
		[bytecode = 240 ifTrue:
			[^client popIntoReceiverVariable: (extA bitShift: 8) + byte].
		 bytecode = 241 ifTrue:
			[^client popIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
		 ^client popIntoTemporaryVariable: byte].
	bytecode = 243 ifTrue:
		[^client storeIntoReceiverVariable: (extA bitShift: 8) + byte].
	bytecode = 244 ifTrue:
		[^client storeIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
	bytecode = 245 ifTrue:
		[^client storeIntoTemporaryVariable: byte].
	"246-247	1111011 i	xxxxxxxx	UNASSIGNED"
	^self unusedBytecode: client at: startPC! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'eem 3/23/2017 16:32'!
interpretNext3ByteSistaV1Instruction: bytecode for: client extA: extA extB: extB startPC: startPC
	"Send to the argument, client, a message that specifies the next instruction.
	 This method handles the three-byte codes.
	 For a table of the bytecode set, see EncoderForSistaV1's class comment."

	| method byte2 byte3 literal |
	method := self method.
	byte2 := method at: pc.
	byte3 := method at: pc + 1.
	pc := pc + 2.

	"**	248	(2)	11111000 	iiiiiiii		mssjjjjj		Call Primitive #iiiiiiii + (jjjjj * 256) 
									m=1 means inlined primitive, no hard return after execution. 
									ss defines the unsafe operation set used to encode the operations. 
									(ss = 0 means sista unsafe operations, ss = 01 means lowcode operations, other numbers are not used)
									Lowcode inlined primitives may have extensions."
	bytecode = 248 ifTrue:
		[| primitiveSetSelector primitiveNumber |
		 byte3 < 128 ifTrue:
			[ "Maybe this should be restricted to the 13 bit primitiveNumber too..."
			 ^client callPrimitive: byte2 + (byte3 bitShift: 8)].
		 primitiveSetSelector := (byte3 bitShift: -5) bitAnd: 3.
		 primitiveNumber := byte2 + ((byte3 bitAnd: 31) bitShift: 8).
		 primitiveSetSelector = 0 ifTrue: "Sista inline primitives"
			[^client callInlinePrimitive: byte2 + (byte3 - 128 bitShift: 8)].
		 primitiveSetSelector = 1 ifTrue: "Lowcode inline primitives"
			[^client callLowcodeInlinePrimitive: byte2 + (byte3 - 128 bitShift: 8) extA: extA extB: extB].
		 "fall through to ^self unusedBytecode: client at: startPC below"].

	"*	249		11111001	xxxxxxxx	siyyyyyy	push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1"
	bytecode = 249 ifTrue:
		[literal := method literalAt: (extA bitShift: 8) + byte2 + 1.
		 (byte3 noMask: 16rC0) ifTrue:
			[^client pushFullClosure: literal numCopied: byte3].
		 ^client
			pushFullClosure: literal
			numCopied: (byte3 bitAnd: 16r3F)
			receiverOnStack: (byte3 anyMask: 16r80)
			ignoreOuterContext: (byte3 anyMask: 16r40)].
	bytecode = 250 ifTrue:
		["**	250  11111010  eeiiikkk  jjjjjjjj  Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
		 ^client
			pushClosureCopyNumCopiedValues: ((byte2 bitShift: -3) bitAnd: 7) + (extA // 16 bitShift: 3)
			numArgs: (byte2 bitAnd: 7) + (extA \\ 16 bitShift: 3)
			blockSize: byte3 + (extB bitShift: 8)].
	bytecode = 251 ifTrue:
		[^client pushRemoteTemp: byte2 inVectorAt: byte3].
	bytecode = 252 ifTrue:
		[^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	bytecode = 253 ifTrue:
		[^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"**	254		11111110	kkkkkkkk	jjjjjjjj		branch If Not Instance Of Behavior/Array Of Behavior kkkkkkkk (+ Extend A * 256, where Extend A >= 0) distance jjjjjjjj (+ Extend B * 256, where Extend B >= 0)"
	bytecode = 254 ifTrue: "The sign bit of extB inverts the operation.  Would like to have extB < -128, but this is good enough for now."
		[literal := method literalAt: (extA bitShift: 8) + byte2 + 1.
		 extB < 0 ifTrue: [^client branchIfInstanceOf: literal distance: (extB + 128 bitShift: 8) + byte3].
		 ^client branchIfNotInstanceOf: literal distance: (extB bitShift: 8) + byte3].
	^self unusedBytecode: client at: startPC! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'mt 12/20/2023 13:20'!
interpretNextSistaV1InstructionFor: client
	"Send to the argument, client, a message that specifies the next instruction."

	| byte div16 offset method extA extB savedPC |
	method := self method.
	"For a table of the bytecode set, see EncoderForSistaV1's class comment."
	"consume and compute any extensions first."
	extA := extB := 0.
	savedPC := pc.
	[byte := self method at: pc.
	 pc := pc + 1.
	 byte >= 16rE0 and: [byte <= 16rE1]] whileTrue:
		[| extByte |
		 extByte := self method at: pc.
		 pc := pc + 1.
		 byte = 16rE0
			ifTrue:
				[extA := (extA bitShift: 8) + extByte]
			ifFalse:
				[extB := (extB = 0 and: [extByte > 127])
							ifTrue: [extByte - 256]
							ifFalse: [(extB bitShift: 8) + extByte]]].
	div16 := byte // 16.
	offset := byte \\ 16.
	"We do an inline quasi-binary search on each of the possible 16 values of div16"
	div16 < 11 ifTrue:
		[div16 < 6 ifTrue:
			[div16 < 4 ifTrue:
				[div16 < 2 ifTrue:
					[div16 = 0 ifTrue:
						 [^client pushReceiverVariable: offset].
					^client pushLiteralVariable: (method literalAt: offset + 1)]. "div16 = 1"
				 ^client pushConstant: (method literalAt: byte \\ 32 + 1)].
			 div16 = 4 ifTrue:
				[offset < 12 ifTrue:
					[^client pushTemporaryVariable: offset].
				 offset = 12 ifTrue:
					[^client pushReceiver].
				 offset = 13 ifTrue:
					[^client pushSpecialConstant: true].
				 offset = 14 ifTrue:
					[^client pushSpecialConstant: false].
				 offset = 15 ifTrue:
					[^client pushSpecialConstant: nil]].
			"div16 = 5"
			 offset < 2 ifTrue:
				[^client pushSpecialConstant: offset].
			 offset = 2 ifTrue:
				[^self interpretSistaV1ExtendedPush: extB for: client].
			 offset = 3 ifTrue:
				[^client doDup].
			 offset = 8 ifTrue:
				[^client methodReturnReceiver].
			 offset = 9 ifTrue:
				[^client methodReturnSpecialConstant: true].
			 offset = 10 ifTrue:
				[^client methodReturnSpecialConstant: false].
			 offset = 11 ifTrue:
				[^client methodReturnSpecialConstant: nil].
			 offset = 12 ifTrue:
				[^client methodReturnTop].
			 offset = 13 ifTrue:
				[^client blockReturnConstant: nil].
			 offset = 14 ifTrue:
				[^client blockReturnTop].
			 offset = 15 ifTrue:
				[^client doNop].
			 ^self unusedBytecode: client at: savedPC].
		"short sends"
		div16 = 6 ifTrue:
			[^client
				sendSpecial: (Smalltalk specialSelectorAt: offset + 1)
				numArgs: (Smalltalk specialNargsAt: offset + 1)].
		 div16 = 7 ifTrue:
			[^client
				sendSpecial: (Smalltalk specialSelectorAt: offset + 17)
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
		^client
			send: (method literalAt: offset + 1)
			super: false
			numArgs: div16 - 8].
	"div16 >= 11; bytecode >= 176"
	div16 < 14 ifTrue:
		[div16 = 11 ifTrue:
			[offset < 8 ifTrue:
				[^client jump: offset + 1].
			 ^client jump: offset - 7 if: true].
		 div16 = 12 ifTrue:
			[offset < 8 ifTrue:
				[^client jump: offset + 1 if: false].
			 ^client popIntoReceiverVariable: offset - 8].
		 "div16 = 13"
		 offset < 8 ifTrue:
		 	[^client popIntoTemporaryVariable: offset].
		 offset = 8 ifTrue:
			[^client doPop].
		 offset = 9 ifTrue:
			[^client trap].
		 ^self unusedBytecode: client at: savedPC].
	"2 byte and 3 byte codes"
	byte < 248 ifTrue:
		[^self interpretNext2ByteSistaV1Instruction: byte for: client extA: extA extB: extB startPC: savedPC].
	^self interpretNext3ByteSistaV1Instruction: byte for: client extA: extA extB: extB startPC: savedPC! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'eem 8/1/2014 07:00'!
interpretSistaV1ExtendedPush: extB for: client
	"Implement the extended push for non-zero extensions."
	"*	82			01010010			Push thisContext, (then Extend B = 1 => push thisProcess)"
	extB = 0 ifTrue:
		[^client pushActiveContext].
	extB = 1 ifTrue:
		[^client pushActiveProcess].
	self error: 'undefined extended push'! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'eem 1/9/2023 08:40'!
interpretSistaV1Jump
	"If the instruction at pc is an unconditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."

	"	176-183	10110 iii				Jump iii + 1 (i.e., 1 through 8)
	 *	225/16rE1	11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 *	237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	| method byte nextpc extB |
	method := self method.
	"consume and compute any extension first."
	extB := 0.
	nextpc := pc. "must not advance pc unless this is a jump."
	[byte := self method at: nextpc.
	 nextpc := nextpc + 1.
	 byte = 16rE1] whileTrue:
		[| extByte |
		 extByte := self method at: nextpc.
		 nextpc := nextpc + 1.
		 extB := (extB = 0 and: [extByte > 127])
					ifTrue: [extByte - 256]
					ifFalse: [(extB bitShift: 8) + extByte]].
	(byte between: 176 and: 183) ifTrue:
		[pc := nextpc.
		 ^byte - 175].
	byte = 237 ifTrue:
		[byte := method at: nextpc.
		 pc := nextpc + 1.
		 ^(extB bitShift: 8) + byte].
	^nil! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'eem 8/1/2014 06:45'!
interpretSistaV1JumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."

	"	184-191	10111 iii				Pop and Jump 0n True iii +1 (i.e., 1 through 8)
		192-199	11000 iii				Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	 *	225/E1		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 *	238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0)
	 *	239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)"
	| method byte nextpc extB |
	method := self method.
	"consume and compute any extension first."
	extB := 0.
	nextpc := pc. "must not advance pc unless this is a jump."
	[byte := self method at: nextpc.
	 nextpc := nextpc + 1.
	 byte = 16rE1] whileTrue:
		[| extByte |
		 extByte := self method at: nextpc.
		 nextpc := nextpc + 1.
		 extB := (extB = 0 and: [extByte > 127])
					ifTrue: [extByte - 256]
					ifFalse: [(extB bitShift: 8) + extByte]].
	(byte between: 184 and: 199) ifTrue:
		[pc := nextpc.
		 ^(byte bitAnd: 7) + 1].
	(byte between: 238 and: 239) ifTrue:
		[byte := method at: nextpc.
		 pc := nextpc + 1.
		 ^(extB bitShift: 8) + byte].
	^nil! !


!InstructionStream class methodsFor: 'class initialization' stamp: 'eem 8/22/2018 15:00'!
initialize
	"Initialize an array of special constants returned by single-bytecode returns
	 in the SqueakV3 bytecode set."

	SpecialConstants := {true. false. nil. -1. 0. 1. 2}
	"InstructionStream initialize."
! !

InstructionStream initialize!

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 8/1/2014 07:17'!
blockReturnConstant: value

	self pushConstant: value; blockReturnTop! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 1/5/2018 10:29'!
directedSuperSend: selector numArgs: numArgs
	stack removeLast. "Discard the pushed class."
	^self send: selector super: true numArgs: numArgs! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'mt 12/20/2023 13:24'!
methodReturnSpecialConstant: value
	^self methodReturnConstant: value! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'CompiledMethodTest 9/12/2021 21:20'!
pushSpecialConstant: value
	^self pushConstant: value! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'CompiledMethodTest 9/12/2021 21:19'!
sendSpecial: selector numArgs: numArgs
	^self send: selector super: false numArgs: numArgs! !


!Decompiler methodsFor: 'private' stamp: 'eem 4/25/2017 18:02'!
convertToDoLoop: blockBodyTempCounts
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	or
		var := startExpr.
		limit := limitExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]
	 and answer true."
	| whileStmt incrStmt initStmt limitStmt toDoStmt |
	whileStmt := statements last.
	incrStmt := whileStmt arguments first statements last.
	incrStmt isAssignmentNode ifFalse:
		[^false].
	(self startAndLimitFor: incrStmt variable from: stack into:
							[:startExpr :limitExpr| initStmt := startExpr. limitStmt := limitExpr])
		ifTrue:
			[| limitInStatements |
			 limitInStatements := limitStmt isNil
								    and: [statements size > 1
								    and: [self startAndLimitFor: incrStmt variable from: { stack last. (statements last: 2) first } into:
												[:startExpr :limitExpr| limitStmt := limitExpr]]].
			(toDoStmt := statements last toDoFromWhileWithCounts: blockBodyTempCounts init: initStmt limit: limitStmt) ifNil:
				[^false].
			 limitInStatements
				ifTrue:
					[stack
						removeLast;
						addLast: toDoStmt.
					 statements removeLast: 2]
				ifFalse:
					[stack
						removeLast: (limitStmt ifNil: [1] ifNotNil: [2]);
						addLast: toDoStmt.
					 statements removeLast]]
		ifFalse:
			[(self startAndLimitFor: incrStmt variable from: statements allButLast into:
							[:startExpr :limitExpr| initStmt := startExpr. limitStmt := limitExpr]) ifFalse:
				[^false].
			 (toDoStmt := statements last toDoFromWhileWithCounts: blockBodyTempCounts init: initStmt limit: limitStmt) ifNil:
				[^false].
			 statements
				removeLast: (limitStmt ifNil: [2] ifNotNil: [3]);
				addLast: toDoStmt].
	self markTemp: initStmt variable asOutOfScope: -1. "Flag arg as out of scope"
	initStmt variable beBlockArg.
	limitStmt ifNotNil:
		[self markTemp: limitStmt variable asOutOfScope: -2.
		 toDoStmt arguments at: 1 put: limitStmt value]. "Flag limit as hidden"
	^true! !


!Decompiler methodsFor: 'private' stamp: 'eem 4/25/2017 16:07'!
markTemp: tempVarNode asOutOfScope: scopeFlag
	tempVarNode scope: scopeFlag.
	tempReadCounts removeKey: tempVarNode ifAbsent: []! !


!Decompiler methodsFor: 'private' stamp: 'eem 1/10/2023 11:36'!
printPCRelative: value on: aStream
	value
		ifNil: [value printOn: aStream]
		ifNotNil:
			[value < method initialPC
				ifTrue: [value printOn: aStream]
				ifFalse: [aStream nextPut: $+; print: value - method initialPC]]! !


!Decompiler methodsFor: 'private' stamp: 'eem 7/12/2024 10:21'!
startAndLimitFor: incrVar from: aStack into: binaryBlock
	"If incrVar matches the increment of a whileLoop at the end of statements
	 evaluate binaryBlock with the init statement for incrVar and the init statement
	 for the block's limit, if any, and answer true.  Otherwise answer false.  Used to
	 help convert whileTrue: loops into to:[by:]do: loops."
	| guard initExpr limitInit size |
	((size := aStack size) >= 1
	 and: [(initExpr := aStack at: size) ~~ CaseFlag]
	 and: [initExpr isAssignmentNode]) ifFalse:
		[^false].
	initExpr variable == incrVar ifTrue:
		[binaryBlock value: initExpr value: nil.
		 ^true].
	limitInit := initExpr.
	(size >= 2
	 and: [(initExpr := aStack at: size - 1) isAssignmentNode
	 and: [initExpr variable == incrVar
	 and: [(guard := statements last receiver) isBlockNode
	 and: [guard statements size = 1
	 and: [(guard := guard statements first) isMessageNode
	 and: [guard receiver == incrVar
	 and: [guard arguments size > 0
	 and: [guard arguments first == limitInit variable]]]]]]]]) ifTrue:
		[binaryBlock value: initExpr value: limitInit.
		 ^true].
	^false! !


!Decompiler class methodsFor: 'class initialization' stamp: 'nice 5/10/2020 11:50'!
initialize

	CascadeFlag := 'cascade'.  "A unique object"
	CaseFlag := 'case'. "Ditto"
	OtherwiseFlag := 'otherwise'. "Ditto"
	ArgumentFlag := 'argument'.  "Ditto"
	IfNilFlag := 'ifNil'.  "Ditto"

	"Decompiler initialize"! !

Decompiler initialize!

!ContextPart methodsFor: 'instruction decoding' stamp: 'tpr 12/25/2022 16:36'!
directedSuperSend: selector numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, selector,
	 starting the message lookup in the superclass of the class on top of stack.
	 The arguments of the message are found in the next numArgs locations on
	 the stack and the receiver just below them."

	| class newReceiver arguments |
	class := self pop.
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	newReceiver := self pop.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: newReceiver with: arguments lookupIn: class superclass].
	^self send: selector to: newReceiver with: arguments lookupIn: class superclass! !


!ContextPart methodsFor: 'controlling' stamp: 'eem 12/27/2024 13:55:04'!
activateMethod: newMethod withArgs: args receiver: rcvr
	"Answer a MethodContext initialized with the arguments."

	^MethodContext 
		sender: self
		receiver: rcvr
		method: newMethod
		arguments: args! !


!ContextPart methodsFor: 'controlling' stamp: 'eem 12/27/2024 14:09:16'!
executeMethod: meth forSelector: selector withArgs: arguments receiver: rcvr

	| primIndex val ctxt |
	(self objectClass: meth) isCompiledMethodClass ifFalse:
		["Object as Methods (OaM) protocol: 'The contract is that, when the VM encounters an ordinary object (rather than a compiled method) in the method dictionary during lookup, it sends it the special selector #run:with:in: providing the original selector, arguments, and receiver.'. DOI: 10.1145/2991041.2991062."
		^self send: #run:with:in:
			to: meth
			with: {selector. arguments. rcvr}].
	
	meth numArgs = arguments size ifFalse:
		[^ self class primitiveFailTokenFor: #'bad number of arguments'].
	(primIndex := meth primitive) > 0 ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		(self isPrimFailToken: val) ifFalse:
			[^val]].
	
	ctxt := self activateMethod: meth withArgs: arguments receiver: rcvr.
	(primIndex isInteger and: [primIndex > 0]) ifTrue:
		[ctxt failPrimitiveWith: val].
	
	^ctxt! !


!ContextPart methodsFor: 'controlling' stamp: 'eem 4/13/2017 13:22'!
send: selector to: rcvr with: arguments
	"Simulate the action of sending a message with selector arguments to rcvr."

	^self send: selector to: rcvr with: arguments lookupIn: (self objectClass: rcvr)! !


!ContextPart methodsFor: 'instruction decoding implicit literals' stamp: 'mt 12/20/2023 13:23'!
methodReturnSpecialConstant: value
	"Simulate the action of a 'return constant' bytecode whose value is the
	 argument, value. This corresponds to a source expression like '^0'."

	^self return: value from: self methodReturnContext! !


!ContextPart methodsFor: 'instruction decoding implicit literals' stamp: 'eem 9/12/2021 21:52'!
pushSpecialConstant: value 
	"Simulate the action of bytecode that pushes the constant, value, on the 
	top of the stack."

	self push: value! !


!ContextPart methodsFor: 'instruction decoding implicit literals' stamp: 'eem 9/12/2021 21:24'!
sendSpecial: selector numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, 
	 selector. The arguments  of the message are found in the top numArgs
	 locations on the stack and the receiver just below them."

	| thisReceiver arguments lookupClass |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	thisReceiver := self pop.
	lookupClass := self objectClass: thisReceiver.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: thisReceiver with: arguments lookupIn: lookupClass].
	^self send: selector to: thisReceiver with: arguments lookupIn: lookupClass! !


!ContextPart methodsFor: 'read-only objects' stamp: 'eem 12/24/2024 19:58:21'!
modificationForbiddenFor: target at: index put: anObject
	"This is a place-holder before proper read-only object support is implemented."
	^self error: 'no modification'! !


!ContextPart methodsFor: 'read-only objects' stamp: 'eem 12/24/2024 19:58:48'!
modificationForbiddenFor: target instVarAt: index put: anObject
	"This is a place-holder before proper read-only object support is implemented."
	^self error: 'no modification'! !


!ContextPart methodsFor: 'read-only objects' stamp: 'eem 3/11/2020 18:02'!
simulatedObject: anObject instVarAt: anIndex put: aValue 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable, or if anObject is read-only. This version correctly simulates
	 assigning to a read-only object. Answer the value stored as the result.
	 Using this message violates the  principle that each object has sovereign
	 control over the storing of values into its instance variables. Essential for
	 the debugger. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	ec == #'no modification' ifTrue:
		[^self send: #attemptToAssign:to:withIndex: to: self with: {aValue. anObject. anIndex} lookupIn: self class].
	self primitiveFailed! !


!ContextPart class methodsFor: 'class initialization' stamp: 'eem 12/26/2024 14:02:33'!
initialize

	PrimitiveFailToken := Object new. "A unique token."
	ValueIndex := Association allInstVarNames indexOf: 'value'.
	QuickStep := nil "The context stepping in the debugger"! !

ContextPart initialize!

!Mutex methodsFor: 'system simulation' stamp: 'eem 3/30/2017 15:44'!
primitiveEnterCriticalSectionOnBehalfOf: effectiveProcess
	"Primitive. Simulate primitiveEnterCriticalSection.  The receiver
	 must be unowned or owned by the effectiveProcess to proceed.
	 Answer if the process is already owned by the current process."
	<primitive: 186>
	^MethodContext primitiveFailTokenFor: nil! !


!Mutex methodsFor: 'system simulation' stamp: 'eem 3/30/2017 15:44'!
primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effectiveProcess
	"Primitive. Simulate primitiveEnterCriticalSection.  Attempt to set the ownership
	 of the receiver.  If the receiver is unowned set its owningProcess to the
	 effectiveProcess and answer false.  If the receiver is owned by the effectiveProcess
	 answer true.  If the receiver is owned by some other process answer nil."
	<primitive: 187>
	^MethodContext primitiveFailTokenFor: nil! !


!CompiledMethod methodsFor: 'testing' stamp: 'eem 1/17/2025 15:16:36'!
isSetterOf: anInstVarName at: anInstVarIndex
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
	^scanner nextInstruction selector == #methodReturnReceiver! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 1/21/2025 12:07'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass ifNil:
		[PrimaryBytecodeSetEncoderClass := EncoderForV3PlusClosures].
	SecondaryBytecodeSetEncoderClass ifNil:
		[SecondaryBytecodeSetEncoderClass := EncoderForSistaV1].
	PreferredBytecodeSetEncoderClass ifNil:
		[PreferredBytecodeSetEncoderClass := PrimaryBytecodeSetEncoderClass]! !

CompiledMethod initialize!

!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 8/13/2014 13:29'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header >= 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 8/13/2014 13:29'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header < 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !


!Parser methodsFor: 'private' stamp: 'eem 4/3/2017 16:21'!
encoder
	^encoder ifNil:
		[encoder := CompiledMethod preferredBytecodeSetEncoderClass new]! !


!ParseNode methodsFor: 'testing' stamp: ''!
isReturningIf

	^false! !


!ParseNode class methodsFor: 'accessing' stamp: 'eem 12/27/2024 15:32:12'!
blockReturnCode

	^ EndRemote! !


!ParseNode class methodsFor: 'accessing' stamp: 'eem 12/27/2024 15:32:16'!
popCode

	^ Pop! !


!ParseNode class methodsFor: 'accessing' stamp: 'eem 12/27/2024 15:30:17'!
pushNilCode

	^LdNil! !


!ParseNode class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:59:33'!
initialize
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

ParseNode initialize!

!Encoder methodsFor: 'initialization' stamp: 'eem 12/27/2024 15:56:37'!
initScopeAndLiteralTables

	StdVariables ifNil: [VariableNode initClassCachedState].
	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new.
	bindingsInBacktickLiterals := Set new.! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 4/5/2017 17:22'!
bindBlockArg: name within: aBlockNode
	| nArgs |
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= 15 ifTrue:
		[^self notify: 'Too many arguments'].
	aBlockNode nArgsSlot: nArgs + 1.
	^(self bindTemp: name)
		beBlockArg;
		nowHasDef;
		nowHasRef;
		yourself! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 4/5/2017 17:20'!
bindBlockTemp: name within: aBlockNode
	| nArgs |
	(nArgs := aBlockNode nArgsSlot) ifNil:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= (CompiledMethod fullFrameSize - 1) ifTrue:
		[^self notify: 'Too many temporaries'].
	aBlockNode nArgsSlot: nArgs + 1.
	^self bindTemp: name! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'eem 4/5/2017 17:23'!
bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp."
	scopeTable at: name ifPresent:
		[:node|
		"When non-interactive raise the error only if it is a duplicate"
		node isTemp
			ifTrue:[node scope >= 0 ifTrue:
						[^self notify: 'Name already used in this method']]
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name! !


!BytecodeEncoder methodsFor: 'special literal encodings' stamp: 'eem 12/26/2024 11:34:56'!
if: code isSpecialLiteralForPush: aBlock
	"If code is that of a special literal for push then evaluate aBlock with the special literal
	 The special literals for push are nil true false -1 0 1 & 2 which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil + 4)
	    and: [aBlock value: (#(true false nil -1 0 1 2) at: code - LdSelf).
			true]! !


!BraceNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:12'!
emitCodeForValue: stack encoder: encoder
	"Hack; when sizing we have no way of knowing how much stack space is available."
	elements size <= self maxElementsForConsArray ifTrue:
		[elements do: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder! !


!BraceNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:16'!
sizeCodeForValue: encoder

	"Hack; we have no way of knowing how much stack space is available."
	elements size <= self maxElementsForConsArray ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	"Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
	emitNode := CascadeNode new
					receiver: (MessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: {encoder encodeLiteral: elements size}
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt |
											MessageNode new
												receiver: nil
												selector: #nextPut:
												arguments: {elt}
												precedence: 3
												from: encoder])
								copyWith: (MessageNode new
												receiver: nil
												selector: #braceArray
												arguments: #()
												precedence: 1
												from: encoder)).
	^emitNode sizeCodeForValue: encoder! !


!BraceNode methodsFor: 'closure analysis' stamp: 'eem 7/20/2009 09:33'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	elements do:
		[:node|
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]! !


!BlockNode methodsFor: 'code generation' stamp: 'eem 01/07/2018 18:41'!
emitCodeForValue: stack encoder: encoder
	encoder supportsFullBlocks ifTrue:
		[^self emitCodeForFullBlockValue: stack encoder: encoder].
	copiedValues do:
		[:copiedValue| copiedValue emitCodeForValue: stack encoder: encoder].
	closureCreationNode pc: encoder nextPC.
	encoder
		genPushClosureCopyNumCopiedValues: copiedValues size
		numArgs: arguments size
		jumpSize: size.
	stack
		pop: copiedValues size;
		push: 1.
	"Emit the body of the block"
	self emitCodeForEvaluatedClosureValue: stack encoder: encoder! !


!BlockNode methodsFor: 'code generation' stamp: 'eem 12/24/2024 16:46:18'!
sizeCodeForValue: encoder
	"Compute the size for the creation of the block and its code."
	copiedValues := self computeCopiedValues: encoder rootNode.
	self closureCreationNode.
	encoder supportsFullBlocks ifTrue:
		[^(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		 + (encoder
				sizePushFullClosure:
					(closureCreationNode
						key: (self createBlockLiteral: encoder);
						reserve: encoder;
						index)
				numCopied: copiedValues size)].
	"Remember size of body for emit time so we know the size of the jump around it."
	size := self sizeCodeForEvaluatedClosureValue: encoder.
	^(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
	  + (encoder sizePushClosureCopyNumCopiedValues: copiedValues size numArgs: arguments size jumpSize: size)
	  + size! !


!BlockNode methodsFor: 'code generation (closures)' stamp: 'eem 12/24/2024 16:30:00'!
constructClosureCreationNode: encoder
	copiedValues := self computeCopiedValues: encoder rootNode.
	^self closureCreationNode! !


!BlockNode class methodsFor: 'instance creation' stamp: 'eem 12/28/2024 13:23'!
withJust: aNode
	^self new statements: { aNode } returns: false! !


!MethodNode methodsFor: 'converting' stamp: 'eem 12/29/2024 17:45:43'!
preen
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.

	 Currently two cases:

		preenLocalIfNotNilArg: blockNode
		hiding the assignment to the arg of an inlined block arg to ifNotNil:,
			(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...].

		preenTempsConflictingWithBlockNode: temps
		hiding the declaration of a temp that is redeclared in some block"

	self preenableNodes ifNotNil:
		[:preenableNodes|
		 preenableNodes keysAndValuesDo:
			[:nodeOrArray :selector |
			 self perform: selector with: nodeOrArray with: preenableNodes]]! !


!LiteralNode methodsFor: 'code generation' stamp: 'eem 8/12/2017 15:26'!
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	(encoder isSpecialLiteralForPush: key)
		ifTrue: [encoder genPushSpecialLiteral: key]
		ifFalse: [encoder genPushLiteral: index]! !


!LiteralNode methodsFor: 'code generation' stamp: 'eem 11/29/2022 12:35'!
sizeCodeForValue: encoder
	^(encoder isSpecialLiteralForPush: key)
		ifTrue: [encoder sizePushSpecialLiteral: key]
		ifFalse:
			[self reserve: encoder.
			encoder sizePushLiteral: index]! !


!RemoteTempVectorNode methodsFor: 'closure analysis' stamp: 'eem 4/5/2017 17:23'!
addRemoteTemp: aTempVariableNode encoder: encoder
	remoteTemps ifNil:
		[remoteTemps := OrderedCollection new].
	remoteTemps addLast: aTempVariableNode.
	aTempVariableNode referenceScopesAndIndicesDo:
		[:scopeBlock "<BlockNode>" :location "<Integer>"|
		 self addReadWithin: scopeBlock at: location]! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:30'!
emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder
	encoder
		genStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:27'!
emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder
	encoder
		genStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index.
	stack pop: 1! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:31'!
emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder
	encoder
		genPushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index.
	stack push: 1! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:27'!
nodeToInitialize: encoder
	^AssignmentNode new
		variable: self
		value: (NewArrayNode new numElements: remoteTemps size)! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:31'!
sizeCodeForStoreInto: aTempVariableNode encoder: encoder
	^encoder
		sizeStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:31'!
sizeCodeForStorePopInto: aTempVariableNode encoder: encoder
	^encoder
		sizeStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index! !


!RemoteTempVectorNode methodsFor: 'code generation' stamp: 'eem 4/5/2017 17:31'!
sizeCodeForValueOf: aTempVariableNode encoder: encoder
	^encoder
		sizePushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index! !


!MessageNode methodsFor: 'testing' stamp: 'eem 9/26/2008 12:39'!
isReturningIf

	^((special between: 3 and: 4) "ifTrue:ifFalse:/ifFalse:ifTrue:"
	    or: [special between: 17 and: 18]) "ifNil:ifNotNil:/ifNotNil:ifNil:"
		and: [arguments first returns and: [arguments last returns]]! !


!MessageNode class methodsFor: 'class initialization' stamp: 'eem 12/27/2024 15:36:08'!
initialize
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
			printRepeatOn:indent:).
	Preferences
		name: #allowNonBooleanReceiversOfBooleanMessages
		description: 'If set, do not optimize boolean selectors, allowing, for example, new Boolean classes.'
		category: #programming type: Boolean value: false! !

MessageNode initialize!

!DecompilerConstructor methodsFor: 'constructor' stamp: 'ct 3/27/2020 20:01'!
codeAnyLitInd: association

	^LiteralVariableNode new
		name: association key
		key: association
		index: nil
		type: LdLitIndType! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'ct 3/28/2020 00:36'!
codeAnySelector: selector

	^SelectorNode new
		key: selector
		index: nil
		type: SendType! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'mt 9/4/2019 16:09'!
codeMessage: receiver selector: selector arguments: arguments
	| symbol |
	symbol := selector key.
	(self
		decodeLiteralVariableValueDereferenceWithReceiver: receiver
		selector: symbol
		arguments: arguments) ifNotNil: [:node| ^node].

	^MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence! !


!Behavior methodsFor: 'system startup' stamp: 'eem 12/26/2024 14:10:16'!
releaseClassCachedState
	"Will be called for each class on shutdown or snapshot. Implemented in relevant subclasses.
	 All class vars or class instVar vars that refer to objects that take up space and can be cheaply
	 recreated lazily on demand, should be nilled. For more expensive stuff to clean and recreate,
	 consider implementing #releaseClassState which is not called on every image save.

	 See implementors for examples"! !


!BlockLocalTempCounter methodsFor: 'initialize-release' stamp: 'eem 4/6/2017 14:16'!
tempCountForBlockAt: pc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.

	 There are short-cuts.  The ones we take here are
		- if there is no sequence of push nils there can be no local temps
		- we follow forward jumps to shorten the amount of scanning"
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: pc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = method encoderClass pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[scanner interpretNextInstructionFor: self]].
	^stackPointer! !


!BlockLocalTempCounter methodsFor: 'initialize-release' stamp: 'eem 4/6/2017 14:56'!
testTempCountForBlockAt: startPc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.There are short-cuts.  The only
	 one we take here is
		- if there is no sequence of push nils there can be no local temps"

	| symbolicLines line prior thePc |
	symbolicLines := Dictionary new.
	method symbolicLinesDo:
		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: startPc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = method encoderClass pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[line := symbolicLines at: scanner pc.
			 prior := stackPointer.
			 thePc := scanner pc.
			 scanner interpretNextInstructionFor: self.
			 Transcript cr; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line; flush]].
	^stackPointer! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 1/1/2023 11:57'!
popIntoReceiverVariable: offset 
	"Print the Remove Top Of Stack And Store Into Instance Variable bytecode."

	self print: 'popIntoRcvr: ' , (self stringForReceiverVariableOffset: offset)! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 12/24/2024 17:12:48'!
pushConstant: obj
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
		^self].! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 1/1/2023 11:57'!
pushReceiverVariable: offset
	"Print the Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

	self print: 'pushRcvr: ' , (self stringForReceiverVariableOffset: offset)! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 3/30/2017 15:54'!
send: selector super: supered numArgs: numArgs
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered
				ifTrue: ['superSend: ']
				ifFalse: ['send: '])
			, (self stringForSelector: selector numArgs: numArgs)! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'eem 1/1/2023 11:57'!
storeIntoReceiverVariable: offset 
	"Print the Store Top Of Stack Into Instance Variable Of Method bytecode."

	self print: 'storeIntoRcvr: ' , (self stringForReceiverVariableOffset: offset)! !


!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'mt 12/20/2023 13:20'!
interpretNextV3ClosuresInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushSpecialConstant: (SpecialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnSpecialConstant: (SpecialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3ClosuresExtension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			sendSpecial: (Smalltalk specialSelectorAt: offset + 1)
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				sendSpecial: (Smalltalk specialSelectorAt: offset + 17)
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"! !


!Decompiler methodsFor: 'control' stamp: 'ct 3/26/2020 17:16'!
blockTo: end
	"Decompile a range of code as in statementsTo:, but return a block node. NB: end is an exclusive index."
	| exprs block oldBase lastStatementOfBlockIsNil |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsTo: end.
	lastStatementOfBlockIsNil := pc < method endPC and: [exprs notEmpty and: [exprs last == (constTable at: 4)]].
	lastStatementOfBlockIsNil ifTrue:
		[exprs := exprs allButLast].
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block! !


!Decompiler methodsFor: 'control' stamp: 'eem 12/29/2024 15:16:40'!
doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block tmpNumberBase |
	tmpNumberBase := statements size.
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := BlockLocalTempCounter tempCountForBlockStartingAt: pc in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 'argm', tmpNumberBase printString, '_', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 'temp', tmpNumberBase printString, '_', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[super interpretNextInstructionFor: self.
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


!Decompiler methodsFor: 'control' stamp: 'nice 5/10/2020 12:23'!
statementsForCaseTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a OtherwiseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for OtherwiseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == OtherwiseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos! !


!Decompiler methodsFor: 'control' stamp: 'eem 12/29/2024 12:19:28'!
statementsTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end."

	| encoderClass blockPos stackPos localLastPC initialPC |
	initialPC := pc.
	encoderClass := method encoderClass.
	blockPos := statements size.
	stackPos := stack size.
	[pc < end]
		whileTrue:
			[lastPc := localLastPC := pc.  limit := end. "for performs"
			 "for logging use the first form"
			 encoderClass interpretNextInstructionFor: self in: self].
	"If there is an additional item on the stack, it will be the value of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[statements addLast: stack removeLast].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	^self popTo: blockPos! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 5/10/2020 12:33'!
case: dist
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CaseFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	"Trick: put a flag on the stack.
	If it is the last case before otherwise: block, then
	- there won't be a dup of caseOf: receiver before sending =
	- there won't be a pop in the case handling block"
	stack addLast: OtherwiseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).
		
	stack last == OtherwiseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns wierdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CaseFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 5/10/2020 12:31'!
doDup
	stack last == CaseFlag
		ifTrue:
			["We are in the process of decompiling a caseOf:"
			stack addLast: CaseFlag.
			^self].
	stack last == CascadeFlag
		ifFalse:
			["Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: CascadeFlag].
	stack addLast: CascadeFlag! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 5/10/2020 12:26'!
doPop

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == OtherwiseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].! !


!Decompiler methodsFor: 'instruction decoding' stamp: ''!
doStore: stackOrBlock
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 12/28/2024 14:42'!
jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack |
	lastJumpIfPcStack addLast: lastPc.
	stack last == CaseFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			[| blockBody blockArgs savedReadCounts blockBodyReadCounts selector |
			 "Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by searching for the node
			  with the relevant pc."
			stack := saveStack.
			savedReadCounts := tempReadCounts copy.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			blockBodyReadCounts := tempReadCounts.
			savedReadCounts keysAndValuesDo:
				[:temp :count|
				 blockBodyReadCounts at: temp put: (blockBodyReadCounts at: temp) - count].
			tempReadCounts := savedReadCounts.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			blockArgs := thenBlock statements = constructor codeEmptyBlock statements
							ifTrue: [#()]
							ifFalse: [{ thenBlock }].
			selector := blockArgs isEmpty
							ifTrue: [sign ifTrue: [#whileFalse] ifFalse: [#whileTrue]]
							ifFalse: [sign ifTrue: [#whileFalse:] ifFalse: [#whileTrue:]].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor codeSelector: selector code: #macro)
					arguments: blockArgs).
			pc := elseStart.
			selector == #whileTrue: ifTrue:
				[self convertToDoLoop: blockBodyReadCounts]]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: (Array with: thenBlock)]
						ifFalse:
							[| args |
							args := sign
										ifTrue: [{elseBlock. thenBlock}]
										ifFalse: [{thenBlock. elseBlock}].
							(constructor
								decodeIfNilWithReceiver: ifExpr
								selector: #ifTrue:ifFalse:
								arguments: args
								tempReadCounts: tempReadCounts) ifNil:
									[constructor
										codeMessage: ifExpr
										selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
										arguments: args]].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 12/29/2024 12:42:50'!
popIntoTemporaryVariable: offset
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars "implies we were intialized with temp names."
			ifNotNil: "Use the provided temps"
				[self assert: ((tempVector := tempVars at: offset + 1 ifAbsent: [ParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]])]
			ifNil: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 'temp', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	stack addLast: (offset >= tempVars size
						ifTrue: "Handle the case of chained LiteralVariableBinding assigments"
							[stack at: (offset + 1 - tempVars size)]
						ifFalse: "A regular argument or temporary"
							[tempVars at: offset + 1]).
	self doStore: statements! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 4/24/2017 10:17'!
pushTemporaryVariable: offset
	| node |
	offset >= tempVars size
				ifTrue: "Handle the case of chained LiteralVariableBinding assigments"
					[self halt.
					 node := stack at: offset + 1 - tempVars size]
				ifFalse: "A regular argument or temporary"
					[node := tempVars at: offset + 1.
					 node isArg ifFalse: "count temp reads for the whileTrue: => to:do: transformation."
						[tempReadCounts at: node put: (tempReadCounts at: node ifAbsent: [0]) + 1]].
	stack addLast: node! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 5/10/2020 12:35'!
send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	selNode := constructor codeAnySelector: selector.
	rcvr == CaseFlag
		ifTrue:
			[| cases stmtStream elements node b |
			selector == #= ifTrue:
					[" = signals a case statement..."
					statements addLast: args first.
					stack addLast: rcvr. "restore CaseFlag"
					^ self].
			selector = #caseError ifFalse: [self error: 'unexpected message send while decompiling a caseOf:'].
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CaseFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf: code: #macro)
					arguments: (Array with: cases)).
			^self].
	rcvr == CascadeFlag
		ifTrue:
			["May actually be a cascade or an ifNil: for value."
			self willJumpIfFalse
				ifTrue: "= generated by a case macro"
					[selector == #= ifTrue:
						[" = signals a case statement..."
						statements addLast: args first.
						stack removeLast; addLast: CaseFlag; addLast: CaseFlag.	"Properly mark the case statement"
						^ self].
					selector == #== ifTrue:
						[" == signals an ifNil: for value..."
						stack removeLast; removeLast.
						rcvr := stack removeLast.
						stack addLast: IfNilFlag;
							addLast: (constructor
								codeMessage: rcvr
								selector: selNode
								arguments: args).
						^ self]]
				ifFalse:
					[(self willJumpIfTrue and: [selector == #==]) ifTrue:
						[" == signals an ifNotNil: for value..."
						stack removeLast; removeLast.
						rcvr := stack removeLast.
						stack addLast: IfNilFlag;
							addLast: (constructor
								codeMessage: rcvr
								selector: selNode
								arguments: args).
						^ self]].
			msgNode := constructor
							codeCascadedMessage: selNode
							arguments: args.
			stack last == CascadeFlag ifFalse:
				["Last message of a cascade"
				statements addLast: msgNode.
				messages := self popTo: stack removeLast.  "Depth saved by first dup"
				msgNode := constructor
								codeCascade: stack removeLast
								messages: messages]]
		ifFalse:
			[msgNode := constructor
						codeMessage: rcvr
						selector: selNode
						arguments: args].
	stack addLast: msgNode! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 4/24/2017 10:18'!
storeIntoTemporaryVariable: offset
	stack addLast: (offset >= tempVars size
						ifTrue: "Handle the case of chained LiteralVariableBinding assigments"
							[stack at: (offset + 1 - tempVars size)]
						ifFalse: "A regular argument or temporary"
							[tempVars at: offset + 1]).
	self doStore: stack! !


!Decompiler methodsFor: 'public access' stamp: 'eem 1/21/2025 15:32'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new.
			numLocalTemps := 0.
			self method: method pc: method initialPC.
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


!Decompiler methodsFor: 'public access' stamp: 'jmv 9/25/2019 17:51:15'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompile)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'! !


!Decompiler methodsFor: 'initialization' stamp: 'eem 12/28/2024 14:29:22'!
initSymbols: aClass
	| argCount |
	constructor method: method class: aClass literals: method literals.
	constTable := constructor codeConstants.
	instVars := Array new: aClass instSize.
	tempVarCount := method numTemps.
	argCount := method numArgs.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars := self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars := blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps := tempVars ifNil: [(1 to: tempVarCount) collect: [ :i | 
					(i <= argCount ifTrue: ['arg'] ifFalse: ['temp']), i printString]].
			tempVars := (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i)
			beMethodArg].
	tempReadCounts := Dictionary new! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'ct 12/30/2023 20:23'!
jump: distance if: condition 
	"Simulate the action of a 'conditional jump' bytecode whose offset is the 
	 argument, distance, and whose condition is the argument, condition."

	| bool |
	bool := self pop.
	condition == bool
		ifTrue: [self jump: distance]
		ifFalse:
			[(true == bool or: [false == bool]) ifFalse:
				[^self send: #mustBeBoolean to: bool with: #()]]! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/11/2020 17:29'!
popIntoLiteralVariable: value 
	"Simulate the action of bytecode that removes the top of the stack and 
	 stores it into a literal variable of my method. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self pop.
	result := self simulatedObject: value instVarAt: ValueIndex put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/11/2020 17:30'!
popIntoReceiverVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	 stores it into an instance variable of my receiver. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self pop.
	result := self simulatedObject: self receiver instVarAt: offset + 1 put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/29/2017 12:48'!
popIntoTemporaryVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into one of my temporary variables."

	self at: offset + 1 put: self pop! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 12/26/2024 14:04:11'!
pushLiteralVariable: value 
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: (self object: value instVarAt: ValueIndex)! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 12/24/2024 19:40:19'!
send: selector super: superFlag numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, 
	 selector. The argument, superFlag, tells whether the receiver of the 
	 message was specified with 'super' in the source method. The arguments 
	 of the message are found in the top numArgs locations on the stack and 
	 the receiver just below them."

	| thisReceiver arguments lookupClass |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	thisReceiver := self pop.
	lookupClass := superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: thisReceiver].
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: thisReceiver with: arguments lookupIn: lookupClass].
	^self send: selector to: thisReceiver with: arguments lookupIn: lookupClass! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/11/2020 17:31'!
storeIntoLiteralVariable: value 
	"Simulate the action of bytecode that stores the top of the stack into a 
	literal variable of my method. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self top.
	result := self simulatedObject: value instVarAt: ValueIndex put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/11/2020 17:32'!
storeIntoReceiverVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into an 
	instance variable of my receiver. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self top.
	result := self simulatedObject: self receiver instVarAt: offset + 1 put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/29/2017 12:48'!
storeIntoTemporaryVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into one 
	of my temporary variables."

	self at: offset + 1 put: self top! !


!ContextPart methodsFor: 'controlling' stamp: 'jar 2/14/2023 22:06'!
restart
	"Unwind thisContext to self and resume from beginning.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	^self resumeEvaluating: [self privRefresh]! !


!ContextPart methodsFor: 'controlling' stamp: 'ct 3/25/2022 18:36'!
send: selector to: rcvr with: arguments lookupIn: lookupClass
	"Simulate the action of sending a message with selector and arguments to rcvr. The argument, lookupClass, is the class in which to lookup the message. This is the receiver's class for normal messages, but for super messages it will be some specific class related to the source method."

	| meth |
	(meth := lookupClass lookupSelector: selector) ifNil:
		[selector == #doesNotUnderstand: ifTrue:
			[self error: 'Recursive message not understood!!' translated].
		^self send: #doesNotUnderstand:
				to: rcvr
				with: {(Message selector: selector arguments: arguments) lookupClass: lookupClass}
				lookupIn: lookupClass].
	
	^ self
		executeMethod: meth
		forSelector: selector
		withArgs: arguments
		receiver: rcvr! !


!ContextPart methodsFor: 'private' stamp: 'ct 3/3/2024 19:22'!
doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Test for unsimulatable side effects (that is, code that will be triggered in the image outside of the simulator range). This includes simulation guards, which are traditionally flagged using primitive 19 (a null primitive that doesn't do anything), as well as certain control primitives that might trigger code on other processes. If a side effect is detected, raise a warning to give the user/client a chance to cancel or virtualize the operation."
	"#(19 87) do: [:primitive | self systemNavigation browseAllSelect: [:m | m primitive = primitive]]"
	(primitiveIndex = 19 "simulationGuard" or: [primitiveIndex = 87 "primitiveResume"]) ifTrue:
		[[(SimulationSideEffectWarning forPrimitive: primitiveIndex)
			context: self method: meth receiver: receiver arguments: arguments;
			signalIfSkipped: [^ self]]
				ifCurtailed:
					[self push: receiver "Cheap fix of the context's internal state. Note that unwinding the receiver -- so that the next step would invoke the primitive again -- would be challenging due to to the variety of senders to this method."]].
	
	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: receiver) includesBehavior: BlockClosure]) ifTrue:
		[(primitiveIndex = 206
		  or: [primitiveIndex = 208]) ifTrue:						"[Full]BlockClosure>>valueWithArguments:"
			[^receiver simulateValueWithArguments: arguments first caller: self].
		 ((primitiveIndex between: 201 and: 209)			 "[Full]BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "[Full]BlockClosure>>valueNoContextSwitch[:]"
			[^receiver simulateValueWithArguments: arguments caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[| selector |
		selector := arguments at: 1 ifAbsent:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: arguments allButFirst].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[| selector args |
		arguments size = 2 ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		selector := arguments first.
		args := arguments second.
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: args].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[| rcvr selector args superclass |
		arguments size
			caseOf: {
				[3] -> [
					rcvr := receiver.
					selector := arguments first.
					args := arguments second.
					superclass := arguments third].
				[4] -> ["mirror primitive"
					rcvr := arguments first.
					selector := arguments second.
					args := arguments third.
					superclass := arguments fourth] }
			otherwise: [^ self class primitiveFailTokenFor: #'bad number of arguments'].
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		((self objectClass: rcvr) includesBehavior: superclass) ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: rcvr with: args lookupIn: superclass].

	"Mutex>>primitiveEnterCriticalSection
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSection"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| effective |
		 effective := Processor activeProcess effectiveProcess.
		 "active == effective"
		 value := primitiveIndex = 186
					ifTrue: [receiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [receiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].
	
	(primitiveIndex = 188 or: [primitiveIndex = 189]) ifTrue:
		[| n args methodArg thisReceiver |
		 primitiveIndex caseOf:
			{[188 "primitiveExecuteMethodArgsArray"] ->
				["Object>>withArgs:executeMethod:
				  CompiledMethod class>>receiver:withArguments:executeMethod:
				  VMMirror>>ifFail:object:with:executeMethod: et al"
				 ((n := arguments size) between: 2 and: 4) ifFalse:
					[^self class primitiveFailTokenFor: #'unsupported operation'].
				 ((self objectClass: (args := arguments at: n - 1)) == Array
				  and: [(self objectClass: (methodArg := arguments at: n)) includesBehavior: CompiledMethod]) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 thisReceiver := arguments at: n - 2 ifAbsent: [receiver]].
			 [189 "primitiveExecuteMethod"] ->
				["Object>>executeMethod:
				  Object>>with:...executeMethod:"
				 (arguments size > 0) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 ((self objectClass: (methodArg := arguments atLast: 1)) includesBehavior: CompiledMethod) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 args := arguments allButLast.
				 thisReceiver := receiver]}.
		 methodArg numArgs = args size ifFalse:
			[^self class primitiveFailTokenFor: #'bad number of arguments'].
		 methodArg primitive > 0 ifTrue:
			[methodArg isQuick ifTrue:
				[^self push: (methodArg valueWithReceiver: thisReceiver arguments: args)].
			 ^self doPrimitive: methodArg primitive method: methodArg receiver: thisReceiver args: args].
		 ^self
			activateMethod: methodArg
			withArgs: args
			receiver: thisReceiver].

	primitiveIndex = 118 ifTrue: "[receiver:]tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 3
		  and: [(self objectClass: arguments second) == SmallInteger
		  and: [(self objectClass: arguments last) == Array]]) ifTrue:
			[^self doPrimitive: arguments second method: meth receiver: arguments first args: arguments last].
		 (arguments size = 2
		 and: [(self objectClass: arguments first) == SmallInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^self class primitiveFailTokenFor: -3].
		 ^self doPrimitive: arguments first method: meth receiver: receiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments]
						ifFalse: "should use self receiver: receiver tryPrimitive: primitiveIndex withArgs: arguments but this is only in later VMs (and appears to be broken)"
							[receiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]! !


!ContextPart methodsFor: 'private' stamp: 'eem 12/26/2024 14:03:04'!
isPrimFailToken: anObject
	^(self objectClass: anObject) == Array
	  and: [anObject size = 2
	  and: [anObject first == PrimitiveFailToken]]! !


!ContextPart methodsFor: 'mirror primitives' stamp: 'eem 2/22/2019 16:26'!
object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	self primitiveFailed! !


!ContextPart methodsFor: 'mirror primitives' stamp: 'eem 3/11/2020 18:06'!
object: anObject instVarAt: anIndex put: aValue 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable, or if anObject is read-only.  Answer the value stored as
	 the result. Using this message violates the  principle that each object
	 has sovereign control over the storing of values into its instance variables.
	 Essential for the debugger. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	ec == #'no modification' ifTrue:
		[^self modificationForbiddenFor: anObject instVarAt: anIndex put: aValue].
	self primitiveFailed! !


!ContextPart class methodsFor: 'simulation' stamp: 'eem 12/26/2024 14:03:14'!
primitiveFailTokenFor: errorCode

	^{PrimitiveFailToken. errorCode}! !


!ContextPart class methodsFor: 'cached class state' stamp: 'eem 12/26/2024 14:02:45'!
releaseClassCachedState

	QuickStep := nil! !


!Debugger class methodsFor: 'instance creation' stamp: 'eem 12/24/2024 19:42:44'!
informExistingDebugger: aContext label: aString
	"Walking the context chain, we try to find out if we're in a debugger stepping situation.
	If we find the relevant contexts, we must rearrange them so they look just like they would
	if the methods were excuted outside of the debugger."
	| ctx quickStepMethod oldSender baseContext |
	ctx := thisContext.
	quickStepMethod := ContextPart compiledMethodAt: #quickSend:to:with:lookupIn:.
	[ctx sender == nil or: [ctx sender method == quickStepMethod]] whileFalse: [ctx := ctx sender].
	ctx sender ifNil: [^self].
	baseContext := ctx.
	"baseContext is now the context created by the #quickSend... method."
	oldSender := ctx := ctx sender home sender.
	"oldSender is the context which originally sent the #quickSend... method"
	[ctx == nil or: [ctx receiver isKindOf: self]] whileFalse: [ctx := ctx sender].
	ctx == nil ifTrue: [^self].
	"ctx is the context of the Debugger method #doStep"
	ctx receiver labelString: aString.
	baseContext swapSender: baseContext sender sender sender.	"remove intervening contexts"
	thisContext swapSender: oldSender.	"make myself return to debugger"
	^aContext! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'eem 12/27/2024 15:53:47'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| selectedClass methodNode tempNode |
	self codeProvider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: self codeProvider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !

!methodRemoval: UndefinedObject #literalIndexOfBytecodeAt:in: stamp: 'eem 1/4/2025 17:31:05'!
UndefinedObject removeSelector: #literalIndexOfBytecodeAt:in:!
!methodRemoval: ContextPart class #primitiveFailToken stamp: 'eem 12/26/2024 14:03:57'!
ContextPart class removeSelector: #primitiveFailToken!
!methodRemoval: ContextPart class #valueIndex stamp: 'eem 12/26/2024 14:04:18'!
ContextPart class removeSelector: #valueIndex!
!methodRemoval: ContextPart #quickSend:to:with:super: stamp: 'eem 12/24/2024 19:43:06'!
ContextPart removeSelector: #quickSend:to:with:super:!
!methodRemoval: Decompiler #printInstVarNamed:at:on:asPc: stamp: 'eem 12/27/2024 15:45:55'!
Decompiler removeSelector: #printInstVarNamed:at:on:asPc:!
!methodRemoval: Decompiler #convertToDoLoop stamp: 'eem 12/29/2024 15:07:15'!
Decompiler removeSelector: #convertToDoLoop!
!methodRemoval: Decompiler #withTempNames: stamp: 'eem 12/27/2024 15:46:21'!
Decompiler removeSelector: #withTempNames:!
!methodRemoval: Decompiler #methodRefersOnlyOnceToTemp: stamp: 'eem 12/27/2024 15:45:59'!
Decompiler removeSelector: #methodRefersOnlyOnceToTemp:!
!methodRemoval: Decompiler #doClosureCopy:copiedValues: stamp: 'eem 12/27/2024 15:45:45'!
Decompiler removeSelector: #doClosureCopy:copiedValues:!
!methodRemoval: InstructionStream class #releaseClassCachedState stamp: 'eem 12/26/2024 12:46:14'!
InstructionStream class removeSelector: #releaseClassCachedState!
!methodRemoval: InstructionStream class #specialConstants stamp: 'eem 12/24/2024 20:32:16'!
InstructionStream class removeSelector: #specialConstants!
!methodRemoval: InstructionStream class #initSpecialConstants stamp: 'eem 12/24/2024 20:32:55'!
InstructionStream class removeSelector: #initSpecialConstants!
!methodRemoval: InstructionStream #interpretExtension:in:for: stamp: 'eem 12/24/2024 17:14:14'!
InstructionStream removeSelector: #interpretExtension:in:for:!
!methodRemoval: InstructionStream #scanForInstructionSequence: stamp: 'eem 12/28/2024 13:20:31'!
InstructionStream removeSelector: #scanForInstructionSequence:!
!methodRemoval: InstructionStream #blockMethodOrNil stamp: 'eem 12/24/2024 20:21:33'!
InstructionStream removeSelector: #blockMethodOrNil!
!methodRemoval: InstructionPrinter #pushFullClosure:numCopied:receiverOnStack:ignoreOuterContext: stamp: 'eem 12/27/2024 15:44:35'!
InstructionPrinter removeSelector: #pushFullClosure:numCopied:receiverOnStack:ignoreOuterContext:!
!methodRemoval: InstructionPrinter #pushFullClosure:numCopied: stamp: 'eem 12/27/2024 15:44:31'!
InstructionPrinter removeSelector: #pushFullClosure:numCopied:!
!methodRemoval: NarrowerVariableScopeFinder #visitVariableUsageNode: stamp: 'eem 12/29/2024 18:34:31'!
NarrowerVariableScopeFinder removeSelector: #visitVariableUsageNode:!
!methodRemoval: MessageNode class #releaseClassCachedState stamp: 'eem 12/27/2024 15:35:50'!
MessageNode class removeSelector: #releaseClassCachedState!
!methodRemoval: MessageNode class #new stamp: 'eem 12/27/2024 15:36:01'!
MessageNode class removeSelector: #new!
!methodRemoval: MessageNode class #initClassCachedState stamp: 'eem 12/27/2024 15:36:36'!
MessageNode class removeSelector: #initClassCachedState!
!methodRemoval: RemoteTempVectorNode #emitCodeForIndexOf:stack:encoder: stamp: 'eem 12/24/2024 16:08:54'!
RemoteTempVectorNode removeSelector: #emitCodeForIndexOf:stack:encoder:!
!methodRemoval: RemoteTempVectorNode #sizeCodeForIndexOf:encoder: stamp: 'eem 12/24/2024 16:10:56'!
RemoteTempVectorNode removeSelector: #sizeCodeForIndexOf:encoder:!
!methodRemoval: RemoteTempVectorNode #sizeCodeForLoadFor:encoder: stamp: 'eem 12/24/2024 16:10:44'!
RemoteTempVectorNode removeSelector: #sizeCodeForLoadFor:encoder:!
!methodRemoval: RemoteTempVectorNode #emitCodeForLoadFor:stack:encoder: stamp: 'eem 12/24/2024 16:08:31'!
RemoteTempVectorNode removeSelector: #emitCodeForLoadFor:stack:encoder:!
!methodRemoval: TempVariableNode #emitCodeForLoad:forValue:encoder: stamp: 'eem 12/24/2024 16:08:25'!
TempVariableNode removeSelector: #emitCodeForLoad:forValue:encoder:!
!methodRemoval: TempVariableNode #sizeCodeForLoad:forValue: stamp: 'eem 12/24/2024 16:10:36'!
TempVariableNode removeSelector: #sizeCodeForLoad:forValue:!
!methodRemoval: VariableNode #emitCodeForLoad:forValue:encoder: stamp: 'eem 12/24/2024 16:08:16'!
VariableNode removeSelector: #emitCodeForLoad:forValue:encoder:!
!methodRemoval: MethodNode #preenIfNotNilNode: stamp: 'eem 12/29/2024 18:38:44'!
MethodNode removeSelector: #preenIfNotNilNode:!
!methodRemoval: MethodNode #preenNilNodeFollowingNilIfNode: stamp: 'eem 12/29/2024 18:38:41'!
MethodNode removeSelector: #preenNilNodeFollowingNilIfNode:!
!methodRemoval: MethodNode #preenTempsConflictingWithBlockNode: stamp: 'eem 12/29/2024 18:38:37'!
MethodNode removeSelector: #preenTempsConflictingWithBlockNode:!
!methodRemoval: MethodNode #preenLocalIfNotNilArg stamp: 'eem 12/29/2024 18:38:03'!
MethodNode removeSelector: #preenLocalIfNotNilArg!
!methodRemoval: BlockNode #sizeCodeForClosureValue: stamp: 'eem 12/24/2024 16:43:12'!
BlockNode removeSelector: #sizeCodeForClosureValue:!
!methodRemoval: BlockNode #emitCodeForClosureValue:encoder: stamp: 'eem 12/24/2024 16:43:06'!
BlockNode removeSelector: #emitCodeForClosureValue:encoder:!
!methodRemoval: BlockNode #ensureClosureCreationNode stamp: 'eem 12/24/2024 16:42:12'!
BlockNode removeSelector: #ensureClosureCreationNode!
!methodMoveToSomePackage: EncoderForV3PlusClosures class #literalIndexOfBytecodeAt:in: stamp: 'eem 1/12/2025 13:32:08'!
EncoderForV3PlusClosures class removeSelectorIfInBaseSystem: #literalIndexOfBytecodeAt:in:!
!methodRemoval: EncoderForV3PlusClosures #if:isSpecialLiteralForPush: stamp: 'eem 12/26/2024 11:35:00'!
EncoderForV3PlusClosures removeSelector: #if:isSpecialLiteralForPush:!
!methodRemoval: EncoderForV3PlusClosures #if:isSpecialLiteralForReturn: stamp: 'eem 12/26/2024 11:23:13'!
EncoderForV3PlusClosures removeSelector: #if:isSpecialLiteralForReturn:!
!methodRemoval: EncoderForV3PlusClosures #isSpecialLiteralForReturn: stamp: 'eem 12/26/2024 11:33:04'!
EncoderForV3PlusClosures removeSelector: #isSpecialLiteralForReturn:!
!methodRemoval: EncoderForV3PlusClosures #genPushNClosureTemps: stamp: 'eem 12/26/2024 16:35:20'!
EncoderForV3PlusClosures removeSelector: #genPushNClosureTemps:!
!methodRemoval: EncoderForV3PlusClosures #supportsClosureOpcodes stamp: 'eem 12/24/2024 16:43:33'!
EncoderForV3PlusClosures removeSelector: #supportsClosureOpcodes!
!methodRemoval: EncoderForSistaV1 class #supportsClosures stamp: 'eem 12/24/2024 20:37:42'!
EncoderForSistaV1 class removeSelector: #supportsClosures!
!methodRemoval: EncoderForSistaV1 class #method:refersInBytecodeToLiteral:specialSelectorIndex: stamp: 'eem 12/24/2024 20:30:54'!
EncoderForSistaV1 class removeSelector: #method:refersInBytecodeToLiteral:specialSelectorIndex:!
!methodRemoval: EncoderForSistaV1 class #isBranchAt:in: stamp: 'eem 12/24/2024 20:27:06'!
EncoderForSistaV1 class removeSelector: #isBranchAt:in:!
!methodRemoval: EncoderForSistaV1 class #nopCode stamp: 'eem 12/24/2024 20:31:17'!
EncoderForSistaV1 class removeSelector: #nopCode!
!methodRemoval: EncoderForSistaV1 class #blockMethodOrNilFor:in:at: stamp: 'eem 12/24/2024 20:21:38'!
EncoderForSistaV1 class removeSelector: #blockMethodOrNilFor:in:at:!
!methodRemoval: EncoderForSistaV1 class #isRealSendAt:in: stamp: 'eem 12/24/2024 20:27:31'!
EncoderForSistaV1 class removeSelector: #isRealSendAt:in:!
!methodMoveToSomePackage: EncoderForSistaV1 class #literalIndexOfBytecodeAt:in: stamp: 'eem 1/12/2025 13:32:08'!
EncoderForSistaV1 class removeSelectorIfInBaseSystem: #literalIndexOfBytecodeAt:in:!
!methodRemoval: EncoderForSistaV1 #isSpecialLiteralForReturn: stamp: 'eem 12/26/2024 11:33:02'!
EncoderForSistaV1 removeSelector: #isSpecialLiteralForReturn:!
!methodRemoval: BytecodeEncoder #supportsClosureOpcodes stamp: 'eem 12/24/2024 16:43:32'!
BytecodeEncoder removeSelector: #supportsClosureOpcodes!
!methodMoveToSomePackage: BytecodeEncoder #litIndexOrNilFor: stamp: 'eem 1/12/2025 13:32:08'!
BytecodeEncoder removeSelectorIfInBaseSystem: #litIndexOrNilFor:!
!methodRemoval: Encoder #initForCuisWithClass:literalStream: stamp: 'eem 1/11/2025 22:07:28'!
Encoder removeSelector: #initForCuisWithClass:literalStream:!
!methodMoveToSomePackage: Encoder #initPreFullBlocksClass:literalStream: stamp: 'eem 1/12/2025 13:32:08'!
Encoder removeSelectorIfInBaseSystem: #initPreFullBlocksClass:literalStream:!
!methodMoveToSomePackage: Encoder #initClass:literalStream: stamp: 'eem 1/12/2025 13:32:08'!
Encoder removeSelectorIfInBaseSystem: #initClass:literalStream:!
!methodRemoval: ParseNode class #releaseClassCachedState stamp: 'eem 12/26/2024 14:06:36'!
ParseNode class removeSelector: #releaseClassCachedState!
!methodRemoval: ParseNode class #new stamp: 'eem 12/27/2024 15:33:36'!
ParseNode class removeSelector: #new!
!methodRemoval: ParseNode class #initClassCachedState stamp: 'eem 12/27/2024 15:34:36'!
ParseNode class removeSelector: #initClassCachedState!
!methodRemoval: ParseNode #isOnlySubnodeOf:in: stamp: 'eem 12/29/2024 18:38:09'!
ParseNode removeSelector: #isOnlySubnodeOf:in:!
!methodMoveToSomePackage: WriteStream #crtab stamp: 'eem 1/12/2025 13:32:08'!
WriteStream removeSelectorIfInBaseSystem: #crtab!
!classDefinition: #CompiledMethod category: #'Kernel-Methods'!
ByteArray variableByteSubclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'LargeFrame PreferredBytecodeSetEncoderClass PrimaryBytecodeSetEncoderClass SecondaryBytecodeSetEncoderClass SmallFrame'
	poolDictionaries: ''
	category: 'Kernel-Methods'!
!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass endPC flag frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: setSignFlag signFlag trailer)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('cleaning' flushCache voidCogVMState)
('closures' containsBlockClosures embeddedBlockClosures)
('code analysis' scanForEqSmallConstant)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesFieldCode:with:using: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved dateSortingValue timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' penultimateLiteral penultimateLiteral:)
!
!Parser reorganize!
('private' addComment encoder init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken matchReturn startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression isLiteralExpression isLiteralNumberExpression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:context: newMethodNode parenthesisExpression pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: performInteractiveChecks: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset:)
('public access' encoderClass: optimizeSpecialSends: parse:class: parse:class:category:noPattern:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('backtick' addToSentInLiterals: backtickExpression)
('comparing' match: matchToken:)
('temporaries' bindTemp:)
!
!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: newTemp: newUndeclaredTemp:)
('encoding' asciiStringLiteral: cantStoreInto: encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteBactickLiteralBinding: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: completeSourceRangesBasedOn: criteriaToSortSourceRangeBetween:and: globalSourceRanges initializeCompleteSourceRangesBasedOn: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: parseNodesPathAt:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables temps:literals:class:)
('initialize-release' noteSuper release)
('temporaries' maxTemp)
!
!BraceNode reorganize!
('code generation' emitCodeForValue:encoder: selectorForShortForm: sizeCodeForValue:)
('code generation (closures)' elements maxElementsForConsArray)
('code generation (new scheme)')
('enumerating' casesForwardDo: casesReverseDo: elementsDo:)
('initialization' elements: elements:sourceLocations: matchBraceStreamReceiver:messages: matchBraceWithReceiver:selector:arguments:)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' blockAssociationCheck: equivalentTo: isBraceNode isComplex numElements)
('visiting' accept:)
('private' hasEquivalentElementsTo:)
('closure analysis' analyseTempsWithin:rootNode:assignmentPools: deoptimize)
!
!BlockNode reorganize!
('accessing' addArgument: arguments arguments: block closureCreationNode ensureClosureCreationNode: firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('code generation' code emitCodeForValue:encoder: sizeCodeForValue:)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForEvaluatedClosureValue:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue:)
('enumerating' statementsDo:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: removeImplicitSelfReturnWhenIn: statements:returns:)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('source ranges' expandRanges:basedOn:using:)
('testing' declaresAnyVariable equivalentTo: hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine referencesSelf referencesSuper returns sendsMessageToSelf:)
('visiting' accept:)
('private' hasEquivalentStatementsWith:)
!
!LiteralNode reorganize!
('visiting' accept:)
('code generation (new scheme)' sizeCodeForEffect:)
('evaluation' eval)
('testing' equivalentTo: isConstantCharacter isConstantNumber isLiteralNode isSpecialConstant literalValue)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' name:key:index:type:)
('code generation' emitCodeForValue:encoder: reserve: sizeCodeForValue:)
!
!RemoteTempVectorNode reorganize!
('visiting' accept:)
('code generation (closures)' isIndirectTempVector referenceScopesAndIndicesDo: scope)
('printing' printDefinitionForClosureAnalysisOn:)
('accessing' remoteTemps)
('decompiler' remoteTemps:)
('closure analysis' addRemoteTemp:encoder:)
('code generation' emitCodeForStoreInto:stack:encoder: emitCodeForStorePopInto:stack:encoder: emitCodeForValueOf:stack:encoder: nodeToInitialize: sizeCodeForStoreInto:encoder: sizeCodeForStorePopInto:encoder: sizeCodeForValueOf:encoder:)
!
!Behavior reorganize!
('accessing' classDepth decompilerClass format name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('testing' canZapMethodDictionary hasChangedComparedTo: instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!
!BlockLocalTempCounter reorganize!
('instruction decoding' blockReturnConstant: blockReturnTop doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConsArrayWithElements: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: send:super:numArgs:)
('private' doJoin)
('initialization')
('initialize-release' tempCountForBlockAt:in: testTempCountForBlockAt:in:)
!
!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack tempReadCounts'
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag OtherwiseFlag'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: stackPtr stackp: stepToCalleeOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndStop: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext runUntilReturnFrom: unwindTo: unwindTo:safely:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
!
!ContextPart class reorganize!
('examples' runSimulated: tallyInstructions: tallyMethods: trace: trace:on: trace:onFileNamed:)
('simulation' primitiveFailTokenFor:)
('instance creation' basicNew: initializedInstance new new: newForMethod:)
('private' carefullyPrint:on: isContextClass)
('special context creation' contextEnsure: contextOn:do: theReturnMethod)
('cached class state' releaseClassCachedState)
('class initialization' initialize)
!
!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasNonLocalReturn home isExecutingBlock method methodReturnContext outerContext outerContextsDo: receiver removeSelf tempAt: tempAt:put:)
('closure support' asContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop callPrimitive: pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd:)
('private' aboutToReturn:through: cannotReturn: endpc instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
('instruction decoding implicit literals')
('controlling')
!
!UndefinedObject reorganize!
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('copying' shallowCopy)
('printing' printOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral isNil notNil)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
!
