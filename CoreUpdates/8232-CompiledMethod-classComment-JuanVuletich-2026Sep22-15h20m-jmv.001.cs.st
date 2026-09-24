'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 3:21:16 pm'!

!CompiledMethod commentStamp: 'jmv 22/Sep/2026, 5:09:18 pm (UTC)' prior: 0!
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
	trailer (4 bytes)

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

The trailer holds #historyPointer.

While there are disadvantages to this "flat" representation (it is impossible to add named instance variables to CompiledMethodor its subclasses, but it is possible indirectly; see AdditionalMethodState) it is effective for interpreters.  It means that both bytecodes and literals can be fetched directly from a single method object, and that only one object, the method, must be saved and restored on activation and return.  A more natural representation, in which there are searate instance variables for the bytecode, and (conveniently) the literals, requires either much more work on activation and return setting up references to the literals and bytecodes, or slower access to bytecodes and literals, indirecting on each access.

The last literal of a CompiledMethod is reserved for special use by the kernel and/or the virtual machine.  It must either be the methodClassAssociation, used to implement super sends, or nil, if the method is anonymous.

By convention, the penultimate literal is reserved for special use by the kernel. In CompiledMethod instances it must either be the method selector, or an instance of AdditionalMethodState which holds the selector and any pragmas or properties in the method.
!


!CuisSourceFileArray commentStamp: '<historical>' prior: 0!
Cuis source code access mechanism.
Uses the range for sourcePointer in CompiledMethods (16r1000000 to 16r4FFFFFF) quite differently than StandardSourceFileArray (the older way, inherited from Squeak). First half is for Sources, second half is for Changes. The actual offset in the file is the sourcePointer minus 16r1000000 (or minus 16r3000000) multiplied by a scaling factor. This scaling factor is (right now) 32, raising the limit to 1Gb.!

