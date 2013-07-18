'From Cuis 4.1 of 12 December 2012 [latest update: #1656] on 2 April 2013 at 2:36:38 pm'!

!Float commentStamp: '<historical>' prior: 0!
A note About Floating Point numbers and Floating Point Arithmetic.

The following is not specific to Cuis or Smalltalk at all. This is about the properties of Float numbers in any computer implementation.

Floating Point numbers are a bit special. If you haven't done so already, read 
	"Why don't my numbers add up?":
		http://floating-point-gui.de/
and
	"What Every Computer Scientist Should Know About Floating-Point Arithmetic":
		http://docs.sun.com/source/806-3568/ncg_goldberg.html
and
	"Comparing floating point numbers"
		http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
Now that you read them, and we are on the same boat, some further comments (from jmv):

Floats are (conceptually) approximate real numbers. That's why trig and other trascendental functions answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality.

(If you want exact arithmetic, can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead.)

There are some things that can be done to increase the confidence you can have on Float results. One is to do an error propagation analysis on the code you are running. This is not easy, but it is done for any widely used numerical method. Then, you can know real bounds and/or estimates of the errors made.

Perhaps you have heard about "interval arithmetic". It is a bit better than simple Float, but doesn't really fix the problems.

The ultimate solution is to do Monte Carlo analysis, with random contamination of inputs. After the Monte Carlo run, it is needed to do statistical analysis of possible correlations between the distributions of the random noise added to imputs and the result of the algorithm.

A possibility that could be a practical solution, being much cheaper than Monte Carlo, but better than interval arithmetic, is to represent each value by 2 Floats: an estimation of the real value (i.e. an estimation of the mean value of the distribution of the corresponding Monte Carlo result), and an estimation of the error (i.e. an estimation of the standard deviation of the corresponding Monte Carlo result). Or perhaps even 3 of them. In addition to the estimation of the real value and an estimation of the error, we could add a hard bound on the error. In many cases it will be useless, because the error can not really be bound. But in those cases where it is possible to bound it, applications could really know about the quality of computed values.

=======================================================================

My instances represent IEEE-754 floating-point double-precision numbers. They have about 16 digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:
	
	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12

Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point.  It is actually possible to specify a radix for Squeak Float constants.  This is great for teaching about numbers, but may be confusing to the average reader:

	3r20.2 --> 6.66666666666667
	8r20.2 --> 16.25

If you don't have access to the definition of IEEE-754, you can figure out what is going on by printing various simple values in Float hex.  It may help you to know that the basic format is...
	sign		1 bit
	exponent	11 bits with bias of 1023 (16r3FF) to produce an exponent
						in the range -1023 .. +1024
				- 16r000:
					significand = 0: Float zero
					significand ~= 0: Denormalized number (exp = -1024, no hidden '1' bit)
				- 16r7FF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	53 bits, but only 52 are stored (20 in the first word, 32 in the second).  This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE-754 omits this redundant bit to gain an extra bit of precision instead.  People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND.

The single-precision format is...
	sign		1 bit
	exponent	8 bits, with bias of 127, to represent -126 to +127
                    - 0x0 and 0xFF reserved for Float zero (mantissa is ignored)
                    - 16r7F reserved for Float underflow/overflow (mantissa is ignored)
	mantissa	24 bits, but only 23 are stored
This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:.

Thanks to Rich Harmon for asking many questions and to Tim Olson, Bruce Cohen, Rick Zaccone and others for the answers that I have collected here.!


!MatrixTransform2x3 commentStamp: 'jmv 4/2/2013 14:35' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying a vector by the matrix. The inverse transformation is multiplying a vector by the inverse of the matrix. By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol. In addition, the #doAutoHinting: flag is available for some applications, like text rendering.!


!MatrixTransform2x3 commentStamp: 'jmv 4/2/2013 14:35' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying a vector by the matrix. The inverse transformation is multiplying a vector by the inverse of the matrix. By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol. In addition, the #doAutoHinting: flag is available for some applications, like text rendering.!


!ShortIntegerArray commentStamp: 'jmv 4/2/2013 14:36' prior: 0!
ShortIntegerArray is an array for efficiently representing signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.

Additionaly, using #at:putUnsigned: and #at:putSigned:, it supports unsigned 16 bits integers, between 0 and 16rFFFF. You can also use ShortWordArray to store unsigned 16 bit integers with the usual #at: and #at:put: protocol.

Array size must be even, as two vales are stored in each word.!


!SoundBuffer commentStamp: '<historical>' prior: 0!
SoundBuffers store 16 bit unsigned quantities.

Array size must be even, as two vales are stored in each word.!


!Float class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:00'!
new
	"Floats are 2 words of size"
	^self basicNew: 2! !

!Float class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:04'!
new: s
	self error: 'Please call Float>>#new (without arguments).'.
	^self new! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:08'!
new: s
	self error: 'Please call MatrixTransform2x3>>#new (without arguments).'.
	^self new! !


!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 4/2/2013 14:02'!
doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 4/2/2013 14:01'!
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	 We support 8-byte Floats here.  Non-IEEE"

	| new |
	new _ Float new.		"To get an instance"
	new at: 1 put: (byteStream nextNumber: 4).
	new at: 2 put: (byteStream nextNumber: 4).
	^ new! !


!Float class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:01'!
fromIEEE32Bit: word
	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from a 32bit IEEE floating point representation into an actual Squeak float object (being 64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
	
	| sign mantissa exponent newFloat delta |
	word negative ifTrue: [^ self error:'Cannot deal with negative numbers'].
	word = 0 ifTrue: [^ 0.0].
	sign := word bitAnd: 16r80000000.
	word = sign ifTrue: [^self negativeZero].
	
	exponent := ((word bitShift: -23) bitAnd: 16rFF) - 127.
	mantissa := word bitAnd:  16r7FFFFF.

	exponent = 128 ifTrue:["Either NAN or INF"
		mantissa = 0 ifFalse:[^ Float nan].
		sign = 0 
			ifTrue:[^ Float infinity]
			ifFalse:[^ Float infinity negated]].

	exponent = -127 ifTrue: [
		"gradual underflow (denormalized number)
		Remove first bit of mantissa and adjust exponent"
		delta := mantissa highBit.
		mantissa := (mantissa bitShift: 1) bitAnd: (1 bitShift: delta) - 1.
		exponent := exponent + delta - 23].
	
	"Create new float"
	newFloat := self new.
	newFloat basicAt: 1 put: ((sign bitOr: (1023 + exponent bitShift: 20)) bitOr: (mantissa bitShift: -3)).
	newFloat basicAt: 2 put: ((mantissa bitAnd: 7) bitShift: 29).
	^newFloat! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:07'!
new
	^self basicNew: 6! !


!ShortIntegerArray class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:18'!
new: n
	"Instances are always of even size. Only accept those."
	n \\ 2 = 0 ifFalse: [
		self error: 'ShortIntegerArray>>#new: must be called with an even argument' ].
	^ super new: n // 2! !


!SoundBuffer class methodsFor: 'instance creation' stamp: 'jmv 4/2/2013 14:21'!
newMonoSampleCount: anInteger
	"Return a SoundBuffer large enough to hold the given number of monaural samples (i.e., 16-bit words)."
	"Instances are always of even size, since the underlying representation is in terms of 32-bit words.
	Only accept those."
	anInteger \\ 2 = 0 ifFalse: [
		self error: '#newMonoSampleCount: must be called with an even argument' ].
	^ self basicNew: anInteger // 2
! !


!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 4/2/2013 14:01'!
recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 56.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: Float.
	newArray at: 11 put: MethodContext.
	newArray at: 12 put: BlockContext.
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: (self specialObjectsArray at: 18).
	"(low space Semaphore)"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character value: ascii])."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: (self specialObjectsArray at: 30).
	"(delay Semaphore)"
	newArray at: 31 put: (self specialObjectsArray at: 31).
	"(user interrupt Semaphore)"
	"Prototype instances that can be copied for fast initialization"
	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new.
	newArray at: 35 put: #cannotInterpret:.
	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 37 put: BlockClosure.
	"(BlockContext new: CompiledMethod fullFrameSize)."
	newArray at: 38 put: (self specialObjectsArray at: 38). "Is the prototype BlockContext (unused by the VM)"
	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: nil. "Reserved for a LinkedList instance for overlapped calls in CogMT"
	"finalization Semaphore"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]).
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	"newArray at: 51 put: #attemptToAssign:withIndex:."
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallback:stack:registers:jmpbuf:.
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 4/2/2013 12:37'!
copyright
	"The Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2013.'! !


!FloatArray class reorganize!
('as yet unclassified')
!

!methodRemoval: Float #initialize!
Float removeSelector: #initialize!

!Float reorganize!
('arithmetic' * + - / abs negated)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh degreeCos degreeSin exp floorLog: ln log nthRoot: reciprocalFloorLog: reciprocalLogBase2 safeArcCos sign: sin sinh sqrt tan tanh timesTwoPower:)
('comparing' < <= = > >= closeTo: hash ~=)
('testing' hasContentsInExplorer isFinite isFloat isInfinite isLiteral isNaN isPowerOfTwo isZero sign)
('truncation and round off' exponent exponentPart fractionPart integerPart mantissaPart predecessor reduce rounded signPart significand significandAsInteger successor truncated ulp)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitWord asTrueFraction degreesToRadians partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('printing' absByteEncode:base: absPrintExactlyOn:base: absPrintOn:base: hex printOn:base: printPaddedLeft:decimalPlaces: printPaddedWith:to: printShowingDecimalPlaces: storeOn: storeOn:base:)
('private' absPrintOn:base:digitCount:)
('mathematical' reciprocal)
('intervals' to:count:do: to:hops:do:)
('accessing' at: at:put: basicAt: basicAt:put:)
!

