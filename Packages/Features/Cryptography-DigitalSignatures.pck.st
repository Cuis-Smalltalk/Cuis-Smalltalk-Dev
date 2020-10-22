'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:35:12 pm'!
'Description This package contains implementations of common digital signature algorithms.
For example SHA1, DSA.

It was originally written for Squeak 4.5 and ported to Cuis with few changes by David Graham.

License: MIT'!
!provides: 'Cryptography-DigitalSignatures' 1 14!
!requires: 'Sound' 1 3 nil!
SystemOrganization addCategory: 'Cryptography-DigitalSignatures'!


!classDefinition: #DigitalSignatureAlgorithm category: 'Cryptography-DigitalSignatures'!
Object subclass: #DigitalSignatureAlgorithm
	instanceVariableNames: 'randKey randSeed'
	classVariableNames: 'HighBitOfByte SmallPrimes'
	poolDictionaries: ''
	category: 'Cryptography-DigitalSignatures'!
!classDefinition: 'DigitalSignatureAlgorithm class' category: 'Cryptography-DigitalSignatures'!
DigitalSignatureAlgorithm class
	instanceVariableNames: ''!

!classDefinition: #SecureHashAlgorithm category: 'Cryptography-DigitalSignatures'!
Object subclass: #SecureHashAlgorithm
	instanceVariableNames: 'totalA totalB totalC totalD totalE totals'
	classVariableNames: 'K1 K2 K3 K4'
	poolDictionaries: ''
	category: 'Cryptography-DigitalSignatures'!
!classDefinition: 'SecureHashAlgorithm class' category: 'Cryptography-DigitalSignatures'!
SecureHashAlgorithm class
	instanceVariableNames: ''!

!classDefinition: #SecureHashAlgorithm256 category: 'Cryptography-DigitalSignatures'!
Object subclass: #SecureHashAlgorithm256
	instanceVariableNames: 'totals totalA totalB totalC totalD totalE totalF totalG totalH'
	classVariableNames: 'HashValues RoundConstants'
	poolDictionaries: ''
	category: 'Cryptography-DigitalSignatures'!
!classDefinition: 'SecureHashAlgorithm256 class' category: 'Cryptography-DigitalSignatures'!
SecureHashAlgorithm256 class
	instanceVariableNames: ''!

!classDefinition: #ThirtyTwoBitRegister category: 'Cryptography-DigitalSignatures'!
Object subclass: #ThirtyTwoBitRegister
	instanceVariableNames: 'hi low'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cryptography-DigitalSignatures'!
!classDefinition: 'ThirtyTwoBitRegister class' category: 'Cryptography-DigitalSignatures'!
ThirtyTwoBitRegister class
	instanceVariableNames: ''!


!DigitalSignatureAlgorithm commentStamp: '<historical>' prior: 0!
This class implements the Digital Signature Algorithm (DSA) of the U.S. government's "Digital Signature Standard" (DSS). The DSA algorithm was proposed in 1991 and became a standard in May 1994. The official description is available as a Federal Information Processing Standards Publication (FIPS PUB 186, May 19, 1994). A companion standard, the Secure Hash Standard, or SHS (FIPS PUB 180-1, April 17, 1995), describes a 160-bit message digest algorithm known as the Secure Hash Algorithm (SHA). This message digest is used to compute the document signature.

Here's how to use it:

  1. The "signer" creates a pair of keys. One of these must be kept private. The other may be freely distributed. For example, it could be built into the signature checking code of an application.

  2. When the signer wishes to sign a packet of data (a "message") , he uses the secure hash algorithm to create a 160-bit message digest (hash) which is used as the input to DSA. The result of this is a pair of large numbers called a "signature" that is attached to the original message.

  3. When someone receives a signed message purported to have come from the signer, they compute the 160-bit hash of the message and pass that, along with the message signature and the signer's public key, to the signature verification algorithm. If the signature checks, then it is virtually guaranteed that the message originated from someone who had the signer's private key. That is, the message is not a forgery and has not been modified since it was signed. For example, if the message contains a program, and the recipient trusts the signer, then the recipient can run the program with the assurance that it won't do anything harmful. (At least, not intentionally. A digital signature is no guarantee against bugs!! :->)

The signer must keep the private key secure, since anyone who has the private key can forge the signer's signature on any message they like. As long as the secret key is not stolen, cryptographers believe it to be virtually impossible either to forge a signature, to find a message that matches an existing sigature, or to discover the signer's private key by analyzing message signatures. Knowing the public key (which, for example, could be recovered from an application that had it built in), does not weaken the security at all.

An excellent reference work on digital signatures and cryptography in general is:

  Schneier, Bruce
  "Applied Cryptography: Protocols, Algorithms, and Source Code in C"
  John Wiley and Sons, 1996.

I used this book as a guide to implementing many of the numerical algorithms required by DSA.

Patents and Export Restrictions:

Many digital signature technologies are patented. DSA is also patented, but the patent is owned by the U.S. government which has made DSA available royalty-free. There is a claim that the government patent infringes on an earlier patent by Schnorr, but the government is requiring the use of DSA, so they apparently believe this claim is not strong enough to be a serious threat to their own patent.

Most cryptography technology, including digital signature technology, requires an export license for it to be distributed outside the U.S. Recent legislation may have relaxed the export license requirements, but it would be prudent to check the current regulations before exporting this code.!

!SecureHashAlgorithm commentStamp: '<historical>' prior: 0!
This class implements the Secure Hash Algorithm (SHA) described in the U.S. government's Secure Hash Standard (SHS). This standard is described in FIPS PUB 180-1, "SECURE HASH STANDARD", April 17, 1995.

The Secure Hash Algorithm is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Scheier, Wiley, 1996.

See the comment in class DigitalSignatureAlgorithm for details on its use.

Implementation notes:
The secure hash standard was created with 32-bit hardware in mind. All arithmetic in the hash computation must be done modulo 2^32. This implementation uses ThirtyTwoBitRegister objects to simulate hardware registers; this implementation is about six times faster than using LargePositiveIntegers (measured on a Macintosh G3 Powerbook). Implementing a primitive to process each 64-byte buffer would probably speed up the computation by a factor of 20 or more.
!

!SecureHashAlgorithm256 commentStamp: '<historical>' prior: 0!
Implements the SHA-256 hash algorithm as defined in https://tools.ietf.org/html/rfc6234!

!ThirtyTwoBitRegister commentStamp: 'ul 4/13/2015 05:24' prior: 0!
I represent a 32-bit register. An instance of me can hold any non-negative integer in the range [0..(2^32 - 1)]. Operations are performed on my contents in place, like a hardware register, and results are always modulo 2^32. All operations avoid LargeInteger arithmetic as much as possible.

I'm mainly used by the SecureHashAlgorithm class, but I can be used for implementing other algorithms designed for 32-bit arithmetic. For examble George Marsaglia's Xorshift PRNG from http://www.jstatsoft.org/v08/i14/paper :

"Internal state."
x := ThirtyTwoBitRegister fromInteger: 123456789.
y := ThirtyTwoBitRegister fromInteger: 362436069.
z := ThirtyTwoBitRegister fromInteger: 521288629.
w := ThirtyTwoBitRegister fromInteger: 88675123.
"Temporaries."
t := ThirtyTwoBitRegister new.
temp := nil.
"The algorithm: t=(x^(x<<11));x=y;y=z;z=w; return( w=(w^(w>>19))ˆ(t^(t>>8)) );"
xorShift128 := [
	"t=(x^(x<<11));"
	t
		loadFrom: x;
		<< 11;
		bitXor: x.
	"x=y;y=z;z=w;"
	temp := x.
	x := y.
	y := z.
	z := w.
	"w=(w^(w>>19))ˆ(t^(t>>8))"
	w := temp.
	w
		loadFrom: z;
		>> 19;
		bitXor: z;
		bitXor: t.
	t >> 8.
	w bitXor: t.
	"Truncate to 30 bits."
	(w hi bitShift: 14) bitXor: w low ].

The code is about 9.5x faster than an implementation using LargeIntegers:

"Implementation using LargeIntegers."
x := 123456789.
y := 362436069.
z := 521288629.
w := 88675123.
largeIntegerXorShift128 := [
	| t |
	t := ((x bitAnd: 16r1FFFFF) bitShift: 11) bitXor: x.
	x := y.
	y := z.
	z := w.
	w := (((w bitShift: -19) bitXor: w) bitXor: t) bitXor: (t bitShift: -8) ].

xorShift128 bench. '4,990,000 per second. 201 nanoseconds per run.'.
largeIntegerXorShift128 bench. '529,000 per second. 1.89 microseconds per run.'.!

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'ul 4/10/2015 20:54'!
>> anInteger
	"Unsigned right shift."

	| bitCount shift |
	bitCount := anInteger.
	bitCount >= 32 ifTrue: [
		hi := low := 0.
		^self ].
	bitCount >= 16 ifTrue: [
		low := hi.
		hi := 0.
		bitCount := bitCount - 16 ].
	bitCount >= 2 ifTrue: [
		shift := 0 - bitCount.
		low := (low bitShift: shift) bitOr: ((hi bitShift: shift + 16) bitAnd: 16rFFFF).
		hi := hi bitShift: shift.
		^self ].
	bitCount >= 1 ifTrue: [
		low := (low bitShift: -1) bitOr: ((hi bitAnd: 16r1) bitShift: 15).
		hi := hi bitShift: -1 ]! !

!ThirtyTwoBitRegister methodsFor: 'printing' stamp: 'laza 3/29/2004 12:22'!
printOn: aStream
	"Print my contents in hex with a leading 'R' to show that it is a register object being printed."

	aStream nextPutAll: 'R:'.
	self asInteger storeOn: aStream base: 16.
! !

!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'nice 9/2/2010 21:48'!
computeSignatureForMessageHash: hash privateKey: privateKey
	"Answer the digital signature of the given message hash using the given private key. A signature is a pair of large integers. The private key is an array of four large integers: (p, q, g, x)."

	| p q g x r s k tmp |
	p := privateKey first.
	q := privateKey second.
	g := privateKey third.
	x := privateKey fourth.

	r := s := 0.
	[r = 0 or: [s = 0]] whileTrue: [
		k := self nextRandom160 \\ q.
		r := (g raisedTo: k modulo: p) \\ q.
		tmp := (hash + (x * r)) \\ q.
		s := ((k reciprocalModulo: q) * tmp) \\ q].

	^ Array with: r with: s
! !

!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'jmv 5/19/2015 21:41'!
generateKeySet
	"Generate and answer a key set for DSA. The result is a pair (<private key><public key>). Each key is an array of four large integers. The private key is (p, q, g, x); the public one is (p, q, g, y). The signer must be sure to record (p, q, g, x), and must keep x secret to prevent someone from forging their signature."
	"Note: Key generation can take some time. Open a transcript so you can see what's happening and take a coffee break!!"

	| qAndPandS q p exp g h x y |
	qAndPandS := self generateQandP.
	Transcript show: 'Computing g...'.
	q := qAndPandS first.
	p := qAndPandS second.
	exp := (p - 1) / q.
	h := 2.
	[g := h raisedTo: exp modulo: p. g = 1] whileTrue: [h := h + 1].
	Transcript show: 'done.'; newLine.
	Transcript show: 'Computing x and y...'.
	x := self nextRandom160.
	y := g raisedTo: x modulo: p.
	Transcript show: 'done.'; newLine.
	Transcript show: 'Key generation complete!!'; newLine.
	^ Array
		with: (Array with: p with: q with: g with: x)
		with: (Array with: p with: q with: g with: y)
! !

!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'md 7/30/2005 14:12'!
signatureToString: aSignature
	"Answer a string representation of the given signature. This string can be parsed using the stringToSignature: method."

	| s |
	s := WriteStream on: (String new: 2000).
	s nextPutAll: '[DSA digital signature '.
	s nextPutAll: aSignature first printStringHex.
	s space.
	s nextPutAll: aSignature second printStringHex.
	s nextPutAll: ']'.
	^ s contents
! !

!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'jm 12/14/1999 13:33'!
stringToSignature: aString
	"Answer the signature stored in the given string. A signature string has the format:

		 '[DSA digital signature <r> <s>]'

	where <r> and <s> are large positive integers represented by strings of hexidecimal digits."

	| prefix stream r s |
	prefix := '[DSA digital signature '.
	(aString beginsWith: prefix) ifFalse: [self error: 'bad signature prefix'].
	stream := ReadStream on: aString.
	stream position: prefix size.
	r := Integer readFrom: stream base: 16.
	stream next.
	s := Integer readFrom: stream base: 16.
	^ Array with: r with: s
! !

!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'nice 9/2/2010 21:48'!
verifySignature: aSignature ofMessageHash: hash publicKey: publicKey
	"Answer true if the given signature is the authentic signature of the given message hash. That is, if the signature must have been computed using the private key set corresponding to the given public key. The public key is an array of four large integers: (p, q, g, y)."

	| p q g y r s w u1 u2 v0 v |
	p := publicKey first.
	q := publicKey second.
	g := publicKey third.
	y := publicKey fourth.
	r := aSignature first.
	s := aSignature last.
	((r > 0) and: [r < q]) ifFalse: [^ false].  "reject"
	((s > 0) and: [s < q]) ifFalse: [^ false].  "reject"

	w := s reciprocalModulo: q.
	u1 := (hash * w) \\ q.
	u2 := (r * w) \\ q.
	v0 := (g raisedTo: u1 modulo: p) * (y raisedTo: u2 modulo: p).
	v := ( v0 \\ p) \\ q.
	^ v = r
! !

!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'jmv 5/19/2015 21:41'!
generateQandP
	"Generate the two industrial-grade primes, q (160-bits) and p (512-bit) needed to build a key set. Answer the array (q, p, s), where s is the seed that from which q and p were created. This seed is normally discarded, but can be used to verify the key generation process if desired."

	| pBits halfTwoToTheP chunkCount sAndq q twoQ n c w x p s |
	pBits := 512.  "desired size of p in bits"
	halfTwoToTheP := 2 raisedTo: (pBits - 1).
	chunkCount := pBits // 160.

	Transcript show: 'Searching for primes q and p...'; newLine.
	[
		sAndq := self generateSandQ.
		Transcript show: '  Found a candidate q.';newLine.
		s := sAndq first.
		q := sAndq last.
		twoQ := q bitShift: 1.
		n := 2.
		c := 0.
		[c < 4096] whileTrue: [
			w := self generateRandomLength: pBits s: s n: n.
			x := w + halfTwoToTheP.
			p := (x - ( x \\ twoQ)) + 1.
			p highBit = pBits ifTrue: [
				Transcript show: '    Testing potential p ', (c + 1) printString, '...'; newLine.
				(self isProbablyPrime: p) ifTrue: [
					Transcript show: '  Found p!!'; newLine .
					^ Array with: q with: p with: s]].
			n := n + chunkCount + 1.
			c := c + 1]] repeat! !

!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'jm 12/13/1999 16:36'!
generateRandomLength: bitLength s: s n: n
	"Answer a random number of bitLength bits generated using the secure hash algorithm."

	| sha out count extraBits v |
	sha := SecureHashAlgorithm new.
	out := 0.
	count := (bitLength // 160).
	extraBits := bitLength - (count * 160).
	0 to: count do: [:k |
		v := sha hashInteger: (s + n + k).
		k = count ifTrue: [
			v := v - ((v >> extraBits) << extraBits)].
		out := out bitOr: (v bitShift: (160 * k))].
	^ out
! !

!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'nice 4/11/2011 17:01'!
generateSandQ
	"Generate a 160-bit random seed s and an industrial grade prime q."

	| hasher s sPlusOne u q |
	hasher := SecureHashAlgorithm new.
	[
		s := self nextRandom160.
		sPlusOne := s + 1.
		sPlusOne highBit > 160 ifTrue: [sPlusOne := sPlusOne bitAnd: (1 bitShift: 160) - 1].
		u := (hasher hashInteger: s) bitXor: (hasher hashInteger: sPlusOne).
		q := u bitOr: ((1 bitShift: 159) bitOr: 1).
		(self isProbablyPrime: q) ifTrue: [^ Array with: s with: q]] repeat! !

!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'nice 8/28/2010 21:04'!
logOfLargestPowerOfTwoDividing: aPositiveInteger
	"Answer the base-2 log of the largest power of two that divides the given integer. For example, the largest power of two that divides 24 is 8, whose log base-2 is 3. Do this efficiently even when the given number is a large integer. Assume that the given integer is > 0."
	"DigitalSignatureAlgorithm new logOfLargestPowerOfTwoDividing: (32 * 3)"

	^aPositiveInteger lowBit - 1! !

!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'jm 12/13/1999 14:39'!
nextRandom160
	"Answer a newly generated 160-bit random number in the range [1..(2^160 - 1)]."
	"Details: Try again in the extremely unlikely chance that zero is encountered."

	| result |
	result := 0.
	[result = 0] whileTrue: [
		result := SecureHashAlgorithm new hashInteger: randKey seed: randSeed.
		randKey := randKey + result + 1].
	^ result
! !

!DigitalSignatureAlgorithm methodsFor: 'initialization' stamp: 'jmv 5/19/2015 21:41'!
initRandom: randomInteger
	"Initialize the the secure random number generator with the given value. The argument should be a positive integer of up to 512 bits chosen randomly to avoid someone being able to predict the sequence of random values generated."
	"Note: The random generator must be initialized before generating a key set or signature. Signature verification does not require initialization of the random generator."

	randSeed := 16rEFCDAB8998BADCFE10325476C3D2E1F067452301.  "initial seed"
	randKey := randomInteger.
	Transcript show: 'Random seed: ', randomInteger printString; newLine.
! !

!DigitalSignatureAlgorithm methodsFor: 'initialization' stamp: 'jmv 9/5/2016 21:13:01'!
initRandomFromString: aString
	"Ask the user to type a long random string and use the result to seed the secure random number generator."

	| s k srcIndex |
	s := aString.
	k := LargePositiveInteger new: (s size min: 64).
	srcIndex := 0.
	k digitLength to: 1 by: -1 do: [:i |
		k digitAt: i put: (s at: (srcIndex := srcIndex + 1)) numericValue].
	k := k normalize + (Random new next * 16r7FFFFFFF) asInteger.  "a few additional bits randomness"
	k highBit > 512 ifTrue: [k := k bitShift: k highBit - 512].
	self initRandom: k.
! !

!DigitalSignatureAlgorithm methodsFor: 'initialization' stamp: 'jmv 9/5/2016 21:13:06'!
initRandomFromUser
	"Ask the user to type a long random string and use the result to seed the secure random number generator."

	| s k srcIndex |
	s := FillInTheBlankMorph request: 'Enter a long random string to seed the random generator.'.
	k := LargePositiveInteger new: (s size min: 64).
	srcIndex := 0.
	k digitLength to: 1 by: -1 do: [:i |
		k digitAt: i put: (s at: (srcIndex := srcIndex + 1)) numericValue].
	k := k normalize + (Random new next * 16r7FFFFFFF) asInteger.  "a few additional bits randomness"
	k highBit > 512 ifTrue: [k := k bitShift: k highBit - 512].
	self initRandom: k.
! !

!DigitalSignatureAlgorithm methodsFor: 'initialization' stamp: 'DSG 12/4/2013 17:41'!
initRandomNonInteractively
	[self initRandom: (SoundSystem default randomBitsFromSoundInput: 512)]
		ifError: [self initRandomFromString: 
			Time millisecondClockValue printString, 
			Date today printString, 
			Smalltalk platformName printString].! !

!DigitalSignatureAlgorithm methodsFor: 'large integer arithmetic' stamp: 'nice 8/28/2010 21:16'!
inverseOf: x mod: n
	"Answer the inverse of x modulus n. That is, the integer y such that (x * y) \\ n is 1. Both x and n must be positive, and it is assumed that x < n and that x and n are integers."
	"Details: Use the extended Euclidean algorithm, Schneier, p. 247."

	| v u u1 u2 u3 t1 t2 t3 tmp |
	((x <= 0) or: [n <= 0]) ifTrue: [self error: 'x and n must be greater than zero'].
	x >= n ifTrue: [self error: 'x must be < n'].

	v := x.
	u := n.
	(x even and: [n even]) ifTrue: [self error: 'no inverse'].

	u1 := 1. u2 := 0. u3 := u.
	t1 := v. t2 := u - 1. t3 := v.
	[	[u3 even ifTrue: [
			((u1 odd) or: [u2 odd]) ifTrue: [
				u1 := u1 + v.
				u2 := u2 + u].
			u1 := u1 bitShift: -1.
			u2 := u2 bitShift: -1.
			u3 := u3 bitShift: -1].
		((t3 even) or: [u3 < t3]) ifTrue: [
			tmp := u1. u1 := t1. t1 := tmp.
			tmp := u2. u2 := t2. t2 := tmp.
			tmp := u3. u3 := t3. t3 := tmp].
		u3 even and: [u3 > 0]] whileTrue: ["loop while u3 is even"].

		[((u1 < t1) or: [u2 < t2]) and: [u1 > 0]] whileTrue: [
			u1 := u1 + v.
			u2 := u2 + u].
	
		u1 := u1 - t1.
		u2 := u2 - t2.
		u3 := u3 - t3.
		t3 > 0] whileTrue: ["loop while t3 > 0"].

	[u1 >= v and: [u2 >= u]] whileTrue: [
		u1 := u1 - v.
		u2 := u2 - u].

	u3 = 1 ifFalse: [self error: 'no inverse'].
	^ u - u2
! !

!DigitalSignatureAlgorithm methodsFor: 'large integer arithmetic' stamp: 'jmv 5/19/2015 21:46'!
isProbablyPrime: p
	"Answer true if p is prime with very high probability. Such a number is sometimes called an 'industrial grade prime'--a large number that is so extremely likely to be prime that it can assumed that it actually is prime for all practical purposes. This implementation uses the Rabin-Miller algorithm (Schneier, p. 159)."

	| iterations factor pMinusOne b m r a j z couldBePrime |
	iterations := 50.  "Note: The DSA spec requires >50 iterations; Schneier says 5 are enough (p. 260)"

	"quick elimination: check for p divisible by a small prime"
	SmallPrimes ifNil: [  "generate list of small primes > 2"
		SmallPrimes := Integer primesUpTo: 2000.
		SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size].
	factor := SmallPrimes detect: [:f | (p \\ f) = 0] ifNone: [nil].
	factor ifNotNil: [^ p = factor].

	pMinusOne := p - 1.
	b := self logOfLargestPowerOfTwoDividing: pMinusOne.
	m := pMinusOne bitShift: b negated.
	"Assert: pMinusOne = m * (2 raisedTo: b) and m is odd"

	Transcript show: '      Prime test pass '.
	r := Random new.
	1 to: iterations do: [:i |
		Transcript show: i printString; space.
		a := (r next * 16rFFFFFF) truncated.
		j := 0.
		z := (a raisedTo: m modulo: p) normalize.
		couldBePrime := z = 1.
		[couldBePrime] whileFalse: [
			z = 1 ifTrue: [Transcript show: 'failed!!'; newLine. ^ false].  "not prime"
			z = pMinusOne
				ifTrue: [couldBePrime := true]
				ifFalse: [
					(j := j + 1) < b
						ifTrue: [z := (z * z) \\ p]
						ifFalse: [Transcript show: 'failed!!'; newLine. ^ false]]]].  "not prime"

	Transcript show: 'passed!!'; newLine.
	^ true  "passed all tests; probably prime"
! !

!DigitalSignatureAlgorithm class methodsFor: 'examples' stamp: 'jm 12/22/1999 11:23'!
example
	"Example of signing a message and verifying its signature."
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm example"

	| msg keys sig |
	msg := 'This is a test...'.
	keys := self testKeySet.
	sig := self sign: msg privateKey: keys first.
	self inform: 'Signature created'.
	(self verify: sig isSignatureOf: msg publicKey: keys last)
		ifTrue: [self inform: 'Signature verified.']
		ifFalse: [self error: 'ERROR!! Signature verification failed'].
! !

!DigitalSignatureAlgorithm class methodsFor: 'examples' stamp: 'pb 5/25/2016 01:32'!
testExamplesFromDisk
	"verify messages from file on disk"
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm testExamplesFromDisk"

	'dsa.test.out' asFileEntry readStreamDo: [ :stream |
		| msg  sig publicKey |
		[stream atEnd] whileFalse: [
			sig := stream nextChunk.
			msg := stream nextChunk.
			publicKey := Compiler evaluate: stream nextChunk.
			(self verify: sig isSignatureOf: msg publicKey: publicKey) ifTrue: [
				Transcript show: 'SUCCESS: ',msg; newLine.
			] ifFalse: [
				self error: 'ERROR!! Signature verification failed'
			].
		].
	]! !

!DigitalSignatureAlgorithm class methodsFor: 'examples' stamp: 'jm 12/22/1999 11:28'!
testKeySet
	"Answer a pair of keys for testing. The first key is the private key, the second one is the public key."
	"WARNING: This test key set is public should be used only for testing!! In a real application, the user would create a set of keys using generateKeySet and would keep the private key secret."

	^ #(
		(8343811888543852523216773185009428259187948644369498021763210776677854991854533186365944349987509452133156416880596803846631577352387751880552969116768071 1197175832754339660404549606408619548226315875117 1433467472198821951822151391684734233265646022897503720591270330985699984763922266163182803556189497900262038518780931942996381297743579119123094520048965 957348690772296812)
		(8343811888543852523216773185009428259187948644369498021763210776677854991854533186365944349987509452133156416880596803846631577352387751880552969116768071 1197175832754339660404549606408619548226315875117 1433467472198821951822151391684734233265646022897503720591270330985699984763922266163182803556189497900262038518780931942996381297743579119123094520048965 4645213122572190617807944614677917601101008235397095646475699959851618402406173485853587185431290863173614335452934961425661774118334228449202337038283799))
! !

!DigitalSignatureAlgorithm class methodsFor: 'examples' stamp: 'jmv 9/5/2016 20:48:35'!
timeDecode: count
	"Example of signing a message and verifying its signature."
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm timeDecode: 20"

	| dsa |
	dsa := DigitalSignatureAlgorithm new.
	dsa initRandomFromUser.

	#(1 10 100 1000 10000 100000) do: [ :extraLen |
		| msg keys sig s |
		s := String new: extraLen.
		1 to: s size do: [ :i | s at: i put: (Character numericValue: 200 atRandom)].
		msg := 'This is a test...',s.
		keys := self testKeySet.
		sig := self sign: msg privateKey: keys first dsa: dsa.
		"self inform: 'Signature created'."
		self timeDirect: [
			count timesRepeat: [
				(self verify: sig isSignatureOf: msg publicKey: keys last)
					ifFalse: [self error: 'ERROR!! Signature verification failed'].
			].
		] as: 'verify msgLen = ',msg size printString count: count
	].
! !

!DigitalSignatureAlgorithm class methodsFor: 'examples' stamp: 'pb 5/25/2016 01:36'!
writeExamplesToDisk
	"Example of signing a message and verifying its signature. Used to create samples from one implementation that could later be tested with a different implementation"
	"Note: Secure random numbers are needed for key generation and message signing, but not for signature verification. There is no need to call initRandomFromUser if you are merely checking a signature."
	"DigitalSignatureAlgorithm writeExamplesToDisk"

	| keyList dsa msgList |
	dsa := DigitalSignatureAlgorithm new.
	dsa initRandomFromUser.
	self inform: 'About to generate 5 key sets. Will take a while'.
	keyList := {self testKeySet},((1 to: 5) collect: [ :ignore | self generateKeySet]).
	msgList := {'This is a test...'. 'This is the second test period.'. 'And finally, a third message'}.
	'dsa.test.out' asFileEntry writeStreamDo: [ :stream |
		msgList do: [ :msg |
			keyList do: [ :keys |
				| sig |
				sig := self sign: msg privateKey: keys first dsa: dsa.
				(self verify: sig isSignatureOf: msg publicKey: keys last) ifTrue: [
					stream
						nextChunkPut: sig;
						nextChunkPut: msg;
						nextChunkPut: keys last storeString.
				] ifFalse: [
					self error: 'ERROR!! Signature verification failed'
				].
			].
		].
	]! !

!DigitalSignatureAlgorithm class methodsFor: 'public' stamp: 'ads 7/31/2003 14:01'!
generateKeySet
	"Generate and answer a key set for code signing. The result is a pair (<private key><public key>). Each key is an array of four large integers. The signer must be sure to record this keys set and must keep the private key secret to prevent someone from forging their signature."
	"Note: Key generation can take some time. Open a transcript so you can see what's happening and take a coffee break!!"
	"Note: Unguessable random numbers are needed for key generation. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before generating a key set. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."
	"DigitalSignatureAlgorithm generateKeySet"

	| dsa |
	dsa := DigitalSignatureAlgorithm new.
	(self confirm: 'Shall I seed the random generator from the current sound input?')
		ifTrue: [dsa initRandomNonInteractively]
		ifFalse: [dsa initRandomFromUser].
	^ dsa generateKeySet
! !

!DigitalSignatureAlgorithm class methodsFor: 'public' stamp: 'jm 12/22/1999 11:18'!
sign: aStringOrStream privateKey: privateKey
	"Sign the given message (a stream or string) and answer a signature string."
	"Note: Unguessable random numbers are needed for message signing. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before signing a message. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."

	| dsa hasher h sig |
	dsa := DigitalSignatureAlgorithm new.
	dsa initRandomFromUser.
	hasher := SecureHashAlgorithm new.
	(aStringOrStream class isBytes)
		ifTrue: [h := hasher hashMessage: aStringOrStream]
		ifFalse: [h := hasher hashStream: aStringOrStream].
	sig := dsa computeSignatureForMessageHash: h privateKey: privateKey.
	^ dsa signatureToString: sig
! !

!DigitalSignatureAlgorithm class methodsFor: 'public' stamp: 'RAA 5/31/2000 08:46'!
sign: aStringOrStream privateKey: privateKey dsa: dsa
	"Sign the given message (a stream or string) and answer a signature string."
	"Note: Unguessable random numbers are needed for message signing. The user will be prompted to type a really long random string (two or three lines) to initialize the random number generator before signing a message. A different random string should be typed for every session; it is not a password and we wish to produce different random number streams."

	| hasher h sig |

	hasher := SecureHashAlgorithm new.
	(aStringOrStream class isBytes)
		ifTrue: [h := hasher hashMessage: aStringOrStream]
		ifFalse: [h := hasher hashStream: aStringOrStream].
	sig := dsa computeSignatureForMessageHash: h privateKey: privateKey.
	^ dsa signatureToString: sig
! !

!DigitalSignatureAlgorithm class methodsFor: 'public' stamp: 'jm 12/22/1999 11:20'!
verify: signatureString isSignatureOf: aStringOrStream publicKey: publicKey
	"Answer true if the given signature string signs the given message (a stream or string)."
	"Note: Random numbers are not needed for signature verification; thus, there is no need to call initRandomFromUser before verifying a signature."

	| dsa hasher h sig |
	dsa := DigitalSignatureAlgorithm new.
	hasher := SecureHashAlgorithm new.
	(aStringOrStream class isBytes)
		ifTrue: [h := hasher hashMessage: aStringOrStream]
		ifFalse: [h := hasher hashStream: aStringOrStream].
	sig := dsa stringToSignature: signatureString.
	^ dsa verifySignature: sig ofMessageHash: h publicKey: publicKey
! !

!DigitalSignatureAlgorithm class methodsFor: 'class initialization' stamp: 'jm 12/21/1999 19:15'!
initialize
	"DigitalSignatureAlgorithm initialize"

	"SmallPrimes is a list of small primes greater than two."
	SmallPrimes := Integer primesUpTo: 2000.
	SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size.

	"HighBitOfByte maps a byte to the index of its top non-zero bit."
	HighBitOfByte := (0 to: 255) collect: [:byte | byte highBit].
! !

!DigitalSignatureAlgorithm class methodsFor: 'testing' stamp: 'RAA 5/31/2000 08:21'!
time: aBlock as: aString count: anInteger

	^{anInteger. aString. (Time millisecondsToRun: aBlock)}! !

!DigitalSignatureAlgorithm class methodsFor: 'testing' stamp: 'jmv 5/19/2015 21:48'!
timeDirect: aBlock as: aString count: anInteger

	Transcript show: anInteger printStringWithCommas,'  ',
		aString ,' took ',
		(Time millisecondsToRun: aBlock) printStringWithCommas,' ms'; newLine
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 23:25'!
constantForStep: i
	"Answer the constant for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."

	i <= 20 ifTrue: [^ K1].
	i <= 40 ifTrue: [^ K2].
	i <= 60 ifTrue: [^ K3].
	^ K4
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'nice 8/28/2010 22:43'!
expandedBlock: aByteArray
	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result." 
	| out src v |
	out := Array new: 80.
	src := 1.
	1 to: 16 do: [:i |
		out at: i put: (ThirtyTwoBitRegister fromByteArray: aByteArray at: src).
		src := src + 4].

	17 to: 80 do: [:i |
		v := (out at: i - 3) copy.
		v	bitXor: (out at: i - 8);
			bitXor: (out at: i - 14);
			bitXor: (out at: i - 16);
			leftRotateBy: 1.
		out at: i put: v].
	^ out
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 20:02'!
finalHash
	"Concatenate the final totals to build the 160-bit integer result."
	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."

	| r |
	totals ifNil: [  "compute final hash when not using primitives"
		^ (totalA asInteger bitShift: 128) +
		  (totalB asInteger bitShift:  96) +
		  (totalC asInteger bitShift:  64) +
		  (totalD asInteger bitShift:  32) +
		  (totalE asInteger)].

	"compute final hash when using primitives"
	r := 0.
	1 to: 5 do: [:i |
		r := r bitOr: ((totals at: i) bitShift: (32 * (5 - i)))].
	^ r
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 22:15'!
hashFunction: i of: x with: y with: z
	"Compute the hash function for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."
	"Details: There are four functions, one for each 20 iterations. The second and fourth are the same."

	i <= 20 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)].
	i <= 40 ifTrue: [^ x copy bitXor: y; bitXor: z].
	i <= 60 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitAnd: z); bitOr: (y copy bitAnd: z)].
	^ x copy bitXor: y; bitXor: z
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'nice 8/28/2010 22:39'!
initializeTotals
	"Initialize totalA through totalE to their seed values."

	"total registers for use when primitives are absent"
	totalA := ThirtyTwoBitRegister fromInteger: 16r67452301.
	totalB := ThirtyTwoBitRegister fromInteger: 16rEFCDAB89.
	totalC := ThirtyTwoBitRegister fromInteger: 16r98BADCFE.
	totalD := ThirtyTwoBitRegister fromInteger: 16r10325476.
	totalE := ThirtyTwoBitRegister fromInteger: 16rC3D2E1F0.
	self initializeTotalsArray.
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:38'!
initializeTotalsArray
	"Initialize the totals array from the registers for use with the primitives."

	totals := Bitmap new: 5.
	totals at: 1 put: totalA asInteger.
	totals at: 2 put: totalB asInteger.
	totals at: 3 put: totalC asInteger.
	totals at: 4 put: totalD asInteger.
	totals at: 5 put: totalE asInteger.
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'nice 8/28/2010 22:28'!
processBuffer: aByteArray
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."

	| a b c d e w tmp |
	self primHasSecureHashPrimitive
		ifTrue: [^ self processBufferUsingPrimitives: aByteArray]
		ifFalse: [totals := nil].

	"initialize registers a through e from the current totals" 
	a := totalA copy.
	b := totalB copy.
	c := totalC copy.
	d := totalD copy.
	e := totalE copy.

	"expand and process the buffer"
	w := self expandedBlock: aByteArray.
	1 to: 80 do: [:i |
		tmp := (a copy leftRotateBy: 5)
			+= (self hashFunction: i of: b with: c with: d);
			+= e;
			+= (w at: i);
			+= (self constantForStep: i).
		e := d.
		d := c.
		c := b leftRotateBy: 30.
		b := a.
		a := tmp].

	"add a through e into total accumulators"
	totalA += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e.
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 23:32'!
processBufferUsingPrimitives: aByteArray
	"Process given 64-byte buffer using the primitives, accumulating the results in totals."

	| w |
	"expand and process the buffer"
	w := Bitmap new: 80.
	self primExpandBlock: aByteArray into: w.
	self primHashBlock: w using: totals.
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:40'!
processFinalBuffer: buffer bitLength: bitLength
	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."

	| out |
	out := ByteArray new: 64.
	out replaceFrom: 1 to: buffer size with: buffer startingAt: 1.
	buffer size < 56 ifTrue: [  "padding and length fit in last data block"
		out at: buffer size + 1 put: 128.  "trailing one bit"
		self storeLength: bitLength in: out.  "end with length"
		self processBuffer: out.
		^ self].

	"process the final data block"
	buffer size < 64 ifTrue: [
		out at: buffer size + 1 put: 128].  "trailing one bit"
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.  "filled with zeros"
	buffer size = 64 ifTrue: [
		"add trailing one bit that didn't fit in final data block"
		out at: 1 put: 128].
	self storeLength: bitLength in: out.
	self processBuffer: out.
! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:10'!
storeLength: bitLength in: aByteArray
	"Fill in the final 8 bytes of the given ByteArray with a 64-bit big-endian representation of the original message length in bits."

	| n i |
	n := bitLength.
	i := aByteArray size.
	[n > 0] whileTrue: [
		aByteArray at: i put: (n bitAnd: 16rFF).
		n := n bitShift: -8.
		i := i - 1].
! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'jm 12/14/1999 11:56'!
hashInteger: aPositiveInteger
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in key generation."

	| buffer dstIndex |
	self initializeTotals.

	"pad integer with zeros"
	aPositiveInteger highBit <= 512
		ifFalse: [self error: 'integer cannot exceed 512 bits'].
	buffer := ByteArray new: 64.
	dstIndex := 0.
	aPositiveInteger digitLength to: 1 by: -1 do: [:i |
		buffer at: (dstIndex := dstIndex + 1) put: (aPositiveInteger digitAt: i)].

	"process that one block"
	self processBuffer: buffer.

	^ self finalHash
! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'nice 8/28/2010 22:40'!
hashInteger: aPositiveInteger seed: seedInteger
	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in the production of random numbers"

	| buffer dstIndex |
	"Initialize totalA through totalE to their seed values."
	totalA := ThirtyTwoBitRegister
		fromInteger: ((seedInteger bitShift: -128) bitAnd: 16rFFFFFFFF).
	totalB := ThirtyTwoBitRegister
		fromInteger: ((seedInteger bitShift: -96) bitAnd: 16rFFFFFFFF).
	totalC := ThirtyTwoBitRegister
		fromInteger: ((seedInteger bitShift: -64) bitAnd: 16rFFFFFFFF).
	totalD := ThirtyTwoBitRegister
		fromInteger: ((seedInteger bitShift: -32) bitAnd: 16rFFFFFFFF).
	totalE := ThirtyTwoBitRegister
		fromInteger: (seedInteger bitAnd: 16rFFFFFFFF).
	self initializeTotalsArray.

	"pad integer with zeros"
	buffer := ByteArray new: 64.
	dstIndex := 0.
	aPositiveInteger digitLength to: 1 by: -1 do: [:i |
		buffer at: (dstIndex := dstIndex + 1) put: (aPositiveInteger digitAt: i)].

	"process that one block"
	self processBuffer: buffer.

	^ self finalHash
! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'jm 12/14/1999 11:28'!
hashMessage: aStringOrByteArray
	"Hash the given message using the Secure Hash Algorithm."

	^ self hashStream: (ReadStream on: aStringOrByteArray asByteArray)
! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'ar 2/25/2010 23:40'!
hashStream: aPositionableStream
	"Hash the contents of the given stream from the current position to the end using the Secure Hash Algorithm. The SHA algorithm is defined in FIPS PUB 180-1. It is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Scheier, Wiley, 1996."
	"SecureHashAlgorithm new hashStream: (ReadStream on: 'foo')"

	| startPosition buf bitLength |
	self initializeTotals.

	"(SecureHashAlgorithm new hashMessage: '') radix: 16 	
	=> 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'"
	aPositionableStream atEnd ifTrue: [self processFinalBuffer: #() bitLength: 0].

	startPosition := aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: [
		buf := aPositionableStream next: 64.
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [
				bitLength := (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength]].

	^ self finalHash
! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:11'!
primExpandBlock: aByteArray into: wordBitmap
	"Expand the given 64-byte buffer into the given Bitmap of length 80."

	<primitive: 'primitiveExpandBlock' module: 'DSAPrims'>
	^ self primitiveFailed
! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 22:58'!
primHasSecureHashPrimitive
	"Answer true if this platform has primitive support for the Secure Hash Algorithm."

	<primitive: 'primitiveHasSecureHashPrimitive' module: 'DSAPrims'>
	^ false
! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:13'!
primHashBlock: blockBitmap using: workingTotalsBitmap
	"Hash the given block (a Bitmap) of 80 32-bit words, using the given workingTotals."

	<primitive: 'primitiveHashBlock' module: 'DSAPrims'>
	^ self primitiveFailed
! !

!SecureHashAlgorithm class methodsFor: 'class initialization' stamp: 'nice 8/28/2010 22:39'!
initialize
	"SecureHashAlgorithm initialize"
	"For the curious, here's where these constants come from:
	  #(2 3 5 10) collect: [:x | ((x sqrt / 4.0) * (2.0 raisedTo: 32)) truncated hex]"

	K1 := ThirtyTwoBitRegister fromInteger: 16r5A827999.
	K2 := ThirtyTwoBitRegister fromInteger: 16r6ED9EBA1.
	K3 := ThirtyTwoBitRegister fromInteger: 16r8F1BBCDC.
	K4 := ThirtyTwoBitRegister fromInteger: 16rCA62C1D6.
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 23:05:57'!
expandedBlock: aByteArray
	"Convert the given 64 byte buffer into 64 32-bit registers and answer the result." 
	| out src |
	out _ Array new: 64.
	src _ 1.
	
	1 to: 16 do: [:i |
		out at: i put: (ThirtyTwoBitRegister fromByteArray: aByteArray at: src).
		src _ src + 4].

	17 to: 64 do: [:t |
		| v1 v2 v3 v4 result |
		"SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)"
		"SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)"
		"Wt = SSIG1(W(t-2)) + W(t-7) + SSIG0(w(t-15)) + W(t-16)"
		v1 _ (out at: t - 2) copy.
		v2 _ (out at: t - 7) copy.
		v3 _ (out at: t - 15) copy.
		v4 _ (out at: t - 16) copy.
		
		result _ self smallSigmaOne: v1.
		result += v2.
		result += (self smallSigmaZero: v3).
		result += v4.
		
		out at: t put: result ].
	
	^ out
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 14:18:46'!
finalHash
	"Concatenate the final totals to build the 256-bit integer result."

	^ (totalA asInteger bitShift: 224) +
	   (totalB asInteger bitShift: 192) +
	   (totalC asInteger bitShift: 160) +
	   (totalD asInteger bitShift: 128) +
	   (totalE asInteger bitShift:  96) +
	   (totalF asInteger bitShift: 64) +
	   (totalG asInteger bitShift: 32) +
	   (totalH asInteger)! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 13:59:19'!
findConstantBy: aSmallInteger 
	^(RoundConstants at: aSmallInteger) copy! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 23:32:38'!
hash1With: register1 with: register2 with: register3 
	 "BSIG1(e) + CH(e,f,g) "
	| result chResult |
	result _ register1 copy.
	
	"BSIG1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)"
	result rightRotateBy: 6.
	result bitXor: (register1 copy rightRotateBy: 11).
	result bitXor: (register1 copy rightRotateBy: 25).
	
	"CH( x, y, z) = (x AND y) XOR ( (NOT x) AND z)"
	chResult _ register1 copy.
	chResult bitAnd: register2.
	chResult bitXor: (register1 copy bitInvert bitAnd: register3).
	
	result += chResult.
	
	^result.! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 23:37:28'!
hash2With: register1 with: register2 with: register3
	| result majResult |

	"BSIG0(a) + MAJ(a,b,c)" 
	result _ register1 copy.

	"BSIG0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)"
	result rightRotateBy: 2.
	result bitXor: (register1 copy rightRotateBy: 13).
	result bitXor: (register1 copy rightRotateBy: 22).
	
	"MAJ( x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z)"
	majResult _ register1 copy.
	majResult bitAnd: register2.
	majResult bitXor: (register1 copy bitAnd: register3).
	majResult bitXor: (register2 copy bitAnd: register3).
	
	result += majResult.
	
	^result! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 4/25/2020 19:19:04'!
hashMessage: aStringOrByteArray
	"Hash the given message using the Secure Hash Algorithm 256."

	^ self hashStream: (ReadStream on: aStringOrByteArray asByteArray)
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 4/30/2020 18:12:30'!
hashStream: aPositionableStream
	| startPosition buf bitLength |
	self initializeTotals.

	aPositionableStream atEnd ifTrue: [self processFinalBuffer: #() bitLength: 0].

	startPosition _ aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: [
		buf _ aPositionableStream next: 64.
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [
				bitLength _ (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength]].

	^ self finalHash
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/3/2020 23:01:19'!
initializeTotals
	| initialValues |
	initialValues _ self class makeHashValuesArray.
	
	totalA _ initialValues at: 1.
	totalB _ initialValues at: 2.
	totalC _ initialValues at: 3.
	totalD _ initialValues at: 4.
	totalE _ initialValues at: 5.
	totalF _ initialValues at: 6.
	totalG _ initialValues at: 7.
	totalH _ initialValues at: 8.
	! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/3/2020 23:01:56'!
processBuffer: aByteArray
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."
	| a b c d e f g h w tmp1 tmp2 |

	"initialize registers a through e from the current totals" 
	a _ totalA copy.
	b _ totalB copy.
	c _ totalC copy.
	d _ totalD copy.
	e _ totalE copy.
	f _ totalF copy. 
	g _ totalG copy.
	h _ totalH copy.

	"expand and process the buffer"
	w _ self expandedBlock: aByteArray.
	1 to: 64 do: [:i |
		tmp1 _ h copy.
		tmp1 += (self hash1With: e with: f with: g). "T1 = h + BSIG1(e) + CH(e,f,g) + Kt + Wt"
		tmp1 += (self findConstantBy: i).
		tmp1 += (w at: i).
		
		tmp2 _ self hash2With: a with: b with: c. "T2 = BSIG0(a) + MAJ(a,b,c)"
			
		h _ g.
		g _ f.
           f _ e.
           e _ d copy += tmp1.
           d _ c.
           c _ b.
           b _ a.
		a _ tmp1 copy += tmp2.
	 ].

	"add a through e into total accumulators"
	totalA  += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e.
	totalF += f.
	totalG += g.
	totalH += h.
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 14:39:11'!
processFinalBuffer: buffer bitLength: bitLength 
	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."

	| out |
	out _ ByteArray new: 64.
	out replaceFrom: 1 to: buffer size with: buffer startingAt: 1.

	buffer size < 56 ifTrue: [  "padding and length fit in last data block"
		out at: buffer size + 1 put: 128.  "trailing one bit"
		self storeLength: bitLength in: out.  "end with length"
		self processBuffer: out.
		^ self].

	"process the final data block"
	buffer size < 64 ifTrue: [
		out at: buffer size + 1 put: 128].  "trailing one bit"

	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out _ ByteArray new: 64.  "filled with zeros"
	buffer size = 64 ifTrue: [
		"add trailing one bit that didn't fit in final data block"
		out at: 1 put: 128].
	
	self storeLength: bitLength in: out.
	self processBuffer: out.
! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 11:09:27'!
smallSigmaOne: register
	"SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)"
	| result v1 v2 |
	result _ register copy.
	v1 _ register copy.
	v2 _ register copy.
	
	result rightRotateBy: 17.
	v1 rightRotateBy: 19.
	v2 >> 10.
	
	result bitXor: v1.
	result bitXor: v2.

	^result
	! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 5/2/2020 11:07:55'!
smallSigmaZero: register
	"SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)"
	| result v1 v2 |
	result _ register copy.
	v1 _ register copy.
	v2 _ register copy.
	
	result rightRotateBy: 7.
	v1 rightRotateBy: 18.
	v2 >> 3.
	
	result bitXor: v1.
	result bitXor: v2.
	
	^result! !

!SecureHashAlgorithm256 methodsFor: 'as yet unclassified' stamp: 'jpb 4/27/2020 17:07:04'!
storeLength: bitLength in: aByteArray
	"Fill in the final 8 bytes of the given ByteArray with a 64-bit big-endian representation of the original message length in bits."

	| n i |
	n _ bitLength.
	i _ aByteArray size.
	
	[n > 0] whileTrue: [
		aByteArray at: i put: (n bitAnd: 16rFF).
		n _ n bitShift: -8.
		i _ i - 1].
! !

!SecureHashAlgorithm256 class methodsFor: 'as yet unclassified' stamp: 'jpb 4/25/2020 18:41:51'!
initialize
	HashValues _ self makeHashValuesArray.
	RoundConstants _ self makeRoundConstantsArray.! !

!SecureHashAlgorithm256 class methodsFor: 'as yet unclassified' stamp: 'jpb 4/25/2020 18:46:21'!
makeHashValuesArray
	| constants |
	constants _ #(
		16r6A09E667 16rBB67AE85 16r3C6EF372 16rA54FF53A
		16r510E527F 16r9B05688C 16r1F83D9AB 16r5BE0CD19
	).
	^constants collect: [:const| ThirtyTwoBitRegister fromInteger: const ]! !

!SecureHashAlgorithm256 class methodsFor: 'as yet unclassified' stamp: 'jpb 5/3/2020 22:18:14'!
makeRoundConstantsArray
	| constants |
	constants _ #(
		16r428A2F98 16r71374491 16rB5C0FBCF 16rE9B5DBA5 16r3956C25B
 		16r59F111F1 16r923F82A4 16rAB1C5ED5  16rD807AA98 16r12835B01
		16r243185BE 16r550C7DC3 16r72BE5D74 16r80DEB1FE 16r9BDC06A7
		16rC19BF174 16rE49B69C1 16rEFBE4786 16r0FC19DC6 16r240CA1CC
		16r2DE92C6F 16r4A7484AA 16r5CB0A9DC 16r76F988DA 16r983E5152
		16rA831C66D 16rB00327C8 16rBF597FC7 16rC6E00BF3 16rD5A79147
		16r06CA6351 16r14292967  16r27B70A85 16r2E1B2138 16r4D2C6DFC
		16r53380D13 16r650A7354 16r766A0ABB 16r81C2C92E 16r92722C85
		16rA2BFE8A1 16rA81A664B 16rC24B8B70 16rC76C51A3 16rD192E819
		16rD6990624 16rF40E3585 16r106AA070 16r19A4C116 16r1E376C08
		16r2748774C 16r34B0BCB5 16r391C0CB3 16r4ED8AA4A 16r5B9CCA4F
		16r682E6FF3  16r748F82EE 16r78A5636F 16r84C87814 16r8CC70208
		16r90BEFFFA 16rA4506CEB 16rBEF9A3F7 16rC67178F2
	).
	^constants collect: [:const | ThirtyTwoBitRegister fromInteger: const ].! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'ul 4/11/2015 02:24'!
*= aThirtTwoBitRegister
	"Replace my contents with the product of the given register and my current contents."

	| otherLow otherHi mul newLow newHi |
	otherLow := aThirtTwoBitRegister low.
	otherHi := aThirtTwoBitRegister hi.
	"Multiply low with otherLow. Process the two highest bits of low separately if necessary to avoid LargeInteger operations."
	(low bitShift: -8) * (otherLow bitShift: -8) > 16r3FFF
		ifTrue: [ 
			mul := (low bitAnd: 16r3FFF) * otherLow. "Without the two most significant bits of low."
			newLow := (mul bitAnd: 16rFFFF).
			newHi := (mul bitShift: -16).
			mul := (low bitShift: -14) * otherLow. "The two most significant bits of low"
			newLow := newLow + ((mul bitAnd: 16r3) bitShift: 14).
			newHi := newHi + (mul bitShift: -2) + (newLow bitShift: -16) "Carry from newLow" ]
		ifFalse: [
			newLow := low * otherLow. "We'll trim newLow at the end of the method."
			newHi := newLow bitShift: -16 ].
	"Multiply hi with otherLow."
	(hi bitShift: -8) * (otherLow bitShift: -8) > 16r3FFF
		ifTrue: [
			newHi := newHi + 
				((hi bitAnd: 16r3FFF) * otherLow bitAnd: 16rFFFF) +
				(((hi bitShift: -14) * otherLow bitAnd: 16r3) bitShift: 14) ]
		ifFalse: [ newHi := newHi + (hi * otherLow bitAnd: 16rFFFF) ].
	"Multiply low with otherHi."
	(low bitShift: -8) * (otherHi bitShift: -8) > 16r3FFF
		ifTrue: [
			newHi := newHi + 
				((low bitAnd: 16r3FFF) * otherHi bitAnd: 16rFFFF) +
				(((low bitShift: -14) * otherHi bitAnd: 16r3) bitShift: 14) ]
		ifFalse: [ newHi := newHi + (low * otherHi bitAnd: 16rFFFF) ].
	"Truncate and store the results."
	hi := newHi bitAnd: 16rFFFF.
	low := newLow bitAnd: 16rFFFF 
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:36'!
+= aThirtTwoBitRegister
	"Replace my contents with the sum of the given register and my current contents."

	| lowSum |
	lowSum := low + aThirtTwoBitRegister low.
	hi := (hi + aThirtTwoBitRegister hi + (lowSum bitShift: -16)) bitAnd: 16rFFFF.
	low := lowSum bitAnd: 16rFFFF.
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'ul 4/11/2015 04:39'!
<< anInteger
	"Unsigned left shift."

	| bitCount |
	bitCount := anInteger.
	bitCount >= 32 ifTrue: [
		hi := low := 0.
		^self ].
	bitCount >= 16 ifTrue: [
		hi := low.
		low := 0.
		bitCount := bitCount - 16 ].
	bitCount >= 15 ifTrue: [
		hi := ((hi bitAnd: 1) bitShift: 15) bitOr: (low bitShift: -1).
		low := (low bitAnd: 1) bitShift: 15.
		^self ].
	bitCount >= 1 ifTrue: [
		hi := ((hi bitShift: bitCount) bitAnd: 16rFFFF) bitOr: (low bitShift: bitCount - 16).
		low := (low bitShift: bitCount) bitAnd: 16rFFFF ]! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:41'!
bitAnd: aThirtTwoBitRegister
	"Replace my contents with the bitwise AND of the given register and my current contents."

	hi := hi bitAnd: aThirtTwoBitRegister hi.
	low := low bitAnd: aThirtTwoBitRegister low.
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitInvert
	"Replace my contents with the bitwise inverse my current contents."

	hi := hi bitXor: 16rFFFF.
	low := low bitXor: 16rFFFF.
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitOr: aThirtTwoBitRegister
	"Replace my contents with the bitwise OR of the given register and my current contents."

	hi := hi bitOr: aThirtTwoBitRegister hi.
	low := low bitOr: aThirtTwoBitRegister low.
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:38'!
bitXor: aThirtTwoBitRegister
	"Replace my contents with the bitwise exclusive OR of the given register and my current contents."

	hi := hi bitXor: aThirtTwoBitRegister hi.
	low := low bitXor: aThirtTwoBitRegister low.
! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'ul 3/6/2015 02:50'!
leftRotateBy: bits
	"Rotate my contents left by the given number of bits, retaining exactly 32 bits."
	"Details: Perform this operation with no LargeInteger arithmetic."

	| bitCount newHi |
	bitCount := bits.
	bitCount >= 32 ifTrue: [ bitCount := bitCount \\ 32 ].
	bitCount >= 16 ifTrue: [
		newHi := low.
		low := hi.
		hi := newHi.
		bitCount := bitCount - 16 ].
	bitCount >= 15 ifTrue: [
		newHi := ((hi bitAnd: 16r1) bitShift: 15) bitOr: (low bitShift: -1).
		low := ((low bitAnd: 16r1) bitShift: 15) bitOr: (hi bitShift: -1).
		hi := newHi.
		^self ].
	bitCount >= 1 ifTrue: [
		| shift |
		shift := bitCount - 16.
		newHi := ((hi bitShift: bitCount) bitAnd: 16rFFFF) bitOr: (low bitShift: shift).
		low := ((low bitShift: bitCount) bitAnd: 16rFFFF) bitOr: (hi bitShift: shift).
		hi := newHi ]! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jpb 5/1/2020 19:23:07'!
rightRotateBy: bits
	"Rotate my contents right by the given number of bits, retaining exactly 32 bits."
	"Details: Perform this operation with no LargeInteger arithmetic."
	| bitCount newLow |
	
	bitCount _ bits.
	bitCount >= 32 ifTrue: [ bitCount _ bitCount \\ 32 ].
	bitCount >= 16 ifTrue: [
		newLow _ hi.
		hi _ low.
		low _ newLow.
		bitCount _ bitCount - 16 ].

	bitCount >= 15 ifTrue: [
		newLow _ ((low bitAnd: 16r8000) bitShift: -15) bitOr: (hi << 1 bitAnd: 16rFFFF).
		hi _ ((hi bitAnd: 16r8000) bitShift: -15) bitOr: (low << 1 bitAnd: 16rFFFF).
		low _ newLow.
		^self ].
	
	bitCount >= 1 ifTrue: [
		| shift |
		shift _ 16 - bitCount .
		newLow _ ((hi bitShift: shift) bitAnd: 16rFFFF) bitOr: low >> bitCount.
		hi _ ((low bitShift: shift) bitAnd: 16rFFFF) bitOr: hi >> bitCount.
		low _ newLow ]! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
hi

	^ hi
! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'nice 8/28/2010 22:34'!
load: anInteger
	"Set my contents to the value of given integer."

	(anInteger positive and: [anInteger digitLength <= 4])
		ifFalse: [self error: 'out of range: ', anInteger printString].
	low := anInteger bitAnd: 16rFFFF.
	hi := (anInteger bitShift: -16) bitAnd: 16rFFFF
! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'ul 4/11/2015 04:01'!
loadFrom: aThirtyTwoBitRegister
	"Set my contents from the given ThirtyTwoBitRegister."

	hi := aThirtyTwoBitRegister hi.
	low := aThirtyTwoBitRegister low
! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:07'!
loadFrom: aByteArray at: index
	"Load my 32-bit value from the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."

	hi := ((aByteArray at: index) bitShift: 8) + ( aByteArray at: index + 1).
	low := ((aByteArray at: index + 2) bitShift: 8) + ( aByteArray at: index + 3).
! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
low

	^ low! !

!ThirtyTwoBitRegister methodsFor: 'copying' stamp: 'jm 12/7/1999 15:26'!
copy
	"Use the clone primitive for speed."

	<primitive: 148>
	^ super copy
! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/11/2015 04:50'!
< aThirtyTwoBitRegister

	^hi < aThirtyTwoBitRegister hi or: [
		hi = aThirtyTwoBitRegister hi and: [
			low < aThirtyTwoBitRegister low ] ]! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/11/2015 04:51'!
<= aThirtyTwoBitRegister

	^hi < aThirtyTwoBitRegister hi or: [
		hi = aThirtyTwoBitRegister hi and: [
			low <= aThirtyTwoBitRegister low ] ]! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/6/2015 02:30'!
= anObject

	^self class == anObject class
		and: [ anObject low = low
		and: [ anObject hi = hi ] ]! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/11/2015 04:51'!
> aThirtyTwoBitRegister

	^hi > aThirtyTwoBitRegister hi or: [
		hi = aThirtyTwoBitRegister hi and: [
			low > aThirtyTwoBitRegister low ] ]! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/11/2015 04:51'!
>= aThirtyTwoBitRegister

	^hi > aThirtyTwoBitRegister hi or: [
		hi = aThirtyTwoBitRegister hi and: [
			low >= aThirtyTwoBitRegister low ] ]! !

!ThirtyTwoBitRegister methodsFor: 'comparing' stamp: 'ul 4/6/2015 03:06'!
hash

	^((hi bitShift: 14) bitXor: low) hashMultiply! !

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'jm 12/14/1999 16:03'!
asInteger
	"Answer the integer value of my current contents."

	^ (hi bitShift: 16) + low
! !

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'ul 4/10/2015 20:52'!
asSignedInteger
	"Answer the signed integer value of my current contents."

	hi >= 16r8000 ifFalse: [ ^(hi bitShift: 16) + low ].
	^-1 - (low bitXor: 16rFFFF) - ((hi bitXor: 16rFFFF) bitShift: 16)


! !

!ThirtyTwoBitRegister class methodsFor: 'instance creation' stamp: 'nice 8/28/2010 22:42'!
fromByteArray: aByteArray at: startIndex
	"Answer a new instance whose initial contents is copied from next four bytes from aByteArray starting at startIndex..
	Convention is Most Significant Byte first (aka big endian)."

	^ self basicNew loadFrom: aByteArray at: startIndex
! !

!ThirtyTwoBitRegister class methodsFor: 'instance creation' stamp: 'nice 8/28/2010 22:38'!
fromInteger: aPositiveInteger
	"Answer a new instance whose initial contents is copied from aPositiveInteger.
	It is required that aPositiveInteger has no more than 32 bits."

	^ self basicNew load: aPositiveInteger
! !

!ThirtyTwoBitRegister class methodsFor: 'instance creation' stamp: 'jm 12/14/1999 16:05'!
new
	"Answer a new instance whose initial contents is zero."

	^ super new load: 0
! !
DigitalSignatureAlgorithm initialize!
SecureHashAlgorithm initialize!
SecureHashAlgorithm256 initialize!
