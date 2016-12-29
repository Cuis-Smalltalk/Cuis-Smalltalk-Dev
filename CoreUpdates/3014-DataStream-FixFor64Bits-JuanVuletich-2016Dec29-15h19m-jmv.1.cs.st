'From Cuis 5.0 of 7 November 2016 [latest update: #3013] on 29 December 2016 at 3:36:31 pm'!

!CompiledMethod methodsFor: 'file in/out' stamp: 'jmv 12/29/2016 15:25:13'!
storeDataOn: aDataStream
	"Store myself on a DataStream.  I am a mixture of objects and raw data bytes.  Only use this for blocks.  Normal methodDictionaries should not be put out using ReferenceStreams.  Their fileOut should be attached to the beginning of the file."

	| byteLength lits |
	"No inst vars of the normal type"
	byteLength _ self basicSize.
	aDataStream
		beginInstance: self class
		size: byteLength.
	lits _ self numLiterals + 1.	"counting header"
	1 to: lits do:
		[:ii | aDataStream nextPut: (self objectAt: ii)].
	lits*Smalltalk wordSize+1 to: byteLength do:
		[:ii | aDataStream byteStream nextPut: (self basicAt: ii)].
			"write bytes straight through to the file"! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 12/29/2016 15:27:40'!
readMethod
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	className _ self next.
	newClass _ Smalltalk at: className asSymbol.

	xxHeader _ self next.
		"nArgs _ (xxHeader >> 24) bitAnd: 16rF."
		"nTemps _ (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit _ (xxHeader >> 17) bitAnd: 1."
	nLits _ (xxHeader >> 9) bitAnd: 16rFF.
		"primBits _ ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer _ instSize - (newClass instSize "0") - (nLits + 1 * Smalltalk wordSize).

	newMethod _ newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits _ newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*Smalltalk wordSize+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod! !

!DataStream methodsFor: 'other' stamp: 'jmv 12/29/2016 15:36:22'!
vacantRef
	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference
	 position' to identify a reference that's not yet filled in. This must be a
	 value that won't be used as an ordinary reference. Cf. outputReference: and
	 readReference. -- 
	 NOTE: We could use a different type ID for vacant-refs rather than writing
		object-references with a magic value. (The type ID and value are
		overwritten by ordinary object-references when weak refs are fullfilled.)"

	"In 32 bit Cuis it was:"
	"^ SmallInteger maxVal"

	"Use that very same value even if in 64 bit Cuis.
	This means that DataStreams are limited to 1GibiBytes in size."
	^16r3FFFFFFF! !

