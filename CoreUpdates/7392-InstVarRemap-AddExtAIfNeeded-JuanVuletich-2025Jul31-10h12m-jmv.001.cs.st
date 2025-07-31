'From Cuis7.5 [latest update: #7390] on 31 July 2025 at 10:16:54 am'!

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/30/2025 16:14:42'!
withLongInstVarAccessBytecodes: aCompiledMethodOrMethodSpec
	"Answer a CompiledMethod or MethodSpec that is functionally equivalent to the argument.
	But:
		- All instance variable accessing code are 'padded' to 4 bytes (see below).
		- All jump / branch bytecodes are 'padded' to 4 bytes (see below) AND corrected to jump to the correct place.

	This is to enable #remapInstVarAccessesIn:asPer:doIt: and #displaceInstVarAccessesIn:starting:by:doIt:
	to work on methods using short bytecodes that prevent the modification of the involved instance variable
	instance variable indexes in place.

	In the answer:
	- Short (1 byte) instance variable access bytecodes [0..15],[200..207] are kept unmodified, but followed by a NOP bytecode
	- Short (1 byte) jump / branch bytecodes [176..199] are converted to the the corresponding two byte format: #[237 238 239]
	- All the ivar access, jump and branch bytecodes are prepended with a (likely zero) extA or extB extension bytecode
		This includes  [0..15],[200..207], #[226 240 243], [176..199], #[237 238 239].
		Any extA or extB previously present is kept unmodified and not duplicated.
	- The result is larger than the argument #bytecodes. All jump / branch bytecodes are corrected to jump to the correct
		spot in the new, longer bytecodes sequence.

	Note 1: This is somewhat expensive and not especially optimized. It is expected to be needed in a very small percentage of
		methods: Only those that remap instance variables in the set of the first 256 ones to/from variables beyond index 255.
	Note 2: The result is not space optimal. In many cases the extA / extB will not actually be needed. Computing the optimum
		bytecode sequence is even more expensive. Besides this usually won't give a performance improvement, as both
		forms would be jitted to the same machine code by Cog."

	| bytecodesAndPadInfo newBytecodes |

	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		"No conversion possible."
		^nil ].

	"Quick return. Convert into a regular method."
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		| bytes |
		bytes := #[224 0 226 0 92] copy. "#genUnsignedSingleExtendA:, #genPushInstVarLong:, #genReturnTop"
		bytes at: 4 put: aCompiledMethodOrMethodSpec returnField.
		^aCompiledMethodOrMethodSpec copyWithBytecodes: bytes ].

	bytecodesAndPadInfo := aCompiledMethodOrMethodSpec bytecodes asArray.
	self withLongInstVarAccessConversion01On: bytecodesAndPadInfo.
	self withLongInstVarAccessConversion02On: bytecodesAndPadInfo.
	newBytecodes := self withLongInstVarAccessConversion03On: bytecodesAndPadInfo.
	^aCompiledMethodOrMethodSpec copyWithBytecodes: newBytecodes! !

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/29/2025 16:00:34'!
withLongInstVarAccessConversion01On: bytecodesAndPadInfo
	"Extract ivar accesses and jumps into aux arrays"

	| pc end hadExtendA oldExtendBByte bytecode bytecodeSize oldSize newBytes newExtendB oldDistance sizeDelta |
	
	hadExtendA := false.
	oldExtendBByte := nil.
	pc := 1.
	end := bytecodesAndPadInfo size.
	[pc <= end] whileTrue: [
		bytecode := bytecodesAndPadInfo at: pc.
		
		"#genPushInstVar: #genStorePopInstVar: Short 1 byte format."
		"Add a trailing NOP if not already there. Prepend with a zero extA if not already there."
		(bytecode < 16 or: [(bytecode between: 200 and: 207)]) ifTrue: [
			oldSize := (pc+1 <= end and: [bytecodesAndPadInfo at: pc+1]) = 16r5F
				ifTrue: [2] "Already had a trailing NOP"
				ifFalse: [1]. "Didn't have it."
			newBytes := {bytecode. 16r5F}. "Will indeed have a trailing NOP"
			hadExtendA ifFalse: [
				newBytes := #[224 0], newBytes]. "Will add a zero extA if it didn't have one already"
			bytecodesAndPadInfo at: pc put: { #ivarAccess. oldSize. newBytes size-oldSize. newBytes} ].
		
		"#genPushInstVar: #genStorePopInstVar: #genStoreInstVar: Regular 2 byte format."
		"Prepend with a zero extA if not already there."
		(#[226 240 243] includes: bytecode) ifTrue: [
			pc+1 <= end ifFalse: [
				^false].
			newBytes := {bytecode. bytecodesAndPadInfo at: pc+1}.
			hadExtendA ifFalse: [
				newBytes := #[224 0], newBytes]. "Add a zero extA before our bytecode if there wasn't one already there."
			bytecodesAndPadInfo at: pc put: ( { #ivarAccess. 2. newBytes size-2. newBytes}) ].
		
		"#genJump:, #genBranchPopTrue:, #genBranchPopFalse:, short 1 byte format. Also handle possible extB (four byte format)"
		"Convert to 2 byte format. Prepend with a zero extB if not already present (there shouldn't be one)."
		(bytecode between: 176 and: 199) ifTrue: [
			newExtendB := oldExtendBByte ifNil: [{225. 0}] ifNotNil: [nil]. 	"Add extB if not already there. Second byte for jump distance to be set later."
			newBytes := {bytecode >> 3 + 215. 0}. 						"Compute two byte bytecode (237, 238, 239). Second byte to be set later."
			sizeDelta := (oldExtendBByte ifNil: [2] ifNotNil: [0]) + 2-1. 		"extB size + newBytes size - oldSize"
			oldDistance := (bytecode bitAnd: 7) + 1.						"Extract jump distance from 1-byte bytecode"
			"Ignore very unlikely oldExtendB, for oldDistance. Assume zero. (It doesn't make sense to actually use it for 3 bit jumps!!)"
			bytecodesAndPadInfo at: pc put: ( {#branch. 1. sizeDelta. oldDistance. newExtendB. newBytes }) ].
		
		"#genJump:, #genBranchPopTrue:, #genBranchPopFalse:, Two byte format. Also handle possible extB (four byte format)"
		(#[237 238 239] includes: bytecode) ifTrue: [
			pc+1 <= end ifFalse: [
				^false].
			newExtendB := oldExtendBByte ifNil: [{225. 0}] ifNotNil: [nil]. 	"Add extB if not already there. Second byte for jump distance to be set later."
			newBytes := {bytecode. 0}. 									"Second byte for jump distance will be set later."
			sizeDelta := (oldExtendBByte ifNil: [2] ifNotNil: [0]) + 2-2. 		"extB size + newBytes size - oldSize"
			"Compute oldDistance. Include possible extB byte."
			oldDistance := bytecodesAndPadInfo at: pc+1.
			oldExtendBByte notNil ifTrue: [
				(bytecode = 237 and: [oldExtendBByte > 127]) ifTrue: [oldExtendBByte := oldExtendBByte-256].
				oldDistance := oldExtendBByte * 256 + oldDistance ].
			bytecodesAndPadInfo at: pc put: ( {#branch. 2. sizeDelta. oldDistance. newExtendB. newBytes }) ].
		
		"#genUnsignedSingleExtendA: and genUnsignedSingleExtendB: Will be taken into account for next bytecode."
		hadExtendA := bytecode = 224.
		oldExtendBByte := nil.
		(bytecode = 225 and: [pc+1 <= end]) ifTrue: [
			oldExtendBByte := bytecodesAndPadInfo at: pc+1 ].
		
		"Copied from EncoderForSistaV1 class >> bytecodeSize:"
		bytecodeSize := 1.
		bytecode >= 224 ifTrue: [ bytecodeSize := 2 ].
		bytecode >= 248 ifTrue: [ bytecodeSize := 3 ].
		pc := pc + bytecodeSize ].

	^true! !

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/29/2025 16:03:20'!
withLongInstVarAccessConversion02On: bytecodesAndPadInfo
	"Correct #branch (branch and jump bytecodes) with correct jump distance."

	| newDistance oldDistance |
	1 to: bytecodesAndPadInfo size do: [ :i | | entry |
		entry := bytecodesAndPadInfo at: i.
		(entry isArray and: [entry first = #branch]) ifTrue: [ | newDistanceHigh newDistanceLow |
			oldDistance := entry fourth.
			newDistance := oldDistance.
			i+entry second to: i+entry second+oldDistance-oldDistance sign by: oldDistance sign do: [ :j | | eachJumpedOver |
				eachJumpedOver := bytecodesAndPadInfo at: j.
				eachJumpedOver isArray ifTrue: [ newDistance := newDistance + eachJumpedOver third ]].
			"See #genUnsignedSingleExtendB: and #genSignedSingleExtendB:"
			newDistanceHigh := newDistance >> 8. newDistanceHigh < 0 ifTrue: [newDistanceHigh := newDistanceHigh + 256].
			newDistanceLow := newDistance bitAnd: 255.
			entry fifth "High byte goes to existing extB if there was one. Or to a new one otherwise."
				ifNil: [ bytecodesAndPadInfo at: i-1 put: newDistanceHigh ]
				ifNotNil: [ :extB | extB at: 2 put: newDistanceHigh ].
			entry sixth at: 2 put: newDistanceLow ]].! !

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/29/2025 16:03:48'!
withLongInstVarAccessConversion03On: bytecodesAndPadInfo
	"Write the updated bytecode sequence using the already calculated bytecodes."
	
	^ByteArray streamContents: [ :strm | | i |
		i := 1.
		[ i <= bytecodesAndPadInfo size ] whileTrue: [ | entry |
			entry := bytecodesAndPadInfo at: i.
			entry isArray
				ifTrue: [
					entry first == #ivarAccess ifTrue: [
						strm nextPutAll: entry fourth ].
					entry first == #branch ifTrue: [
						entry fifth ifNotNil: [ :extB | strm nextPutAll: extB ].
						strm nextPutAll: entry sixth ].
					i := i + entry second ]
				ifFalse: [
					strm nextPut: entry.
					i := i + 1 ]]].! !


!EncoderForV3PlusClosures class methodsFor: 'services' stamp: 'jmv 7/30/2025 15:24:12'!
withLongInstVarAccessBytecodes: aCompiledMethodOrMethodSpec
	"This method is provided for compatibility with EncoderForSistaV1.
	However, the desired result can not be produced for the V3 Bytecode Set.
	The reason is the need for a space padding NOP bytecode, missing in V3.
	Answer nil: no conversion possible."

	^nil! !


!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/30/2025 16:36:30'!
displaceInstVarAccessesIn: aCompiledMethodOrMethodSpec starting: firstIvarToDisplace by: delta doIt: aBoolean
	"Remap instance variable accesses according to parameters.
	Affect instance variables access starting with ivar number firstIvarToDisplace. Displace them by delta.
	Instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Similar to #remapInstVarAccessesIn:asPer:doIt:
	Note: Assumes the Sista V1 bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVar:
		#genStoreInstVar:
		#genStorePopInstVar:
		#returnField and #primitive

	Warning: After calling on a CompiledMethod, you must do either
		aBoolean ifTrue: [aCompiledMethodOrMethodSpec voidCogVMState].
	or
		aBoolean ifTrue: [Smalltalk voidCogVMState].
	This is not done in this method, because when processing many, it is best to do
		Smalltalk voidCogVMState
	after all of them. See senders."
	
	"To handle inst var indexes > 255 the following is necessary:
		- Two byte ivar access opcodes, #[226 240 243], need to be prepended by a (possibly zero) extendA bytecode
		- 1 byte ivar access opcodes, [0..15],[200..207], need to also be followed by NOT bytecode 0x5F
	These conditions can be fullfilled by calling #withLongInstVarAccessBytecodes: . See senders and implementors."

	| pc end oldExtendA bytecode oldIvar newIvar bytecodeSize |

	"Just in case..."
	delta = 0 ifTrue: [ ^true ].
	
	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ].

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		oldIvar := aCompiledMethodOrMethodSpec returnField+1.
		oldIvar >= firstIvarToDisplace ifTrue: [
			| primitiveCode |
			newIvar := oldIvar+delta.
			newIvar > 256 ifTrue: [
				aBoolean ifTrue: [self error: 'Can not convert a quick return instance variable into the long form.'].
				^false].
			aBoolean ifTrue: [
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	oldExtendA := nil.
	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"#genPushInstVar: Short 1 byte format."
		bytecode < 16 ifTrue: [
			oldIvar := bytecode+1.
			oldIvar >= firstIvarToDisplace ifTrue: [
				newIvar := oldIvar+delta.
				newIvar <= 16
					ifTrue: [ "Keep it short."
						aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc put: newIvar-1 ]]
					ifFalse: [ "Make it two byte format"
						(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
							aBoolean ifTrue: [self error: 'Can not convert short instance variable access bytecodes to 4 byte'].
							^false].
						(pc+1 <= end and: [
							aCompiledMethodOrMethodSpec at: pc+1]) = 16r5F ifFalse: [
								aBoolean ifTrue: [self error: 'NOP padding missing'].
								^false ].
						aBoolean ifTrue: [
							aCompiledMethodOrMethodSpec at: pc put: 226.
							aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
							oldExtendA notNil ifTrue: [
								aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256  ]]]]].
		
		"#genStorePopInstVar: Short 1 byte format."
		(bytecode between: 200 and: 207) ifTrue: [
			oldIvar := bytecode-200+1.
			oldIvar >= firstIvarToDisplace ifTrue: [
				newIvar := oldIvar+delta.
				newIvar <= 8
					ifTrue: [ "Keep it short."
						aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200 ]]
					ifFalse: [ "Make it two byte format"
						(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
							aBoolean ifTrue: [self error: 'Can not convert short instance variable access bytecodes to 4 byte'].
							^false].
						(pc+1 <= end and: [
							aCompiledMethodOrMethodSpec at: pc+1]) = 16r5F ifFalse: [
								aBoolean ifTrue: [self error: 'NOP padding missing'].
								^false ].
						aBoolean ifTrue: [
							aCompiledMethodOrMethodSpec at: pc put: 240.
							aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
							oldExtendA notNil ifTrue: [
								aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256 ]]]]].
		
		"#genPushInstVar: #genStorePopInstVar: #genStoreInstVar: Two byte format. Also handle extA (four byte format)"
		(#[226 240 243] includes: bytecode) ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldIvar := (aCompiledMethodOrMethodSpec at: pc+1) + 1.
			oldExtendA ifNotNil: [ oldIvar := oldExtendA * 256 + oldIvar ].
			oldIvar >= firstIvarToDisplace ifTrue: [
				newIvar := oldIvar+delta.
				(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 4 byte'].
					^false].
				"Update two byte format with possible oldExtendA"
				aBoolean ifTrue: [
					aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
					oldExtendA notNil ifTrue: [
						aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256 ].
					"Shorten to 1 byte format + NOP #genPushInstVar:"
					(bytecode = 226 and: [ newIvar <= 16 ]) ifTrue: [
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ].
					"Shorten to 1 byte format + NOP #genStorePopInstVar:"
					(bytecode = 240 and: [ newIvar <= 8 ]) ifTrue: [ 
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ]]]].

		"#genUnsignedSingleExtendA:"
		oldExtendA := nil.
		bytecode = 224 ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldExtendA := aCompiledMethodOrMethodSpec at: pc+1 ].

		"Copied from EncoderForSistaV1 class >> bytecodeSize:"
		bytecodeSize := 1.
		bytecode >= 224 ifTrue: [ bytecodeSize := 2 ].
		bytecode >= 248 ifTrue: [ bytecodeSize := 3 ].
		pc := pc + bytecodeSize ].

	^true! !

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/30/2025 16:36:42'!
remapInstVarAccessesIn: aCompiledMethodOrMethodSpec asPer: aDictionary doIt: aBoolean
	"Remap all instance variable accesses according to aDictionary.
	aDictionary is of the form: {
		(oldIvarNumber -> newIvarNumber).
		...
		(oldIvarNumber -> newIvarNumber) }
	where instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Similar to #displaceInstVarAccessesIn:starting:by:doIt:
	Note: Assumes the Sista V1 bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVar:
		#genStoreInstVar:
		#genStorePopInstVar:
		#returnField and #primitive

	Warning: After calling on a CompiledMethod, you must do either
		aBoolean ifTrue: [aCompiledMethodOrMethodSpec voidCogVMState].
	or
		aBoolean ifTrue: [Smalltalk voidCogVMState].
	This is not done in this method, because when processing many, it is best to do
		Smalltalk voidCogVMState
	after all of them. See senders."
	
	"To handle inst var indexes > 255 the following is necessary:
		- Two byte ivar access opcodes, #[226 240 243], need to be prepended by a (possibly zero) extendA bytecode
		- 1 byte ivar access opcodes, [0..15],[200..207], need to also be followed by NOT bytecode 0x5F
	These conditions can be fullfilled by calling #withLongInstVarAccessBytecodes: . See senders and implementors."

	| pc end oldExtendA bytecode oldIvar bytecodeSize |

	"Just in case..."
	aDictionary isEmpty ifTrue: [ ^true ].

	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ].

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		oldIvar := aCompiledMethodOrMethodSpec returnField+1.
		aDictionary at: oldIvar ifPresent: [ :newIvar |
			| primitiveCode |
			newIvar > 256 ifTrue: [
				aBoolean ifTrue: [self error: 'Can not convert a quick return instance variable into the long form.'].
				^false].
			aBoolean ifTrue: [
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	oldExtendA := nil.
	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"#genPushInstVar: Short 1 byte format."
		bytecode < 16 ifTrue: [
			oldIvar := bytecode+1.
			aDictionary at: oldIvar ifPresent: [ :newIvar |
				newIvar <= 16
					ifTrue: [ "Keep it short."
						aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc put: newIvar-1 ]]
					ifFalse: [ "Make it two byte format"
						(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
							aBoolean ifTrue: [self error: 'Can not convert short instance variable access bytecodes to 4 byte'].
							^false].
						(pc+1 <= end and: [
							aCompiledMethodOrMethodSpec at: pc+1]) = 16r5F ifFalse: [
								aBoolean ifTrue: [self error: 'NOP padding missing'].
								^false ].
						aBoolean ifTrue: [
							aCompiledMethodOrMethodSpec at: pc put: 226.
							aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
							oldExtendA notNil ifTrue: [
								aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256 ]]]]].
		
		"#genStorePopInstVar: Short 1 byte format."
		(bytecode between: 200 and: 207) ifTrue: [
			oldIvar := bytecode-200+1.
			aDictionary at: oldIvar ifPresent: [ :newIvar |
				newIvar <= 8
					ifTrue: [ "Keep it short."
						aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200 ]]
					ifFalse: [ "Make it two byte format"
						(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
							aBoolean ifTrue: [self error: 'Can not convert short instance variable access bytecodes to 4 byte'].
							^false].
						(pc+1 <= end and: [
							aCompiledMethodOrMethodSpec at: pc+1]) = 16r5F ifFalse: [
								aBoolean ifTrue: [self error: 'NOP padding missing'].
								^false ].
						aBoolean ifTrue: [
							aCompiledMethodOrMethodSpec at: pc put: 240.
							aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
							oldExtendA notNil ifTrue: [
								aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256 ]]]]].
		
		"#genPushInstVar: #genStorePopInstVar: #genStoreInstVar: Two byte format. Also handle extA (four byte format)"
		(#[226 240 243] includes: bytecode) ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldIvar := (aCompiledMethodOrMethodSpec at: pc+1) + 1.
			oldExtendA ifNotNil: [ oldIvar := oldExtendA * 256 + oldIvar ].
			aDictionary at: oldIvar ifPresent: [ :newIvar |
				(newIvar > 256 and: [oldExtendA isNil]) ifTrue: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 4 byte'].
					^false].
				"Update two byte format with possible oldExtendA"
				aBoolean ifTrue: [
					aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 \\ 256.
					oldExtendA notNil ifTrue: [
						aCompiledMethodOrMethodSpec at: pc-1 put: newIvar-1 // 256 ].
					"Shorten to 1 byte format + NOP #genPushInstVar:"
					(bytecode = 226 and: [ newIvar <= 16 ]) ifTrue: [
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ].
					"Shorten to 1 byte format + NOP #genStorePopInstVar:"
					(bytecode = 240 and: [ newIvar <= 8 ]) ifTrue: [
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ]]]].

		"#genUnsignedSingleExtendA:"
		oldExtendA := nil.
		bytecode = 224 ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldExtendA := aCompiledMethodOrMethodSpec at: pc+1 ].

		"Copied from EncoderForSistaV1 class >> bytecodeSize:"
		bytecodeSize := 1.
		bytecode >= 224 ifTrue: [ bytecodeSize := 2 ].
		bytecode >= 248 ifTrue: [ bytecodeSize := 3 ].
		pc := pc + bytecodeSize ].

	^true! !

