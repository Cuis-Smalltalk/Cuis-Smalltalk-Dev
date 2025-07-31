'From Cuis7.5 [latest update: #7391] on 31 July 2025 at 12:21:49 pm'!

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/31/2025 11:57:18'!
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
		
		"Copied from EncoderForSistaV1 class >> bytecodeSize:"
		bytecodeSize := 1.
		bytecode >= 224 ifTrue: [ bytecodeSize := 2 ].
		bytecode >= 248 ifTrue: [ bytecodeSize := 3 ].
		
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
							bytecodeSize := 2.
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
							bytecodeSize := 2.
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
						bytecodeSize := 1.	"Hasn't any effect, but hopefully makes more sense to reader."
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ].
					"Shorten to 1 byte format + NOP #genStorePopInstVar:"
					(bytecode = 240 and: [ newIvar <= 8 ]) ifTrue: [ 
						bytecodeSize := 1.	"Hasn't any effect, but hopefully makes more sense to reader."
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ]]]].

		"#genUnsignedSingleExtendA:"
		oldExtendA := nil.
		bytecode = 224 ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldExtendA := aCompiledMethodOrMethodSpec at: pc+1 ].

		pc := pc + bytecodeSize ].

	^true! !

!EncoderForSistaV1 class methodsFor: 'services' stamp: 'jmv 7/31/2025 11:56:39'!
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
		
		"Copied from EncoderForSistaV1 class >> bytecodeSize:"
		bytecodeSize := 1.
		bytecode >= 224 ifTrue: [ bytecodeSize := 2 ].
		bytecode >= 248 ifTrue: [ bytecodeSize := 3 ].
		
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
							bytecodeSize := 2.
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
							bytecodeSize := 2.
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
						bytecodeSize := 1.	"Hasn't any effect, but hopefully makes more sense to reader."
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ].
					"Shorten to 1 byte format + NOP #genStorePopInstVar:"
					(bytecode = 240 and: [ newIvar <= 8 ]) ifTrue: [
						bytecodeSize := 1.	"Hasn't any effect, but hopefully makes more sense to reader."
						aCompiledMethodOrMethodSpec at: pc put: newIvar-1+200.
						aCompiledMethodOrMethodSpec at: pc+1 put: 16r5F ]]]].

		"#genUnsignedSingleExtendA:"
		oldExtendA := nil.
		bytecode = 224 ifTrue: [
			pc+1 <= end ifFalse: [
				aBoolean ifTrue: [self error: 'Bad Method format'].
				^false].
			oldExtendA := aCompiledMethodOrMethodSpec at: pc+1 ].

		pc := pc + bytecodeSize ].

	^true! !


!EncoderForV3PlusClosures class methodsFor: 'services' stamp: 'jmv 7/31/2025 11:52:12'!
displaceInstVarAccessesIn: aCompiledMethodOrMethodSpec starting: firstIvarToDisplace by: delta doIt: aBoolean
	"Remap instance variable accesses according to parameters.
	Affect instance variables access starting with ivar number firstIvarToDisplace. Displace them by delta.
	Instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Similar to #remapInstVarAccessesIn:asPer:doIt:
	Note: Assumes the V3PlusClosures bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVarAllowForIvarsRemapping:
		#genStoreInstVarAllowForIvarsRemapping:
		#genStorePopInstVarAllowForIvarsRemapping:
		#returnField and #primitive

	Warning: After calling on a CompiledMethod, you must do either
		aBoolean ifTrue: [aCompiledMethodOrMethodSpec voidCogVMState].
	or
		aBoolean ifTrue: [Smalltalk voidCogVMState].
	This is not done in this method, because when processing many, it is best to do
		Smalltalk voidCogVMState
	after all of them. See senders."
	
	"This method can not handle remapping of 1-byte ivar access bytecodes: [0..15], [96..103]
	This method can not convert two byte bytecodes #[128 129 130] to or from three byte 132 + #[64 160 192].
	However the Sista Bytecode version of this method can.
	It is therefore recommended to use the Sista Bytecode Set."

	| pc end bytecode followingByte thirdByte oldIvar newIvar bytecodeSize |
	"Just in case..."
	delta = 0 ifTrue: [ ^true ].
	
	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ]	.

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		aBoolean ifTrue: [
			oldIvar := aCompiledMethodOrMethodSpec returnField+1.
			oldIvar >= firstIvarToDisplace ifTrue: [
				| primitiveCode |
				newIvar := oldIvar+delta.
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 							"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode + 1 ].
		(bytecode between: 96 and: 103) ifTrue: [ oldIvar := (bytecode bitAnd: 2r111) + 1 ].
		oldIvar >= firstIvarToDisplace ifTrue: [
			aBoolean ifTrue: [self error: 'Can not handle short instance variable access bytecodes'].
			^false ]	.
		
		"Two byte instance variable access bytecodes"
		followingByte := pc+1 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 ].
		((#[128 129 130] includes: bytecode) and: [followingByte < 64]) ifTrue: [
			followingByte+1 >= firstIvarToDisplace ifTrue: [
				followingByte+delta < 64 ifFalse: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 3 byte'].
					^false].
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 put: followingByte+delta ]]].

		"Three byte instance variable access bytecodes"
		(bytecode = 132 and: [#[64 160 192] includes: followingByte]) ifTrue: [
			thirdByte := pc+2 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 ].
			thirdByte+1 >= firstIvarToDisplace ifTrue: [
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 put: thirdByte+delta ]]].

		pc := pc + bytecodeSize ].

	^true! !

!EncoderForV3PlusClosures class methodsFor: 'services' stamp: 'jmv 7/31/2025 11:52:49'!
remapInstVarAccessesIn: aCompiledMethodOrMethodSpec asPer: aDictionary doIt: aBoolean
	"Remap all instance variable accesses according to aDictionary.
	aDictionary is of the form: {
		(oldIvarNumber -> newIvarNumber).
		...
		(oldIvarNumber -> newIvarNumber) }
	where instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Note: If calling on a CompiledMethod, you must do
		Smalltalk voidCogVMState
	when done. But if you're processing a bunch of methods, do it after you're done with all of them.
	Similar to #displaceInstVarAccessesIn:starting:by:doIt:
	Note: Assumes the V3PlusClosures bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVarAllowForIvarsRemapping:
		#genStoreInstVarAllowForIvarsRemapping:
		#genStorePopInstVarAllowForIvarsRemapping:
		#returnField and #primitive"
	
	"This method can not handle remapping of 1-byte ivar access bytecodes: [0..15], [96..103]
	This method can not convert two byte bytecodes #[128 129 130] to or from three byte 132 + #[64 160 192].
	However the Sista Bytecode version of this method can.
	It is therefore recommended to use the Sista Bytecode Set."

	| pc end bytecode followingByte thirdByte oldIvar bytecodeSize |
	
	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ]	.

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		aBoolean ifTrue: [
			aDictionary at: aCompiledMethodOrMethodSpec returnField+1 ifPresent: [ :newIvar |
				| primitiveCode |
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 							"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode + 1 ].
		(bytecode between: 96 and: 103) ifTrue: [ oldIvar := (bytecode bitAnd: 2r111) + 1 ].
		aDictionary at: oldIvar ifPresent: [ :newIvar |
			newIvar = oldIvar ifFalse: [
				aBoolean ifTrue: [self error: 'Can not handle short instance variable access bytecodes'].
				^false ]	].
		
		"Two byte instance variable access bytecodes"
		followingByte := pc+1 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 ].
		((#[128 129 130] includes: bytecode) and: [followingByte < 64]) ifTrue: [
			aDictionary at: followingByte+1 ifPresent: [ :newIvar |
				newIvar <= 64 ifFalse: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 3 byte'].
					^false].
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 ]]].

		"Three byte instance variable access bytecodes"
		(bytecode = 132 and: [#[64 160 192] includes: followingByte]) ifTrue: [
			thirdByte := pc+2 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 ].
			aDictionary at: thirdByte+1 ifPresent: [ :newIvar |
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 put: newIvar-1 ]]].

		pc := pc + bytecodeSize ].

	^true! !

