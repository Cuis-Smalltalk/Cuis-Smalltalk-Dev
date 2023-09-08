'From Cuis 6.0 [latest update: #5997] on 8 September 2023 at 5:38:45 pm'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 9/8/2023 17:26:37'!
displaceInstVarAccessesIn: aCompiledMethodOrMethodSpec starting: firstIvarToDisplace by: delta doIt: aBoolean
	"Remap instance variable accesses according to parameters.
	Affect instance variables access starting with ivar number firstIvar. Displace them by delta.
	Similar to #remapInstVarAccessesIn:asPer:doIt:
	Instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBollean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
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
	after all of them. See senders.
	"

	| pc end bytecode followingByte thirdByte oldIvar bytecodeSize |
	"Just in case..."
	delta = 0 ifTrue: [ ^true ].

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		aBoolean ifTrue: [
			oldIvar := aCompiledMethodOrMethodSpec returnField+1.
			oldIvar >= firstIvarToDisplace ifTrue: [
				| primitiveCode |
				primitiveCode := oldIvar+delta-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true 	].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode ].
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

		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 						"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		pc := pc + bytecodeSize ].

	^true! !

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 9/8/2023 17:26:55'!
remapInstVarAccessesIn: aCompiledMethodOrMethodSpec asPer: aDictionary doIt: aBoolean
	"Remap all instance variable accesses according to aDictionary.
	aDictionary is of the form: {
		(oldIvarNumber -> newIvarNumber).
		...
		(oldIvarNumber -> newIvarNumber) }
	where instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBollean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Note: If calling on a CompiledMethod, you must do
		Smalltalk voidCogVMState
	when done. But if you're processing a bunch of methods, do it after you're done with all of them.
	Note: Assumes the V3PlusClosures bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVarAllowForIvarsRemapping:
		#genStoreInstVarAllowForIvarsRemapping:
		#genStorePopInstVarAllowForIvarsRemapping:
		#returnField and #primitive"

	| pc end bytecode followingByte thirdByte oldIvar bytecodeSize |
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
		^true 	].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode ].
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

		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 						"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		pc := pc + bytecodeSize ].

	^true! !

