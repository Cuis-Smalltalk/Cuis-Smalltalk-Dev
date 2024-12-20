'From Cuis7.3 [latest update: #6938] on 20 December 2024 at 4:58:56 pm'!

!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 8/2/2016 16:38:32'!
valueStartingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each value ]
			on: UnhandledError
			do: [:exc | 
				self valueStartingFrom: index + 1.
				exc pass]].
	^ answer! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 12/20/2024 16:53:35'!
selector: aSelector
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	self 
		withPropertiesDo: [ :properties | properties selector: aSelector ] 
		ifSelector: [ :selector | | numberOfLiterals |
			(numberOfLiterals := self numLiterals) < 2 ifTrue: [self error: 'insufficient literals to hold selector'].
			self literalAt: numberOfLiterals - 1 put: aSelector]! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 12/20/2024 16:54:39'!
symbolicLinesDo: aBlock
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc |
	aStream := ReadWriteStream on: (String new: 64).
	self isQuick ifTrue:
		[self longPrintOn: aStream.
		 aBlock value: 0 value: aStream contents.
		 ^self].

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(InstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line firstLine index |
			line := stream contents allButLast.
			firstLine := line lines first.
			firstLine size < line size ifTrue: [
				line := firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]! !


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


!RunArray methodsFor: 'enumerating' stamp: 'jmv 12/20/2024 16:57:27'!
runsFrom: start to: stop do: aBlock
	"Evaluate aBlock with all existing runs in the range from start to stop"
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue: [ :firstRun :offset :firstValue|
		| run value index |
		run := firstRun.
		value := firstValue.
		index := start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run := run + 1.
			value := values at: run.
			index := index + (runs at: run)]].
! !


!Set class methodsFor: 'initialization' stamp: 'jmv 12/20/2024 16:58:24'!
quickRehashAllSets  "Set rehashAllSets"
	self withAllSubclassesDo:
		[:c |
			| insts |
			insts := c allInstances.
			(insts isEmpty or: [c = MethodDictionary]) ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor mousePoint
				from: 1 to: insts size
				during: [ :barBlock | 1 to: insts size do: [:x | barBlock value: x. (insts at: x) rehash]]]]! !

!Set class methodsFor: 'initialization' stamp: 'jmv 12/20/2024 16:58:13'!
rehashAllSets  "Set rehashAllSets"
	self withAllSubclassesDo:
		[:c |
			| insts |
			insts := c allInstances.
			insts isEmpty ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor mousePoint
				from: 1 to: insts size
				during: [ :barBlock |
				1 to: insts size do:
					[ :x | barBlock value: x.
					(insts at: x) rehash]]]]! !

