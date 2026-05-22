Class {
	#name : 'ClosureCompilerTest',
	#superclass : 'TestCase',
	#classVars : [
		'CmpRR',
		'CogRTLOpcodes',
		'Jump',
		'MoveCqR',
		'Nop'
	],
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithCopiedAndAssignedTemps [
	| blk "0w" a "0w" b "0w" c "0w" t "0w" r1 "0w" r2 "0w" |
	a := 1. "1w"
	b := 2. "1w"
	c := 4. "1w"
	t := 0. "1w"
	blk "5w" := ["2" t  "3w" := t "3r" + a "3r" + b "3r" + c "3r" ] "4".
	r1 "5w" := blk "5r" value.
	b "5w" := -100.
	r2 "5w" := blk "5r" value.
	^r1 "5r" -> r2 "5r" -> t "5r"

	"a: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 b: main(read(),write(0,1,5)), block(read(3),write()) => remote; write follows contained read
	 blk: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 c: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 r1: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 r2: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 t: main(read(5),write(0,1)), block(read(3),write(3)) => remote; read follows contained write"


	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithCopiedAndPostClosedOverAssignedTemps [
	| blk a b c r1 r2 |
	a := 1.
	b := 2.
	c := 4.
	blk := [a + b + c].
	r1 := blk value.
	b := nil.
	r2 := blk value.
	r1 -> r2

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndPostClosedOverAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithCopiedTemps [
	| a b c r |
	a := 1.
	b := 2.
	c := 4.
	r := [a + b + c] value.
	b := nil.
	r

	"Parser new
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class"

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithOptimizedBlocks [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| b | b := 'notNil'. b].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocks)
		class: self class"
]

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithOptimizedBlocksA [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocksA)
		class: self class"
]

{ #category : 'code examples' }
ClosureCompilerTest class >> methodWithVariousTemps [
	| classes total totalLength |
	classes := self withAllSuperclasses.
	total := totalLength := 0.
	classes do: [:class| | className |
		className := class name.
		total := total + 1.
		totalLength := totalLength + className size].
	^total -> totalLength

	"Parser new
		parse: (self class sourceCodeAt: #methodWithVariousTemps)
		class: self class"
]

{ #category : 'source' }
ClosureCompilerTest >> closureCases [
	^#(
'| n |
n := 1.
^n + n'

'| i |
i := 0.
[i := i + 1.
 i <= 10] whileTrue.
^i'

'[:c :s| | mn |
mn := Compiler new
		compile: (c sourceCodeAt: s)
		in: c
		notifying: nil
		ifFail: [self halt].
mn generate: #(0 0 0 0).
{mn blockExtentsToTempsMap.
  mn encoder schematicTempNames}]
			value: ArrayLiteralTest
			value: #testSymbols'

'inject: thisValue into: binaryBlock
	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue'

'runBinaryConditionalJumps: assertPrintBar
	"CogIA32CompilerTests new runBinaryConditionalJumps: false"
	| mask reg1 reg2 reg3 |
	mask := 1 << self processor bitsInWord - 1.
	self concreteCompilerClass dataRegistersWithAccessorsDo:
		[:n :get :set|
		n = 0 ifTrue: [reg1 := get].
		n = 1 ifTrue: [reg2 := set].
		n = 2 ifTrue: [reg3 := set]].
	#(	(JumpAbove > unsigned)			(JumpBelowOrEqual <= unsigned)
		(JumpBelow < unsigned)			(JumpAboveOrEqual >= unsigned)
		(JumpGreater > signed)			(JumpLessOrEqual <= signed)
		(JumpLess < signed)				(JumpGreaterOrEqual >= signed)
		(JumpZero = signed)				(JumpNonZero ~= signed)) do:
		[:triple|
		[:opName :relation :signednessOrResult| | opcode jumpNotTaken jumpTaken nop memory bogus |
		self resetGen.
		opcode := CogRTLOpcodes classPool at: opName.
		self gen: CmpRR operand: 2 operand: 1.
		jumpTaken := self gen: opcode.
		self gen: MoveCqR operand: 0 operand: 0.
		jumpNotTaken := self gen: Jump.
		jumpTaken jmpTarget: (self gen: MoveCqR operand: 1 operand: 0).
		jumpNotTaken jmpTarget: (nop := self gen: Nop).
		memory := self generateInstructions.
		bogus := false.
		self pairs: (-2 to: 2)  do:
			[:a :b| | taken |
			self processor
				reset;
				perform: reg2 with: a signedIntToLong;
				perform: reg3 with: b signedIntToLong.
			[self processor singleStepIn: memory.
			 self processor pc ~= nop address] whileTrue.
			taken := (self processor perform: reg1) = 1.
			assertPrintBar
				ifTrue:
					[self assert: taken = (signednessOrResult == #unsigned
											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]
											ifFalse: [a perform: relation with: b])]
				ifFalse:
					[Transcript
						nextPutAll: reg2; nextPut: $(; print: a; nextPutAll: '') ''; nextPutAll: relation; space;
						nextPutAll: reg3; nextPut: $(; print: b; nextPutAll: '') = '';
						print: taken; cr; flush.
					 taken = (signednessOrResult == #unsigned
											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]
											ifFalse: [a perform: relation with: b]) ifFalse:
						[bogus := true]]].
			 bogus ifTrue:
				[self processor printRegistersOn: Transcript.
				 Transcript show: (self processor disassembleInstructionAt: jumpTaken address In: memory); cr]]
					valueWithArguments: triple]'

'mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys asSortedCollection
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector| | subMap tempVector numTemps |
		subMap := Dictionary new.
		"Find how many temp slots there are (direct & indirect temp vectors)
		 and for each indirect temp vector find how big it is."
		tempNameTupleVector do:
			[:tuple|
			tuple last isArray
				ifTrue:
					[subMap at: tuple last first put: tuple last last.
					 numTemps := tuple last first]
				ifFalse:
					[numTemps := tuple last]].
		"create the temp vector for this scope level."
		tempVector := Array new: numTemps.
		"fill it in with any indirect temp vectors"
		subMap keysAndValuesDo:
			[:index :size|
			tempVector at: index put: (Array new: size)].
		"fill it in with temp nodes."
		tempNameTupleVector do:
			[:tuple| | itv |
			tuple last isArray
				ifTrue:
					[itv := tempVector at: tuple last first.
					 itv at: tuple last last
						put: (aDecompilerConstructor
								codeTemp: tuple last last - 1
								named: tuple first)]
				ifFalse:
					[tempVector
						at: tuple last
						put: (aDecompilerConstructor
								codeTemp: tuple last - 1
								named: tuple first)]].
		"replace any indirect temp vectors with proper RemoteTempVectorNodes"
		subMap keysAndValuesDo:
			[:index :size|
			tempVector
				at: index
				put: (aDecompilerConstructor
						codeRemoteTemp: index
						remoteTemps: (tempVector at: index))].
		"and update the entry in the map"
		map at: startpc put: tempVector].
	^map'

 'gnuifyFrom: inFileStream to: outFileStream

"convert interp.c to use GNU features"

	| inData beforeInterpret inInterpret inInterpretVars beforePrimitiveResponse inPrimitiveResponse |

	inData := inFileStream upToEnd withSqueakLineEndings.
	inFileStream close.

	"print a header"
	outFileStream
		nextPutAll: ''/* This file has been post-processed for GNU C */'';
		cr; cr; cr.

	beforeInterpret := true.    "whether we are before the beginning of interpret()"
	inInterpret := false.     "whether we are in the middle of interpret"
	inInterpretVars := false.    "whether we are in the variables of interpret"
	beforePrimitiveResponse := true.  "whether we are before the beginning of primitiveResponse()"
	inPrimitiveResponse := false.   "whether we are inside of primitiveResponse"
	''Gnuifying''
		displayProgressAt: Sensor mousePoint
		from: 1 to: (inData occurrencesOf: Character cr)
		during:
			[:bar | | lineNumber |
			lineNumber := 0.
			inData linesDo:
				[ :inLine | | outLine extraOutLine caseLabel |
				bar value: (lineNumber := lineNumber + 1).
				outLine := inLine. 	"print out one line for each input line; by default, print out the line that was input, but some rules modify it"
				extraOutLine := nil.   "occasionally print a second output line..."
				beforeInterpret ifTrue: [
					inLine = ''#include "sq.h"'' ifTrue: [
						outLine := ''#include "sqGnu.h"'' ].
					inLine = ''interpret(void) {'' ifTrue: [
						"reached the beginning of interpret"
						beforeInterpret := false.
						inInterpret := true.
						inInterpretVars := true ] ]
				ifFalse: [
				inInterpretVars ifTrue: [
					(inLine findString: ''register struct foo * foo = &fum;'') > 0 ifTrue: [
						outLine := ''register struct foo * foo FOO_REG = &fum;'' ].
					(inLine findString: '' localIP;'') > 0 ifTrue: [
						outLine := ''    char* localIP IP_REG;'' ].
					(inLine findString: '' localFP;'') > 0 ifTrue: [
						outLine := ''    char* localFP FP_REG;'' ].
					(inLine findString: '' localSP;'') > 0 ifTrue: [
						outLine := ''    char* localSP SP_REG;'' ].
					(inLine findString: '' currentBytecode;'') > 0 ifTrue: [
						outLine := ''    sqInt currentBytecode CB_REG;'' ].
					inLine isEmpty ifTrue: [
						"reached end of variables"
						inInterpretVars := false.
						outLine := ''    JUMP_TABLE;''.
						extraOutLine := inLine ] ]
				ifFalse: [
				inInterpret ifTrue: [
					"working inside interpret(); translate the switch statement"
					(inLine beginsWith: ''		case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''		CASE('', caseLabel, '')'' ].
					inLine = ''			break;'' ifTrue: [
						outLine := ''			BREAK;'' ].
					inLine = ''}'' ifTrue: [
						"all finished with interpret()"
						inInterpret := false ] ]
				ifFalse: [
				beforePrimitiveResponse ifTrue: [
					(inLine beginsWith: ''primitiveResponse('') ifTrue: [
						"into primitiveResponse we go"
						beforePrimitiveResponse := false.
						inPrimitiveResponse := true.
						extraOutLine := ''    PRIM_TABLE;'' ] ]
				ifFalse: [
				inPrimitiveResponse ifTrue: [
					inLine = ''	switch (primitiveIndex) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					inLine = ''	switch (GIV(primitiveIndex)) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					(inLine beginsWith: ''	case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''	CASE('', caseLabel, '')'' ].
					inLine = ''}'' ifTrue: [
						inPrimitiveResponse := false ] ]
				] ] ] ].

				outFileStream nextPutAll: outLine; cr.
				extraOutLine ifNotNil: [
					outFileStream nextPutAll: extraOutLine; cr ]]].

	outFileStream close' )
]

{ #category : 'tests' }
ClosureCompilerTest >> doTestDebuggerTempAccessWith: one with: two [
	"Test debugger access for temps"
	| outerContext local1 remote1 |
	outerContext := thisContext.
	local1 := 3.
	remote1 := 1/2.
	self assert: (Compiler new evaluate: 'one' in: thisContext to: self) == one.
	self assert: (Compiler new evaluate: 'two' in: thisContext to: self) == two.
	self assert: (Compiler new evaluate: 'local1' in: thisContext to: self) == local1.
	self assert: (Compiler new evaluate: 'remote1' in: thisContext to: self) == remote1.
	Compiler new evaluate: 'local1 := -3.0' in: thisContext to: self.
	self assert: local1 = -3.0.
	(1 to: 2) do:
		[:i| | local2 r1 r2 r3 r4 |
		local2 := i * 3.
		remote1 := local2 / 7.
		self assert: thisContext ~~ outerContext.
		self assert: (r1 := Compiler new evaluate: 'one' in: thisContext to: self) == one.
		self assert: (r2 := Compiler new evaluate: 'two' in: thisContext to: self) == two.
		self assert: (r3 := Compiler new evaluate: 'i' in: thisContext to: self) == i.
		self assert: (r4 := Compiler new evaluate: 'local2' in: thisContext to: self) == local2.
		self assert: (r4 := Compiler new evaluate: 'remote1' in: thisContext to: self) == remote1.
		self assert: (r4 := Compiler new evaluate: 'remote1' in: outerContext to: self) == remote1.
		Compiler new evaluate: 'local2 := 15' in: thisContext to: self.
		self assert: local2 = 15.
		Compiler new evaluate: 'local1 := 25' in: thisContext to: self.
		self assert: local1 = 25.
		{ r1. r2. r3. r4 } "placate the compiler"].
	self assert: local1 = 25.
	self assert: remote1 = (6/7)
]

{ #category : 'tests' }
ClosureCompilerTest >> supportTestSourceRangeAccessForDecompiledInjectInto: method source: source [
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	':= arg1'
								'do: [:argm1_5 | temp3 := arg2 value: temp3 value: argm1_5]'
								'value: temp3 value: argm1_5'
								':= arg2 value: temp3 value: argm1_5'
								'temp3 := arg2 value: temp3 value: argm1_5'
								'value: temp3 value: argm1_5'
								':= arg2 value: temp3 value: argm1_5'
								'temp3 := arg2 value: temp3 value: argm1_5'
								'^temp3')
]

{ #category : 'tests' }
ClosureCompilerTest >> supportTestSourceRangeAccessForDecompiledNoBytecodeInjectInto: method source: source [
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	'at: 1 put: t1'
								'do: [:t4 | t3 at: 1 put: (t2 value: (t3 at: 1) value: t4)]'
								'value: (t3 at: 1) value: t4'
								'at: 1 put: (t2 value: (t3 at: 1) value: t4)'
								']'
								'value: (t3 at: 1) value: t4'
								'at: 1 put: (t2 value: (t3 at: 1) value: t4)'
								']'
								'^t3 at: 1')
]

{ #category : 'tests' }
ClosureCompilerTest >> supportTestSourceRangeAccessForInjectInto: method source: source [
	"Test debugger source range selection for inject:into:"
	^self
		supportTestSourceRangeAccessForInjectInto: method
		source: source
		selectionSequence: #(	':= thisValue'
								'do: [:each | nextValue := binaryBlock value: nextValue value: each]'
								'value: nextValue value: each'
								':= binaryBlock value: nextValue value: each'
								'nextValue := binaryBlock value: nextValue value: each'
								'value: nextValue value: each'
								':= binaryBlock value: nextValue value: each'
								'nextValue := binaryBlock value: nextValue value: each'
								'^nextValue')
]

{ #category : 'tests' }
ClosureCompilerTest >> supportTestSourceRangeAccessForInjectInto: method source: source selectionSequence: selections [
	"Test debugger source range selection for inject:into:"
	| evaluationCount sourceMap debugTokenSequence debugCount |
	DebuggerMethodMap voidMapCache.
	evaluationCount := 0.
	sourceMap := method debuggerMap abstractSourceMap.
	debugTokenSequence := selections collect: [:string| Scanner new scanTokens: string].
	debugCount := 0.
	thisContext
		runSimulated: [(1 to: 2)
						withArgs:
							{	0.
								[:sum :each|
								 evaluationCount := evaluationCount + 1.
								 sum + each]}
						executeMethod: method]
		contextAtEachStep:
			[:ctxt| | range debugTokens |
			(ctxt method == method
			and: ["Exclude the send of #blockCopy: or #closureCopy:copiedValues: and braceWith:with:
				    to create the block, and the #new: and #at:'s for the indirect temp vector.
				   This for compilation without closure bytecodes. (Note that at:put:'s correspond to stores)"
				(ctxt willSend
					and: [(#(closureCopy:copiedValues: blockCopy: new: at: braceWith:with:) includes: ctxt selectorToSendOrSelf) not])
				"Exclude the store of the argument into the home context (for BlueBook blocks)
				 and the store of an indirection vector into an initial temp"
				or: [(ctxt willStore
					and: [(ctxt isBlock and: [ctxt pc = ctxt startpc]) not
					and: [(ctxt isBlock not
						and: [(method usesClosureBytecodes and: [ctxt abstractPC = 2])]) not]])
				or: [ctxt willReturn]]]) ifTrue:
				[debugTokens := debugTokenSequence at: (debugCount := debugCount + 1) ifAbsent: [#(bogusToken)].
				 self assert: (sourceMap includesKey: ctxt abstractPC).
				 range := sourceMap at: ctxt abstractPC ifAbsent: [(1 to: 0)].
				 self assert: (Scanner new scanTokens: (source copyFrom: range first to: range last)) = debugTokens]].
	self assert: evaluationCount = 2
]

{ #category : 'tests' }
ClosureCompilerTest >> testBlockDoitDecompilation [
	"Tests that decompile of a doit block with remote vars executes correcly"
	"Tests that decompilation of a Block, when 'method' of block is equivalent to that compiled by a DoIt, preserves the temp names "
	
	| blockSourceStream methodNode block decompiledBlock method |
	blockSourceStream := '|x y| [:a :b | x := a. y := b. x + y]' readStream.
	methodNode := Compiler new 
						from: blockSourceStream class: nil class context: nil notifying: nil;
						translate: blockSourceStream noPattern: true ifFail: [nil].
	method := methodNode generate.
	block := nil withArgs: #() executeMethod: method.
	
	self shouldnt: [decompiledBlock := block decompile] raise: Error.
	self assert: decompiledBlock printString equals:
 '{[ :argm0_4 :argm0_5 |
	temp1 := argm0_4.
	temp3 := argm0_5.
	temp1 + temp3. ]}'.
]

{ #category : 'tests' }
ClosureCompilerTest >> testBlockNumbering [
	"Test that the compiler and CompiledMethod agree on the block numbering of a substantial doit."
	"self new testBlockNumbering"
	| methodNode method tempRefs |
	methodNode _
		Parser new
			encoderClass: EncoderForV3PlusClosures;
			parse: 'foo
					| numCopiedValuesCounts |
					numCopiedValuesCounts := Dictionary new.
					0 to: 32 do: [:i| numCopiedValuesCounts at: i put: 0].
					Transcript clear.
					Smalltalk allClassesDo:
						[:c|
						{c. c class} do:
							[:b|
							Transcript nextPut: b name first; endEntry.
							b selectorsAndMethodsDo:
								[:s :m| | pn |
								m isQuick not ifTrue:
									[pn := b parserClass new
												encoderClass: EncoderForV3PlusClosures;
												parse: (b sourceCodeAt: s)
												class: b.
									 pn generate: #(0 0 0 0).
									 [pn accept: nil]
										on: MessageNotUnderstood
										do: [:ex| | msg numCopied |
											msg := ex message.
											(msg selector == #visitBlockNode:
											 and: [(msg argument instVarNamed: ''optimized'') not]) ifTrue:
												[numCopied := (msg argument computeCopiedValues: pn) size.
												 numCopiedValuesCounts
													at: numCopied
													put: (numCopiedValuesCounts at: numCopied) + 1].
											msg setSelector: #==.
											ex resume: nil]]]]].
					numCopiedValuesCounts'
			class: Object.
	method _ methodNode generate: #(0 0 0 0).
	tempRefs _ methodNode encoder blockExtentsToTempsMap.
	self assert: tempRefs keys asSet = method startpcsToBlockExtents values asSet
]

{ #category : 'tests' }
ClosureCompilerTest >> testBlockNumberingForInjectInto [
	"Test that the compiler and CompiledMethod agree on the block numbering of Collection>>inject:into:
	 and that temp names for inject:into: are recorded."
	"self new testBlockNumberingForInjectInto"
	| methodNode method tempRefs |
	methodNode := Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: (Collection sourceCodeAt: #inject:into:)
						class: Collection.
	method := methodNode generate: #(0 0 0 0).
	tempRefs := methodNode encoder blockExtentsToTempsMap.
	self assert: tempRefs keys asSet = method startpcsToBlockExtents values asSet.
	self assert: ((tempRefs includesKey: (0 to: 6))
				and: [(tempRefs at: (0 to: 6)) hasEqualElements: #(('thisValue' 1) ('binaryBlock' 2) ('nextValue' (3 1)))]).
	self assert: ((tempRefs includesKey: (2 to: 4))
				and: [(tempRefs at: (2 to: 4)) hasEqualElements: #(('each' 1) ('binaryBlock' 2) ('nextValue' (3 1)))])
]

{ #category : 'tests' }
ClosureCompilerTest >> testDebuggerTempAccess [

	"This test also fails in Squeak. Check if it is ever fixed."
	self shouldFail: [ self doTestDebuggerTempAccessWith: 1 with: 2 ]
]

{ #category : 'tests' }
ClosureCompilerTest >> testInjectIntoDecompilationsEncoderForV3PlusClosures [
	"Test various compilations decompile to the same code for a method sufficiently
	 simple that this is possible and sufficiently complex that the code generated
	 varies between the compilations."
	"self new testInjectIntoDecompilationsEncoderForV3PlusClosures"
	| source |

	source := Collection sourceCodeAt: #inject:into:.
	{ EncoderForV3PlusClosures } do: [ :encoderClass | | method |
		method := (Parser new
							encoderClass: encoderClass;
							parse: source
							class: Collection)
						generate: #(0 0 0 0).
		self assert: (Scanner new scanTokens: method decompileString)
					= #(inject: arg1 into: arg2
							| temp3 |
							temp3 := arg1 .
							self do: [ ':argm1_5' | temp3 := arg2 value: temp3 value: argm1_5 ] .
							^ temp3 . )]
]

{ #category : 'tests' }
ClosureCompilerTest >> testInjectIntoDecompiledDebugs [
	"Test various debugs of the decompiled form debug correctly."
	"self new testInjectIntoDecompiledDebugs"
	| source |

	source := Collection sourceCodeAt: #inject:into:.
	{ EncoderForV3PlusClosures } do: [ :encoderClass | | method |
		method := (Parser new
							encoderClass: encoderClass;
							parse: source
							class: Collection)
						generate: #(0 0 0 0).
		self supportTestSourceRangeAccessForDecompiledInjectInto: method source: method decompileString]
]

{ #category : 'tests' }
ClosureCompilerTest >> testInlineBlockCollectionEM1 [
	| a1 b1 i1 a2 b2 i2 we wb |
	b1 := OrderedCollection new.
	i1 := 1.
	[a1 := i1.
	 i1 <= 3] whileTrue:
		[b1 add: [a1].
		i1 := i1 + 1].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	i2 := 1.
	we := [a2 := i2. i2 <= 3].
	wb := [b2 add: [a2]. i2 := i2 + 1].
	we whileTrue: wb. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2
]

{ #category : 'tests' }
ClosureCompilerTest >> testInlineBlockCollectionLR1 [
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | col add: [ each ] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray
]

{ #category : 'tests' }
ClosureCompilerTest >> testInlineBlockCollectionLR2 [
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | #(1) do: [:ignored| col add: [ each ]] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray
]

{ #category : 'tests' }
ClosureCompilerTest >> testInlineBlockCollectionLR3 [
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
	self assert: (col collect: [ :each | each value ]) asArray = (2 to: 12) asArray
]

{ #category : 'tests' }
ClosureCompilerTest >> testInlineBlockCollectionSD1 [
	| a1 b1 a2 b2 |
	b1 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a1 := i.
		b1 add: [a1]].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a2 := i.
		b2 add: [a2]] yourself. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2
]

{ #category : 'tests' }
ClosureCompilerTest >> testSourceRangeAccessForClosureBytecodeInjectInto [
	"Test debugger source range selection for inject:into: for a version compiled with closures"
	"self new testSourceRangeAccessForClosureBytecodeInjectInto"
	| source method |
	source := Collection sourceCodeAt: #inject:into:.
	method := (Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: source
						class: Collection)
					generate: (Collection compiledMethodAt: #inject:into:) trailer.
	self supportTestSourceRangeAccessForInjectInto: method source: source
]

{ #category : 'tests' }
ClosureCompilerTest >> testSourceRangeAccessForInjectInto [
	"Test debugger source range selection for inject:into: for the current version of the method"
	"self new testSourceRangeAccessForInjectInto"
	self supportTestSourceRangeAccessForInjectInto: (Collection compiledMethodAt: #inject:into:)
		source: (Collection sourceCodeAt: #inject:into:)
]

{ #category : 'tests' }
ClosureCompilerTest >> testTempNameAccessForInjectInto [
	"self new testTempNameAccessForInjectInto"
	| methodNode method evaluationCount block debuggerMap |
	methodNode := Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: (Collection sourceCodeAt: #inject:into:)
						class: Collection.
	method := methodNode generate: #(0 0 0 0).
	debuggerMap := DebuggerMethodMap forMethod: method methodNode: methodNode.
	evaluationCount := 0.
	block := [:prev :each| | theContext tempNames |
			evaluationCount := evaluationCount + 1.
			theContext := thisContext sender.
			tempNames := debuggerMap tempNamesForContext: theContext.
			self assert: (tempNames hasEqualElements: tempNames).
			#('thisValue' 'each' 'binaryBlock' 'nextValue')
				with: { 0. each. block. prev}
				do: [:tempName :value|
					self assert: (debuggerMap namedTempAt: (tempNames indexOf: tempName) in: theContext) == value.
					tempName ~= 'each' ifTrue:
						[self assert: (debuggerMap namedTempAt: (tempNames indexOf: tempName) in: theContext home) == value]]].
	(1 to: 10) withArgs: { 0. block } executeMethod: method.
	self assert: evaluationCount = 10
]
