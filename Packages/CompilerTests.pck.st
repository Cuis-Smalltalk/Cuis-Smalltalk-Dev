'From Cuis7.9 [latest update: #8161] on 8 September 2026 at 8:41:11 pm'!
'Description '!
!provides: 'CompilerTests' 1 1!
!requires: 'DynamicallyCodeCreatingTestSupport' 1 0 nil!
SystemOrganization addCategory: #CompilerTests!


!classDefinition: #ArrayLiteralTest category: #CompilerTests!
TestCase subclass: #ArrayLiteralTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ArrayLiteralTest class' category: #CompilerTests!
ArrayLiteralTest class
	instanceVariableNames: ''!

!classDefinition: #ClassDefinitionNodeAnalyzerTest category: #CompilerTests!
TestCase subclass: #ClassDefinitionNodeAnalyzerTest
	instanceVariableNames: 'iv1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ClassDefinitionNodeAnalyzerTest class' category: #CompilerTests!
ClassDefinitionNodeAnalyzerTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureCompilerTest category: #CompilerTests!
TestCase subclass: #ClosureCompilerTest
	instanceVariableNames: ''
	classVariableNames: 'CmpRR CogRTLOpcodes Jump MoveCqR Nop'
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ClosureCompilerTest class' category: #CompilerTests!
ClosureCompilerTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureSerializationTest category: #CompilerTests!
TestCase subclass: #ClosureSerializationTest
	instanceVariableNames: 'ivar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ClosureSerializationTest class' category: #CompilerTests!
ClosureSerializationTest class
	instanceVariableNames: ''!

!classDefinition: #ClosureTests category: #CompilerTests!
TestCase subclass: #ClosureTests
	instanceVariableNames: 'collection ivar'
	classVariableNames: 'ClassVar'
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ClosureTests class' category: #CompilerTests!
ClosureTests class
	instanceVariableNames: ''!

!classDefinition: #ContextCompilationTest category: #CompilerTests!
TestCase subclass: #ContextCompilationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ContextCompilationTest class' category: #CompilerTests!
ContextCompilationTest class
	instanceVariableNames: ''!

!classDefinition: #DecompilerTests category: #CompilerTests!
TestCase subclass: #DecompilerTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'DecompilerTests class' category: #CompilerTests!
DecompilerTests class
	instanceVariableNames: ''!

!classDefinition: #DecompilerTestFailuresCollector category: #CompilerTests!
DecompilerTests subclass: #DecompilerTestFailuresCollector
	instanceVariableNames: 'failures'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'DecompilerTestFailuresCollector class' category: #CompilerTests!
DecompilerTestFailuresCollector class
	instanceVariableNames: ''!

!classDefinition: #MirrorPrimitiveTests category: #CompilerTests!
TestCase subclass: #MirrorPrimitiveTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'MirrorPrimitiveTests class' category: #CompilerTests!
MirrorPrimitiveTests class
	instanceVariableNames: ''!

!classDefinition: #ParseNodeEnumeratorTest category: #CompilerTests!
TestCase subclass: #ParseNodeEnumeratorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ParseNodeEnumeratorTest class' category: #CompilerTests!
ParseNodeEnumeratorTest class
	instanceVariableNames: ''!

!classDefinition: #ParseNodeTest category: #CompilerTests!
TestCase subclass: #ParseNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ParseNodeTest class' category: #CompilerTests!
ParseNodeTest class
	instanceVariableNames: ''!

!classDefinition: #BlockNodeTest category: #CompilerTests!
ParseNodeTest subclass: #BlockNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'BlockNodeTest class' category: #CompilerTests!
BlockNodeTest class
	instanceVariableNames: ''!

!classDefinition: #MessageNodeTest category: #CompilerTests!
ParseNodeTest subclass: #MessageNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'MessageNodeTest class' category: #CompilerTests!
MessageNodeTest class
	instanceVariableNames: ''!

!classDefinition: #VariableNodeTest category: #CompilerTests!
ParseNodeTest subclass: #VariableNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'VariableNodeTest class' category: #CompilerTests!
VariableNodeTest class
	instanceVariableNames: ''!

!classDefinition: #ParserTest category: #CompilerTests!
TestCase subclass: #ParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ParserTest class' category: #CompilerTests!
ParserTest class
	instanceVariableNames: ''!

!classDefinition: #PrettyPrintingTest category: #CompilerTests!
TestCase subclass: #PrettyPrintingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'PrettyPrintingTest class' category: #CompilerTests!
PrettyPrintingTest class
	instanceVariableNames: ''!

!classDefinition: #ReturnNodeTest category: #CompilerTests!
TestCase subclass: #ReturnNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ReturnNodeTest class' category: #CompilerTests!
ReturnNodeTest class
	instanceVariableNames: ''!

!classDefinition: #ScannerTest category: #CompilerTests!
TestCase subclass: #ScannerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'ScannerTest class' category: #CompilerTests!
ScannerTest class
	instanceVariableNames: ''!

!classDefinition: #SourceCodeIntervalTest category: #CompilerTests!
TestCase subclass: #SourceCodeIntervalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'SourceCodeIntervalTest class' category: #CompilerTests!
SourceCodeIntervalTest class
	instanceVariableNames: ''!

!classDefinition: #CompilerTest category: #CompilerTests!
DynamicallyCodeCreatingTest subclass: #CompilerTest
	instanceVariableNames: 'sourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompilerTests'!
!classDefinition: 'CompilerTest class' category: #CompilerTests!
CompilerTest class
	instanceVariableNames: ''!


!DecompilerTests commentStamp: 'sd 9/26/2004 13:24' prior: 0!
Apparently the decompiler does not really work totally.
Here are a bunch of methods that can help improving the decompiler:
	- blockingClasses return class for which it is impossible to decompile methods 
	- failures are problems that lead to a DNU
	- decompilerDiscrepancies are the results of running decompileTestHelper..as you see the pattern 	
	is quite present.!

!DecompilerTestFailuresCollector commentStamp: 'eem 1/16/2025 18:40:11' prior: 0!
(| dtfc |
dtfc := DecompilerTestFailuresCollector new.
(dtfc class superclass organization listAtCategoryNamed: #tests) do:
	[:s| dtfc perform: s].
dtfc failures asArray)

(Transcript nextPut: ${.
self do: [:mr| Transcript print: mr actualClass; nextPut: $.; space; store: mr methodSymbol; nextPut: $.; cr; flush].
Transcript nextPut: $}; flush)!

!ArrayLiteralTest methodsFor: 'tests' stamp: 'jpb 3/Aug/2019, 3:05:08 am (UTC)'!
testIsArray
	self assert: ({1. 2. 'hello'. #Number} is: #Array).! !

!ArrayLiteralTest methodsFor: 'tests' stamp: 'HAW 26/Oct/2019, 10:24:11 pm (UTC)'!
testReservedIdentifiers
	
	self assert: #(nil true false) equals: {nil. true. false}.! !

!ArrayLiteralTest methodsFor: 'tests' stamp: 'HAW 26/Oct/2019, 10:24:52 pm (UTC)'!
testSymbols

	self assert: #(#nil #true #false #'nil' #'true' #'false') equals: {#nil. #true. #false. #nil. #true. #false}.! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:13 am (UTC)'!
test01isAtSuperclassWhenIndexIsInSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtSuperclass: 1).
	self assert: (analyzer isAtSuperclass: self class superclass name size)! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:19 am (UTC)'!
test02isNotAtSuperclassWhenIndexIsOutsideSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtSuperclass: 0).
	self deny: (analyzer isAtSuperclass: self class superclass name size + 1).
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:25 am (UTC)'!
test03isAtClassNameWhenIndexIsInClassName

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) - 1).
	self assert: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size - 1).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:29 am (UTC)'!
test04isNotAtClassNameWhenIndexIsOutsideClassName

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) - 2).
	self deny: (analyzer isAtClassName: (self class definition findString: self class name) +  self class name size).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:35 am (UTC)'!
test05isAtInstanceVariablesWhenIndexIsInInstanceVariablesString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1')).
	self assert: (analyzer isAtInstanceVariables: (self class definition findString: 'iv1') + 2).
! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:40 am (UTC)'!
test06isNotAtInstanceVariablesWhenIndexIsOutsideInstanceVariablesString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''')).
	self deny: (analyzer isAtInstanceVariables: (self class definition findString: '''iv1''') + 4).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 4:16:14 pm (UTC)'!
test07CanAccessSuperclass

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: analyzer superclass equals: self class superclass.! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:50 am (UTC)'!
test08isAtCategoryWhenIndexIsInCategoryString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString)).
	self assert: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size - 1).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:07:55 am (UTC)'!
test09isNotAtCategoryWhenIndexIsOutsideCategoryString

	| classDefinitionNode analyzer |
	
	classDefinitionNode := self thisClassDefinitionNode.
	
	analyzer := ClassDefinitionNodeAnalyzer for: classDefinitionNode.
	
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) - 1).
	self deny: (analyzer isAtCategory: (self class definition findString: self class category asString) + self class category size).
	
	! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 11/Oct/2019, 8:51:32 pm (UTC)'!
test10InstanceVariablesIsTheSecondParameter

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer instanceVariableNamesPositionForClassDefinition ) 
			equals: 'instanceVariableNames:' ]! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 18/Apr/2019, 1:06:49 am (UTC)'!
test11CategoryIsTheFifthParameter

	| subclassCreationSelectors |
	
	subclassCreationSelectors := Class organization listAtCategoryNamed: 'subclass creation'.
	self assert: subclassCreationSelectors notEmpty.
	
	subclassCreationSelectors do: [ :aSubclassCreationSelector |
		self 
			assert: (aSubclassCreationSelector keywords at: ClassDefinitionNodeAnalyzer categoryPosition) 
			equals: 'category:' ]! !

!ClassDefinitionNodeAnalyzerTest methodsFor: 'testing' stamp: 'HAW 17/Apr/2019, 10:16:08 pm (UTC)'!
thisClassDefinitionNode
	
	 ^self class methodNodeFor: self class definition noPattern: true ! !

!ClosureCompilerTest methodsFor: 'source' stamp: 'jmv 9/Jan/2024, 8:00:21 pm (UTC)'!
closureCases
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

	outFileStream close' )! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 24/Jun/2008, 4:28:00 pm (UTC)'!
doTestDebuggerTempAccessWith: one with: two
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
	self assert: remote1 = (6/7)! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:23:53 pm (UTC)'!
supportTestSourceRangeAccessForDecompiledInjectInto: method source: source
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
								'^temp3')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 4/Jun/2008, 11:44:00 pm (UTC)'!
supportTestSourceRangeAccessForDecompiledNoBytecodeInjectInto: method source: source
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
								'^t3 at: 1')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 29/Dec/2023, 9:15:16 pm (UTC)'!
supportTestSourceRangeAccessForInjectInto: method source: source
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
								'^nextValue')! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 29/Jul/2008, 9:16:00 pm (UTC)'!
supportTestSourceRangeAccessForInjectInto: method source: source selectionSequence: selections
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
	self assert: evaluationCount = 2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:18:54 pm (UTC)'!
testBlockDoitDecompilation
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
	temp1 + temp3. ]}'.! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 17/Jan/2011, 4:13:00 am (UTC)'!
testBlockNumbering
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
	self assert: tempRefs keys asSet = method startpcsToBlockExtents values asSet! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 17/Jan/2011, 4:13:00 am (UTC)'!
testBlockNumberingForInjectInto
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
				and: [(tempRefs at: (2 to: 4)) hasEqualElements: #(('each' 1) ('binaryBlock' 2) ('nextValue' (3 1)))])! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 11:20:04 am (UTC)'!
testDebuggerTempAccess

	"This test also fails in Squeak. Check if it is ever fixed."
	self shouldFail: [ self doTestDebuggerTempAccessWith: 1 with: 2 ]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:21:25 pm (UTC)'!
testInjectIntoDecompilationsEncoderForV3PlusClosures
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
							^ temp3 . )]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:21:01 pm (UTC)'!
testInjectIntoDecompiledDebugs
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
		self supportTestSourceRangeAccessForDecompiledInjectInto: method source: method decompileString]! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 24/Jul/2009, 3:51:00 pm (UTC)'!
testInlineBlockCollectionEM1
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
	self assert: b1 = b2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 7/Mar/2009, 3:25:00 pm (UTC)'!
testInlineBlockCollectionLR1
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | col add: [ each ] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 7/Mar/2009, 3:39:00 pm (UTC)'!
testInlineBlockCollectionLR2
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | #(1) do: [:ignored| col add: [ each ]] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 9/Mar/2009, 3:00:00 pm (UTC)'!
testInlineBlockCollectionLR3
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
	self assert: (col collect: [ :each | each value ]) asArray = (2 to: 12) asArray! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 22/Jul/2009, 8:55:00 pm (UTC)'!
testInlineBlockCollectionSD1
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
	self assert: b1 = b2! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:21:37 pm (UTC)'!
testSourceRangeAccessForClosureBytecodeInjectInto
	"Test debugger source range selection for inject:into: for a version compiled with closures"
	"self new testSourceRangeAccessForClosureBytecodeInjectInto"
	| source method |
	source := Collection sourceCodeAt: #inject:into:.
	method := (Parser new
						encoderClass: EncoderForV3PlusClosures;
						parse: source
						class: Collection)
					generate: (Collection compiledMethodAt: #inject:into:) trailer.
	self supportTestSourceRangeAccessForInjectInto: method source: source! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 14/Mar/2024, 4:21:44 pm (UTC)'!
testSourceRangeAccessForInjectInto
	"Test debugger source range selection for inject:into: for the current version of the method"
	"self new testSourceRangeAccessForInjectInto"
	self supportTestSourceRangeAccessForInjectInto: (Collection compiledMethodAt: #inject:into:)
		source: (Collection sourceCodeAt: #inject:into:)! !

!ClosureCompilerTest methodsFor: 'tests' stamp: 'eem 15/Jun/2008, 3:26:00 pm (UTC)'!
testTempNameAccessForInjectInto
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
	self assert: evaluationCount = 10! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 20/May/2008, 1:40:00 pm (UTC)'!
methodWithCopiedAndAssignedTemps
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
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 20/May/2008, 12:45:00 am (UTC)'!
methodWithCopiedAndPostClosedOverAssignedTemps
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
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 20/May/2008, 12:10:00 am (UTC)'!
methodWithCopiedTemps
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
		class: self class) generateUsingClosures: #(0 0 0 0)"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 19/May/2008, 6:24:00 pm (UTC)'!
methodWithOptimizedBlocks
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
		class: self class"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 19/May/2008, 6:24:00 pm (UTC)'!
methodWithOptimizedBlocksA
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
		class: self class"! !

!ClosureCompilerTest class methodsFor: 'code examples' stamp: 'eem 19/May/2008, 6:12:00 pm (UTC)'!
methodWithVariousTemps
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
		class: self class"! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample01
	| blockClosure materialized |
	blockClosure _ self bc01.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample02
	| blockClosure materialized |
	blockClosure _ self bc02.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample03
	| blockClosure materialized |
	blockClosure _ self bc03.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample04
	| blockClosure materialized |
	blockClosure _ self bc04.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample05
	| blockClosure materialized |
	blockClosure _ self bc05.
	materialized _ blockClosure veryDeepCopy.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.
	self assert: blockClosure value = materialized value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample06
	| blockClosures materialized firstRun |

	blockClosures _ self bc06.
	firstRun _ blockClosures second value.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self assert: firstRun = materialized second value.
	self deny: blockClosures second value = materialized second value.

	blockClosures _ self bc06.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.
	self deny: firstRun = materialized second value.
	self assert: blockClosures second value = materialized second value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample07
	| blockClosures materialized |

	blockClosures _ self bc07.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self deny: blockClosures third value = materialized third value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc07.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures third value = materialized third value.
	self assert: blockClosures first value = materialized first value.! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample08
	| blockClosures materialized |

	blockClosures _ self bc08.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self deny: (blockClosures second value: 7) = (materialized second value: 7).
	self deny: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).

	blockClosures _ self bc08.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: blockClosures third value = materialized third value.
	self assert: (blockClosures second value: 7) = (materialized second value: 7).
	self assert: (blockClosures fourth value: 2 value: 3) = (materialized fourth value: 2 value: 3).! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample09
	| blockClosures materialized |

	blockClosures _ self bc09.
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].

	self assert: (blockClosures first value: $c) = (materialized first value: $c).
	self assert: (blockClosures second value: $d value: #e) = (materialized second value: $d value: #e).
	self assert: (blockClosures third value: $d value: $e) = (materialized third value: $d value: $e).! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample10
	| blockClosure1 blockClosure2 blockClosure3 value materialized1 materialized2a materialized2b materialized3a materialized3b materialized3c |

	blockClosure1 _ self bc10.
	blockClosure2 _ blockClosure1 withFirstArg: 1.
	blockClosure3 _ (blockClosure1 withFirstArg: 1) withFirstArg: 2.
	value _ blockClosure1 value: 1 value: 2.
	self assert: (blockClosure2 value: 2) = value.
	self assert: blockClosure3 value = value.

	materialized1 _ blockClosure1 veryDeepCopy.
	materialized2a _ materialized1 withFirstArg: 1.
	materialized2b _ blockClosure2 veryDeepCopy.
	materialized3a _ (materialized1 withFirstArg: 1) withFirstArg: 2.
	materialized3b _ materialized2b withFirstArg: 2.
	materialized3c _ blockClosure3 veryDeepCopy.
	self assert: (materialized1 value: 1 value: 2) = value.
	self assert: (materialized2a value: 2) = value.
	self assert: (materialized2b value: 2) = value.
	self assert: materialized3a value = value.
	self assert: materialized3b value = value.
	self assert: materialized3c value = value.
! !

!ClosureSerializationTest methodsFor: 'testing' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
testSample11
	| blockClosures materialized |

	blockClosures _ self bc11.
	"If sibling closures are serialized separately, their relationship is lost in the copies."
	materialized _ blockClosures collect: [ :each | each veryDeepCopy ].
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self deny: blockClosures first value = materialized first value.

	blockClosures _ self bc11.
	"If sibling closures are serialized together, their relationship is kept in the copies."
	materialized _ blockClosures veryDeepCopy.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.
	self assert: blockClosures second value = materialized second value.
	self assert: blockClosures first value = materialized first value.! !

!ClosureSerializationTest methodsFor: 'aux' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
gimme5
	^1+4! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc01
	^[ self gimme5 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc02
	^[ ivar + 4 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc03
	^[ ivar _ ivar + 3 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc04
	| t |
	t _ self gimme5.
	^[ t * 2 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc05
	| t |
	t _ self gimme5.
	^[ t _ t * 2 ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc06
	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{[ u * 2 ]. [ t * 2 ].[t _ t*3]}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc07
	| a b c d e f g |
	a _ self gimme5 + 1.
	b _ self gimme5 + 2.
	c _ self gimme5 + 3.
	d _ self gimme5 + 4.
	e _ self gimme5 + 5.
	f _ self gimme5 + 6.
	g _ self gimme5 + 7.
	^{[ a * 10 + b * 10 + c * 10 + d * 10 + e + f + g ]. [ a _ 7. a * 2 ].[ c _ 9. d _ d*10. a * 10 + b * 10 + c + d]}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc08

	| u t |
	u _ self gimme5 + 2.
	t _ self gimme5.
	^{
		[ | x y z | x _ t. y _ x*0+2. z _ x * t. {u * 2. x. y. z} ].
		[ :a | t * 2 + a ].
		[|x y | t _ t*3. y _ t*2. {x. t. y}].
		[:a :b | a * b * t ]
	}.! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc09

	| clo1 clo2 closure |
	closure := (
			([ :a |
				[ :b |
					clo1 := [ :c | {c. b. a.}]. 
					clo2 := [ :d :e | {a. b. e. d.}]
				]
			]) value: $a
		) value: 'b'.
	^{ clo1. clo2. closure }! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 27/Sep/2019, 3:19:41 am (UTC)'!
bc10

	^ [ :a :b | a + b ]! !

!ClosureSerializationTest methodsFor: 'sample closures' stamp: 'jmv 1/Mar/2025, 9:39:45 pm (UTC)'!
bc11
	| a b c d e f g |
	a := self gimme5 + 1.
	b := self gimme5 + 2.
	c := self gimme5 + 3.
	d := self gimme5 + 4.
	e := self gimme5 + 5.
	f := self gimme5 + 6.
	g := self gimme5 + 7.
	{ b. d. e. f }"print".
	^{[ g + a * 10 + a + a + g + c ]. [ b := 7. c := c * 2 ]}.! !

!ClosureSerializationTest methodsFor: 'setUp/tearDown' stamp: 'jmv 20/Mar/2024, 7:45:08 pm (UTC)'!
setUp
	super setUp.
	ivar := 7.
	^ivar! !

!ClosureTests methodsFor: 'utilities' stamp: 'lr 9/Mar/2009, 8:48:00 pm (UTC)'!
assertValues: anArray
	| values |
	values := collection collect: [ :each | each value ].
	self 
		assert: anArray asArray = values asArray
		description: 'Expected: ' , anArray asArray printString , 
			', but got ' , values asArray printString! !

!ClosureTests methodsFor: 'utilities' stamp: 'cwp 16/Nov/2009, 12:12:00 pm (UTC)'!
evaluateCopyOf: aBlock
	aBlock copy value! !

!ClosureTests methodsFor: 'utilities' stamp: 'cwp 16/Nov/2009, 12:12:00 pm (UTC)'!
methodWithNonLocalReturn
	self evaluateCopyOf: [^ self].
	self signalFailure: 'Should never reach here'! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 11/Mar/2009, 9:35:00 pm (UTC)'!
methodArgument: anObject
	^ [ anObject ]
	! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 11/Mar/2009, 9:33:00 pm (UTC)'!
testBlockArgument
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 11/Mar/2009, 9:33:00 pm (UTC)'!
testBlockTemp
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'cwp 16/Nov/2009, 12:11:00 pm (UTC)'!
testCopyNonLocalReturn
	self
		shouldnt: [self methodWithNonLocalReturn]
		raise: Error! !

!ClosureTests methodsFor: 'testing' stamp: 'jmv 14/Jul/2025, 8:47:28 pm (UTC)'!
testIsCleanClosure
	"
	ClosureTests new testIsCleanClosure
	"
	| tempVar |
	tempVar := 1.
	self assert: [ 3 + 4 ] isCleanClosure.
	self assert: [ :a | a * 2 ] isCleanClosure.
	self assert: [ Smalltalk size ] isCleanClosure.
	self assert: [ ClosureTests selectors size ] isCleanClosure.
	self assert: [ :blockArg | blockArg printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp printString ] isCleanClosure.
	self assert: [ | blockTemp | blockTemp := 7 ] isCleanClosure.
	self assert: [ | c | c := [ :a :b | a+b ]. c value: 3 value: 4 ] isCleanClosure.

	self assert: [[ 3 + 4 ] value] isCleanClosure.
	self assert: [[ :a | a * 2 ] value] isCleanClosure.
	self assert: [[ Smalltalk size ] value] isCleanClosure.
	self assert: [[ ClosureTests selectors size ] value] isCleanClosure.
	self assert: [[ :blockArg | blockArg printString ] value] isCleanClosure.
	self assert: [[ | blockTemp | blockTemp printString ] value] isCleanClosure.
	self assert: [[ | blockTemp | blockTemp := 7 ] value] isCleanClosure.
	self assert: [[ | c | c := [ :a :b | a+b ]. c value: 3 value: 4 ] value] isCleanClosure.

	self assert: [ | outerBlockTemp | [ outerBlockTemp printString ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [ outerBlockTemp := 7 ] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ | outerBlockTemp | [[ outerBlockTemp := 7 ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp printString ]] ] isCleanClosure.
	self assert: [ [| outerBlockTemp | [ outerBlockTemp := 7 ]] ] isCleanClosure.

	self deny: [ | outerBlockTemp | [ outerBlockTemp printString ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [ outerBlockTemp := 7 ] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp printString ]] isCleanClosure ] value.
	self deny: [ | outerBlockTemp | [[ outerBlockTemp := 7 ]] isCleanClosure ] value.

	self deny: [ tempVar + 1 ] isCleanClosure.
	self deny: [ tempVar := 1 ] isCleanClosure.
	self deny: [ ivar + 1 ] isCleanClosure.
	self deny: [ ivar := 1 ] isCleanClosure.
	self deny: [ ^ true ] isCleanClosure.
	self deny: [ self printString ] isCleanClosure.
	self deny: [ ^ self ] isCleanClosure.
	self deny: [ super printString ] isCleanClosure.
	self deny: [ ClassVar + 1 ] isCleanClosure.
	self deny: [ ClassVar := 1 ] isCleanClosure.

	self deny: [[ 3 + 4 ]. [ tempVar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ tempVar := 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ivar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ivar := 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ]. [ ^ true ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ self printString ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ^ self ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ super printString ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ClassVar + 1 ] value ] isCleanClosure.
	self deny: [[ 3 + 4 ].[ ClassVar := 1 ] value ] isCleanClosure.! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 11/Mar/2009, 9:36:00 pm (UTC)'!
testMethodArgument
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	self assert: block value = 2! !

!ClosureTests methodsFor: 'testing' stamp: 'lr 11/Mar/2009, 9:36:00 pm (UTC)'!
testMethodTemp
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2! !

!ClosureTests methodsFor: 'running' stamp: 'lr 9/Mar/2009, 8:48:00 pm (UTC)'!
setUp
	super setUp.
	collection := OrderedCollection new! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoArgument
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoArgumentNotInlined
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoInsideTemp
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoInsideTempNotInlined
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoOutsideTemp
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-todo' stamp: 'lr 10/Mar/2009, 6:51:00 pm (UTC)'!
testToDoOutsideTempNotInlined
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:29:00 pm (UTC)'!
testWhileModificationAfter
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		collection add: [ index ].
		index := index + 1 ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:29:00 pm (UTC)'!
testWhileModificationAfterNotInlined
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:29:00 pm (UTC)'!
testWhileModificationBefore
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:29:00 pm (UTC)'!
testWhileModificationBeforeNotInlined
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:52:00 pm (UTC)'!
testWhileWithTemp
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)! !

!ClosureTests methodsFor: 'testing-while' stamp: 'lr 10/Mar/2009, 6:53:00 pm (UTC)'!
testWhileWithTempNotInlined
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(1 2 3 4 5)! !

!ContextCompilationTest methodsFor: 'tests' stamp: 'eem 19/Jun/2008, 2:11:00 pm (UTC)'!
testVariablesAndOffsetsDo

	"ContextCompilationTest new testVariablesAndOffsetsDo"
	| contextClasses |
	contextClasses := ContextPart withAllSuperclasses, ContextPart allSubclasses asArray.
	contextClasses do:
		[:class|
		class variablesAndOffsetsDo:
			[:var :offset|
			self assert: offset < 0.
			self assert: (class instVarNameForIndex: offset negated) == var]].

	InstructionStream withAllSuperclasses, InstructionStream allSubclasses asArray do:
		[:class|
		(contextClasses includes: class) ifFalse:
			[class variablesAndOffsetsDo:
				[:var :offset|
				(InstructionStream instVarNames includes: var) ifFalse:
					[self assert: offset > 0.
					 self assert: (class instVarNameForIndex: offset) == var]]]]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'sd 25/Sep/2004, 7:30:00 pm (UTC)'!
blockingClasses


	^ #(CompiledMethod)! !

!DecompilerTests methodsFor: 'utilities' stamp: 'jmv 11/Dec/2022, 11:36:11 pm (UTC)'!
checkDecompileMethod: oldMethod
	
	| cls selector oldMethodNode methodNode newMethod oldCodeString newCodeString |
	cls _ oldMethod methodClass.
	selector := oldMethod selector.
	oldMethodNode _ Decompiler new
						decompile: selector
						in: cls
						method: oldMethod.
	[oldMethodNode properties includesKey: #warning]
		whileTrue: [oldMethodNode properties removeKey: #warning].
	oldCodeString _ oldMethodNode decompileString.
	methodNode _ [ Compiler new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil]
						on: SyntaxErrorNotification
						do: [ :ex |
							ex errorMessage = 'Cannot store into'
								ifTrue: [ex return: #badStore].
							ex pass ].
	"Ignore cannot store into block arg errors; they're not our issue."
	methodNode ~~ #badStore ifTrue: [
		newMethod _ methodNode generate: #(0 0 0 0).
		newCodeString := (Decompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString.
		"Decompiler might move temp declarations (keeping valid, equivalent code) and therefore affect the temp numbering.
		Make temp numbering canonical in decompiler code, to avoid bogus failures"
		oldCodeString _ self withFixedTempNumbering: oldCodeString.
		newCodeString _ self withFixedTempNumbering: newCodeString.
		"(DifferenceFinder displayPatchFrom: oldCodeString to: newCodeString tryWords: true)
			editLabel: 'Decompilation Differences for ', cls name,'>>',selector."
		"(DifferenceFinder displayPatchFrom: oldMethod abstractSymbolic to: newMethod abstractSymbolic tryWords: true)
			editLabel: 'Bytecode Differences for ', cls name,'>>',selector."
		self assert: oldCodeString = newCodeString
			description: cls name asString, ' ', selector asString
			resumable: true ]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'HAW 6/May/2025, 11:10:09 pm (UTC)'!
decompileClassesInRootCategoryOf: aClass

	| rootCategory |
	
	rootCategory := SystemOrganization rootCategoryOf: aClass category.
	(SystemOrganization allClassesAt: rootCategory) do: [ :class |
		class selectorsAndMethodsDo: [ :selector :method |
			(class organization categoryOfElement: selector) ifNotNil: [:category |
				(category beginsWith: '*') ifFalse: [
					(self exceptionClassForFailureFor: class selector: selector)
						ifNil: [ self checkDecompileMethod: method ]
						ifNotNil: [ :exceptionClass |
							self
								should: [ self checkDecompileMethod: method ]
								raise: exceptionClass ] ] ] ] ]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'ul 6/May/2015, 7:59:00 pm (UTC)'!
exceptionClassForFailureFor: class selector: selector
	
	^self decompilerFailures
		detect: [ :each | each first = class name and: [ each second = selector ] ]
		ifFound: [ :each | each third ifNotNil: [ Smalltalk classNamed: each third ] ]
		ifNone: [ nil ]! !

!DecompilerTests methodsFor: 'utilities' stamp: 'jmv 1/Jun/2021, 6:00:00 pm (UTC)'!
withFixedTempNumbering: aString
	| k code newCode tempStart startAt |
	code _ aString.
	`{'argm'. 'temp'}` with: `{'arg'. 'tmp'}` do: [ :a :b |
		k _ 1.
		startAt _ 1.
		[tempStart _ code findString: a startingAt: startAt. tempStart > 0] whileTrue: [ | end |
			end _ tempStart. [end <= code size and: [(code at: end) tokenish]] whileTrue: [end _ end + 1]. end _ end-1.
			newCode _ code  copyReplaceTokens: (code copyFrom: tempStart to: end) with: b, k printString.
			code = newCode ifTrue: [startAt _ tempStart + 1].
			code _ newCode.
			k _ k + 1 ]].
	^code! !

!DecompilerTests methodsFor: 'filtering errors' stamp: 'jmv 22/Jan/2025, 6:47:23 pm (UTC)'!
decompilerFailures
	"Here is the list of failures: either a syntax error, a hard error or some failure to decompile correctly.
	 Collected initially via 
		DecompilerTestFailuresCollector new computeFailures.
	 But can be maintained manually."

	"class name, selector, error class name or nil"
	^#(
		(Dictionary scanFor: TestFailure)
		(IdentityDictionary scanFor: TestFailure)
		(IdentitySet scanFor: TestFailure)
		(Integer lowBit TestFailure)
		(MethodDictionary scanFor: TestFailure)
		(Set scanFor: TestFailure)
		(WeakIdentityKeyDictionary scanFor: TestFailure)
		(WeakKeyDictionary scanFor: TestFailure))! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:12:51 pm (UTC)'!
testDecompilerBrowserClasses

	self decompileClassesInRootCategoryOf: Browser ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:09:16 pm (UTC)'!
testDecompilerCollectionClasses

	self decompileClassesInRootCategoryOf: Collection! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:11:28 pm (UTC)'!
testDecompilerCompilerClasses

	self decompileClassesInRootCategoryOf: Compiler ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:11:11 pm (UTC)'!
testDecompilerExceptionsClasses

	self decompileClassesInRootCategoryOf: Exception ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:12:17 pm (UTC)'!
testDecompilerGraphicsClasses

	self decompileClassesInRootCategoryOf: Form ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:09:22 pm (UTC)'!
testDecompilerKernelClasses

	self decompileClassesInRootCategoryOf: Object ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:12:32 pm (UTC)'!
testDecompilerMorphicClasses

	self decompileClassesInRootCategoryOf: Morph ! !

!DecompilerTests methodsFor: 'tests' stamp: 'HAW 6/May/2025, 11:12:01 pm (UTC)'!
testDecompilerSystemClasses

	self decompileClassesInRootCategoryOf: Editor ! !

!DecompilerTests methodsFor: 'tests' stamp: 'jmv 28/Dec/2013, 11:04:00 pm (UTC)'!
testRemoteTemp
	| aBlock |
	aBlock := Compiler evaluate: '| x y |  [:a :b | x := a. y := b. x+y]'.
	self shouldnt: [aBlock decompile] raise: Error
	! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'ul 6/May/2015, 8:36:00 pm (UTC)'!
assert: aBoolean description: aString resumable: resumableBoolean

	| method |
	aBoolean ifTrue: [ ^self ].
	method := thisContext sender home tempAt: 1.
	self noteFailure: TestFailure for: method! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'eem 16/Jan/2025, 10:45:07 pm (UTC)'!
computeFailures
	
	| tests |
	failures := nil.
	tests := (DecompilerTests organization listAtCategoryNamed: #tests) select:
				[ :s | s beginsWith: 'testDecompilerInClasses' ].
	tests
		do: [ :each | self perform: each ]
		displayingProgress: 'Computing failures...'.
	^self decompilerFailures! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'ul 6/May/2015, 8:53:00 pm (UTC)'!
decompilerFailures

	failures ifNil: [ ^#() ].
	^failures asArray sort: [ :a :b |
		| r |
		r := a first compare: b first caseSensitive: false.
		r = 1 or: [ r = 2 and: [
			(a second compare: b second caseSensitive: false) <= 2 ] ] ]! !

!DecompilerTestFailuresCollector methodsFor: 'accessing' stamp: 'eem 10/Nov/2008, 7:47:00 pm (UTC)'!
failures
	^failures! !

!DecompilerTestFailuresCollector methodsFor: 'utilities' stamp: 'ul 6/May/2015, 8:38:00 pm (UTC)'!
checkDecompileMethod: oldMethod
	
	[ super checkDecompileMethod: oldMethod ]
		on: Error
		do: [ :error | self noteFailure: error class for: oldMethod ]! !

!DecompilerTestFailuresCollector methodsFor: 'utilities' stamp: 'ul 6/May/2015, 8:24:00 pm (UTC)'!
exceptionClassForFailureFor: class  selector: selector
	
	^nil! !

!DecompilerTestFailuresCollector methodsFor: 'utilities' stamp: 'ul 6/May/2015, 8:40:00 pm (UTC)'!
noteFailure: exceptionClass for: method

	(failures ifNil: [ failures := OrderedCollection new ]) add: {
		method methodClass name.
		method selector.
		exceptionClass name }! !

!DecompilerTestFailuresCollector class methodsFor: 'as yet unclassified' stamp: 'mt 3/Sep/2019, 1:08:00 pm (UTC)'!
isAbstract

	^ true! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 28/Dec/2013, 11:06:00 pm (UTC)'!
testMirrorAt
	| stackpBefore stackpAfter array byteArray |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	byteArray := ByteArray with: 1 with: 2 with: 3.
	self assert: (thisContext object: array basicAt: 1) = 1.
	self assert: (thisContext object: byteArray basicAt: 2) = 2.
	thisContext object: array basicAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: byteArray basicAt: 2 put: 222.
	self assert: byteArray asArray = #(1 222 3).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array basicAt: 4] raise: Error.
	self should: [thisContext object: byteArray basicAt: 0] raise: Error.
	self should: [thisContext object: byteArray basicAt: 1 put: -1] raise: Error! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'eem 8/Apr/2009, 11:44:00 pm (UTC)'!
testMirrorClass
	| stackpBefore stackpAfter |
	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectClass: Array new) = Array.
	self assert: (thisContext objectClass: 1) = 1 class.
	self assert: (thisContext objectClass: ProtoObject new) = ProtoObject.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 28/Dec/2013, 11:06:00 pm (UTC)'!
testMirrorEqEq
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext object: Array new eqeq: Array new) == false.
	self assert: (thisContext object: Array eqeq: Array) == true.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 28/Dec/2013, 11:06:00 pm (UTC)'!
testMirrorInstVarAt
	| stackpBefore stackpAfter array point |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	array := { 1. 2. 3 }.
	point := Point x: 1 y: 2.
	self assert: (thisContext object: array instVarAt: 1) = 1.
	self assert: (thisContext object: point instVarAt: 2) = 2.
	thisContext object: array instVarAt: 2 put: #two.
	self assert: array = #(1 #two 3).
	thisContext object: point instVarAt: 1 put: 1/2.
	self assert: point = (Point x: 1 / 2 y: 2).
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"
	self should: [thisContext object: array instVarAt: 4] raise: Error.
	self should: [thisContext object: point instVarAt: 3] raise: Error! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 28/Dec/2013, 11:06:00 pm (UTC)'!
testMirrorPerform
	| stackpBefore stackpAfter anInterval |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	anInterval := 1 to: 2.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval) == Array.
	self assert: (thisContext object: anInterval perform:# species withArguments: #() inClass: Interval superclass) == Interval.
	self should: [thisContext object: anInterval perform:# species withArguments: #() inClass: Point]
		raise: Error.
	self should: [thisContext object: anInterval perform:# species withArguments: OrderedCollection new inClass: Interval]
		raise: Error.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter "Make sure primitives pop all their arguments"! !

!MirrorPrimitiveTests methodsFor: 'tests' stamp: 'jmv 5/Jul/2022, 8:01:51 pm (UTC)'!
testMirrorSize
	| stackpBefore stackpAfter |

	Smalltalk isRunningCog ifFalse: [
		^self assert: false description: 'Needs Cog' ].

	stackpBefore := thisContext stackPtr.
	self assert: (thisContext objectSize: #(1 2 3)) = 3.
	self assert: (thisContext objectSize: '123' asByteString) = 3.
	self assert: (thisContext objectSize: nil) = 0.
	self assert: (thisContext objectSize: 1) = 0.
	stackpAfter := thisContext stackPtr.
	self assert: stackpBefore = stackpAfter. "Make sure primitives pop all their arguments"! !

!ParseNodeEnumeratorTest methodsFor: 'tests about enumeration' stamp: 'RNG 28/Sep/2020, 2:25:05 am (UTC)'!
testItEnumeratesAllTheParseNodesPresentInAMethodNode

	| blockNode enumeratedNodes methodNode enumerator tempVarsDeclarationNode tempVarDeclarationNode literalNode messageNode nodesToBeEnumerated returnNode variableNode |
	methodNode := [ Parser parse: 'm1 | arg | ^ arg + 2' class: self class ]
		on: SyntaxErrorNotification 
		do: [ :anError | self fail ].

	enumeratedNodes := Set new.
	enumerator := ParseNodeEnumerator ofBlock: [ :parseNode | enumeratedNodes add: parseNode ].
	methodNode accept: enumerator.
	blockNode := methodNode block.
	tempVarsDeclarationNode := methodNode temporariesDeclaration.
	tempVarDeclarationNode := tempVarsDeclarationNode temporaryDeclarationNodes first.
	returnNode := blockNode statements first.
	messageNode := returnNode expr.
	variableNode := messageNode receiver.
	literalNode := messageNode arguments first.
	nodesToBeEnumerated := {
		methodNode . blockNode . tempVarsDeclarationNode . tempVarDeclarationNode.
		returnNode . messageNode . variableNode . literalNode.
	}.

	self assert: (enumeratedNodes includesAllOf: nodesToBeEnumerated)! !

!ParseNodeTest methodsFor: 'parse nodes helpers' stamp: 'RNG 20/Jan/2018, 1:09:10 am (UTC)'!
message: aSelector to: aVariableName

	| variableNode selectorNode |
	variableNode _ VariableNode named: aVariableName.
	selectorNode _ SelectorNode new key: aSelector code: 34.
	^ MessageNode new
		receiver: variableNode
		selector: selectorNode
		arguments: #()
		precedence: 1! !

!BlockNodeTest methodsFor: 'detecting self recursion' stamp: 'RNG 20/Jan/2018, 1:05:22 am (UTC)'!
testItDoesNotSendsAGivenMessageToSelfWhenTheBlockIsEmpty

	| anEmptyBlock |
	anEmptyBlock _ BlockNode empty.

	self deny: (anEmptyBlock sendsMessageToSelf: #myMethod)! !

!BlockNodeTest methodsFor: 'detecting self recursion' stamp: 'RNG 20/Jan/2018, 1:08:08 am (UTC)'!
testItSendsAGivenMessageToSelfWhenItContainsAnStatementWithAMessageSendToSelf

	| anEmptyBlock |
	anEmptyBlock _ BlockNode withJust: (self message: #myMethod to: 'self').

	self assert: (anEmptyBlock sendsMessageToSelf: #myMethod)! !

!MessageNodeTest methodsFor: 'tests' stamp: 'HAW 10/Dec/2023, 3:06:00 pm (UTC)'!
cascadeMessageSend

	1 printString; yourself! !

!MessageNodeTest methodsFor: 'tests' stamp: 'HAW 10/Dec/2023, 3:05:52 pm (UTC)'!
messageSend

	1 printString! !

!MessageNodeTest methodsFor: 'tests' stamp: 'HAW 10/Dec/2023, 3:06:08 pm (UTC)'!
test01hasEquivalentReceiverWithWorksForAnyCombinationOfMessageSend

	| cascadeMessageSend messageSend |
	
	messageSend := (self class >> #messageSend) methodNode block statements first.
	"I look for the MessageNode in the sourceRanges because there is where the receiver is nil and 
	#hasEquivalentReceiverWith: was not taking care of that case - Hernan"
	cascadeMessageSend := (self class >> #cascadeMessageSend) methodNode completeSourceRanges keys detect: [ :a | a isMessageNode and: [ a selector key = #printString ]].
	
	self assert: (messageSend hasEquivalentReceiverWith: cascadeMessageSend).
	self assert: (cascadeMessageSend hasEquivalentReceiverWith: messageSend).
	self assert: (cascadeMessageSend hasEquivalentReceiverWith: cascadeMessageSend).
	self assert: (messageSend hasEquivalentReceiverWith: messageSend).
	! !

!MessageNodeTest methodsFor: 'detecting self recursion' stamp: 'RNG 20/Jan/2018, 1:02:30 am (UTC)'!
testItDoesNotSendTheMessageToSelfWhenTheReceiverIsNotSelf

	| messageNode |
	messageNode _ self message: #myMethod to: 'super'.

	self deny: (messageNode sendsMessageToSelf: #myMethod)! !

!MessageNodeTest methodsFor: 'detecting self recursion' stamp: 'RNG 20/Jan/2018, 1:02:19 am (UTC)'!
testItDoesNotSendTheMessageToSelfWhenTheReceiverIsSelfButTheSelectorIsDifferent

	| messageNode |
	messageNode _ self message: #myMethod to: 'self'.

	self deny: (messageNode sendsMessageToSelf: #otherMethod)! !

!MessageNodeTest methodsFor: 'detecting self recursion' stamp: 'RNG 20/Jan/2018, 1:01:10 am (UTC)'!
testItSendsTheMessageToSelfWhenTheReceiverIsSelf

	| messageNode |
	messageNode _ self message: #myMethod to: 'self'.

	self assert: (messageNode sendsMessageToSelf: #myMethod)! !

!VariableNodeTest methodsFor: 'references to self' stamp: 'RNG 20/Jan/2018, 12:51:09 am (UTC)'!
testItDoesNotReferencesSelfWhenTheVariableNameIsNotSelf

	| variableNode |
	variableNode _ VariableNode named: 'abc'.

	self deny: variableNode referencesSelf! !

!VariableNodeTest methodsFor: 'references to self' stamp: 'RNG 20/Jan/2018, 12:50:13 am (UTC)'!
testItReferencesSelfWhenTheVariableNameIsSelf

	| variableNode |
	variableNode _ VariableNode named: 'self'.

	self assert: variableNode referencesSelf! !

!ParserTest methodsFor: 'test' stamp: 'jmv 8/Sep/2026, 11:39:46 pm (UTC)'!
testMulticharBinarySelectors
	| result expression |
	self assert: (Compiler evaluate: '3+4') = 7.

	expression := '1@-2'.
	Preferences
		withTemporaryValue: #st80
		of: #atMinusDigitMeaning
		do: [ result := Compiler evaluate: expression].
	self assert: result x = 1.
	self assert: result y = 2 negated.

	Preferences
		withTemporaryValue: #disabled
		of: #atMinusDigitMeaning
		do: [ result := [Compiler evaluate: expression] on: SyntaxErrorNotification do: [#se]].
	self assert: result = #se.

	Preferences
		withTemporaryValue: #ansiSmalltalk
		of: #atMinusDigitMeaning
		do: [ result := [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result = #dnu.

	expression := '1@-2.'.
	Preferences
		withTemporaryValue: #st80
		of: #atMinusDigitMeaning
		do: [ result := [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result isPoint.
	self assert: result x = 1.
	self assert: result y = 2 negated.

	Preferences
		withTemporaryValue: #disabled
		of: #atMinusDigitMeaning
		do: [ result := [Compiler evaluate: expression] on: SyntaxErrorNotification do: [#se]].
	self assert: result = #se.

	Preferences
		withTemporaryValue: #ansiSmalltalk
		of: #atMinusDigitMeaning
		do: [ result := [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result = #dnu.
! !

!ParserTest methodsFor: 'test' stamp: 'HAW 20/Sep/2025, 10:00:18 pm (UTC)'!
testOptimizedNodesWithNilDoNotFailsWhenAskForEquivalentTo

	| methodNode |
	
	methodNode := self class methodNodeFor: 
	'example
	
	2 to: 4 do: [:i | self at: i].
	^ self'.
	
	self 
		shouldnt: [ methodNode equivalentTo: methodNode ] 
		raise: Error
	! !

!ParserTest methodsFor: 'test' stamp: 'jmv 31/Jul/2023, 4:17:49 pm (UTC)'!
testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat

	| methodNode ranges |
	
	"See Parser>>#variable"
	
	methodNode := self class methodNodeFor: 'm1 |v| ^v'.
	
	ranges := methodNode rangeForNode: methodNode tempNodes first ifAbsent: [ self fail ].
	
	self assert: ranges size equals: 2.
	self assert: ranges includes: (5 to: 5).
	self assert: ranges includes: (9 to: 9).
	
	
	
	! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 13/Mar/2020, 4:39:16 am (UTC)'!
testItGeneratesATempDeclarationNodeForEachTempInABlock

	| methodNode blockNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 [ | a b c | 42 ]'.
	blockNode := methodNode block statements first.
	tempsDeclarationNode := blockNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 13/Mar/2020, 4:38:08 am (UTC)'!
testItGeneratesATempDeclarationNodeForEachTempInAMethod

	| methodNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 | a b c | ^a + b + c'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.! !

!ParserTest methodsFor: 'temporary variables tests' stamp: 'RNG 13/Mar/2020, 4:34:19 am (UTC)'!
testItGeneratesAnEmptyTempsDeclarationNodeForAMethodWithoutTemps

	| methodNode tempsDeclarationNode |
	methodNode := self class methodNodeFor: 'm1 ^42'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	
	self assert: tempsDeclarationNode allDeclaredVariableNodes isEmpty.! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:31:38 pm (UTC)'!
test1
	"
	self new test1
	"
	| prettyPrinted source |

	source := 
'sample1: x
	x with: [
		self print.
		self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:31:45 pm (UTC)'!
test2
	"
	self new test2
	"
	| prettyPrinted source |

	source := 
'sample2: x
	x
		print;
		with: [ :a :b :c | | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:31:52 pm (UTC)'!
test3
	"
	self new test3
	"
	| prettyPrinted source |

	source := 
'sample3: x
	x
		print;
		with: [ :a :b :c |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:32:07 pm (UTC)'!
test4
	"
	self new test4
	"
	| prettyPrinted source |

	source := 
'sample4: x
	x
		print;
		with: [ | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 11/May/2026, 3:56:11 pm (UTC)'!
test5
	"
	self new test5
	"
	| prettyPrinted source |

	source := 
'sample5: x
	x
		print;
		print;
		print;
		with: [
			self print.
			self print ]
		do: [
			self print.
			self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 31/Jul/2023, 3:32:18 pm (UTC)'!
test6
	"
	self new test6
	"
	| prettyPrinted source |

	source := 
'sample6: x
	x
		with: [
			self print.
			self print ]
		do: [
			self print.
			self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 1/Jun/2026, 11:41:01 pm (UTC)'!
test7
	"
	self new test7
	"
	| prettyPrinted source |

	source := 
'sample7: x
	x with: [ self print ] do: [ self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!PrettyPrintingTest methodsFor: 'testing' stamp: 'jmv 11/May/2026, 3:57:09 pm (UTC)'!
test8
	"
	self new test8
	"
	| prettyPrinted source |

	source := 
'sample8: x
	x
		print;
		with: [ self print ] do: [ self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 19/Oct/2020, 8:07:48 pm (UTC)'!
test01isImplicitSelfReturnInReturnsTrueWithMethodWithOutAnySourceCode

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithImplicitReturn) methodNode.
	returnNode := methodNode block statements first.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 19/Oct/2020, 8:08:15 pm (UTC)'!
test02isImplicitSelfReturnInReturnsTrueWithMethodReferencingSelf

	| methodNode returnNode |
	
	methodNode := (self class >> #methodReferencingSelfWithImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'tests' stamp: 'HAW 19/Oct/2020, 8:09:20 pm (UTC)'!
test03isImplicitSelfReturnInReturnsFalseWithMethodReturningSelf

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithoutImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self deny: (returnNode isImplicitSelfReturnIn: methodNode)! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 19/Oct/2020, 8:06:04 pm (UTC)'!
methodReferencingSelfWithImplicitReturn

	self yourself 
	
	! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 19/Oct/2020, 8:00:38 pm (UTC)'!
methodWithImplicitReturn
	
	! !

!ReturnNodeTest methodsFor: 'test data' stamp: 'HAW 19/Oct/2020, 8:08:38 pm (UTC)'!
methodWithoutImplicitReturn
	
	^self! !

!ReturnNodeTest methodsFor: 'test support' stamp: 'HAW 19/Oct/2020, 8:03:45 pm (UTC)'!
returnNodeOf: methodNode
	
	methodNode nodesDo: [:node | node isReturn ifTrue: [^node]].
	
	self error: 'No return node found'! !

!ScannerTest methodsFor: 'testing' stamp: 'jmv 12/May/2025, 1:44:28 pm (UTC)'!
testLiteralSymbols

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | Symbol isLiteralSymbol: (String with: char) asSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | Symbol isLiteralSymbol: str asSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
		
	((32 to: 94), (96 to: 126) collect: [:ascii | Character codePoint: ascii]) ,
	#(':x:yourself' '::' 'x:yourself' '123' 'x0:x1:x2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '-' '++' '+' '+/-' '-/+' '<|>' '#x' '()' '[]' '{}' '')
		do: [:str |
			self assert: (Compiler evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion']! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:18:50 pm (UTC)'!
test01TryingToTrimAnAlreadyTrimmedIntervalThatRepresentsASmalltalkExpressionDoesNotChangeTheOriginalInterval
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '3+4'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: originalInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:19:19 pm (UTC)'!
test02StartingAndEndingSeparatorsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '	3+4 '.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 4) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:19:27 pm (UTC)'!
test03StartingAndEndingDotsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '...3+4..'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (4 to: 6) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:19:38 pm (UTC)'!
test04GroupsOfParenthesesAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := '((3+4))'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (3 to: 5) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:19:58 pm (UTC)'!
test05GroupsOfParenthesesIncludingSeparatorsAreTrimmed
	
	| originalInterval trimmedInterval sourceCode |
	sourceCode := ' (	(3+4)
	)'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (5 to: 7) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:23:09 pm (UTC)'!
test23TrimmsExternalParentesisWhenMoreParentesisAtDifferentCollaborations
	
	| originalInterval trimmedInterval sourceCode |
	
	sourceCode := '((self m2) + (self m3))'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 22) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - trimming' stamp: 'jmv 31/Jul/2023, 4:23:24 pm (UTC)'!
test24TrimmsExternalParentesisWhenMoreParentesisAtDifferentCollaborationsWithOtherSeparators
	
	| originalInterval trimmedInterval sourceCode |
	
	sourceCode := '((self m2) + (self m3)
	)'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	trimmedInterval := originalInterval trimToMatchExpressionOn: sourceCode.

	self assert: trimmedInterval equals: (2 to: 22) asSourceCodeInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - equality' stamp: 'RNG 28/May/2020, 3:59:37 am (UTC)'!
test11AsSourceCodeIntervalMessageDoesNotCreateANewSourceCodeIntervalInstance

	| interval |
	interval := (1 to: 10) asSourceCodeInterval.
	
	self assert: interval == interval asSourceCodeInterval! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'jmv 31/Jul/2023, 4:20:31 pm (UTC)'!
test06AnIntervalThatDoesNotHaveCharactersToExpandRemainsTheSame

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3+4 factorial'.
	originalInterval := (1 to: sourceCode size) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: originalInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'jmv 31/Jul/2023, 4:20:48 pm (UTC)'!
test07AnIntervalEnclosedByParenthesesCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '(3+4 factorial)'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'jmv 31/Jul/2023, 4:20:58 pm (UTC)'!
test08AnIntervalWithParenthesesOnJustOneSideCannotBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '3 + 4 factorial)'.
	originalInterval := (1 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: originalInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'jmv 31/Jul/2023, 4:21:07 pm (UTC)'!
test09AnIntervalEnclosedByBackticksCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`3 + 4 factorial`'.
	originalInterval := (2 to: sourceCode size - 1) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'jmv 31/Jul/2023, 4:21:16 pm (UTC)'!
test10AnIntervalEnclosedMultipleBackticksAndParenthesesCanBeExpanded

	| sourceCode expandedInterval originalInterval |
	sourceCode := '`((3 + 4 factorial))`'.
	originalInterval := (4 to: sourceCode size - 3) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).! !

!SourceCodeIntervalTest methodsFor: 'tests - expanding' stamp: 'HAW 18/May/2025, 3:17:29 pm (UTC)'!
test25CanExpandMultipleParentesisWithSeparatorsInTheMiddle

	| sourceCode expandedInterval originalInterval |
	
	sourceCode := '` ( ( 3 + 4 factorial)     )     `'.
	originalInterval := (7 to: 21) asSourceCodeInterval.
	expandedInterval := originalInterval expandToMatchExpressionOn: sourceCode.

	self assert: expandedInterval equals: (1 to: sourceCode size).! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:21:27 pm (UTC)'!
test12AnIntervalEndingOnDotExpandsUntilStartOfNextStatementOnSameLine

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 6) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to: 7) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:21:42 pm (UTC)'!
test13AnIntervalEndingBeforeDotExpandsUntilStartOfNextStatementOnSameLine

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 5) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to: 7) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:21:50 pm (UTC)'!
test14AnIntervalEndingOnEndOfTemporariesDeclarationExpandsUntilStartOfNextStatement

	| interval sourceCode expectedExpandedInterval expandedInterval |
	
	sourceCode := '| a  b | 
		1 + 2. 3 + 4'.
	interval := (1 to: 8) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedExpandedInterval := (1 to:12) asSourceCodeInterval.
	self assert: expandedInterval equals: expectedExpandedInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:00 pm (UTC)'!
test15AnIntervalEndingOnLastStatemenWithoutDottIsNotExpanded

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 
		^3 + 4'.
	interval := (1 to: 16) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:11 pm (UTC)'!
test16AnIntervalEndingOnLastStatemenWithDotIsNotExpanded

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 
		^3 + 4.'.
	interval := (1 to: 17) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'HAW 12/Jun/2022, 12:09:35 am (UTC)'!
test17AnIntervalEndingBeforeStatementEndCannotBeExpanded

	| interval sourceCode |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 3) asSourceCodeInterval.
	self should: [interval expandUntilStartOfNextStatementOn: sourceCode.]
		raise: Error 
		withMessageText: [SourceCodeInterval canNotExpandIncompleteStatementError].! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'HAW 12/Jun/2022, 12:09:38 am (UTC)'!
test18AnIntervalEndingAfterLastStatementCannotBeExpanded

	| interval sourceCode |
	
	sourceCode := '| a  b | 
		1 + 2. 3 + 4'.
	interval := (1 to: 20) asSourceCodeInterval.
	self should: [interval expandUntilStartOfNextStatementOn: sourceCode.]
		raise: Error 
		withMessageText: [SourceCodeInterval canNotExpandIncompleteStatementError].! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:22 pm (UTC)'!
test19AnIntervalEndingAfterSourceCodeEndIsNotExpanded

	| interval sourceCode expandedInterval |
	
	sourceCode := '1 + 2. 3 + 4'.
	interval := (1 to: 1000) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	
	self assert: expandedInterval equals: interval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:30 pm (UTC)'!
test20AnIntervalEndingExpandsUntilStartOfNextStatementOnNextLine

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2.
		3 + 4'.
	interval := (1 to: 6) asSourceCodeInterval.
	expandedInterval := interval expandUntilStartOfNextStatementOn: sourceCode.
	expectedInterval := (1 to: 9) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:40 pm (UTC)'!
test21ExpandIntervalToEndOfStatement

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2. ^2'.
	interval := (1 to: 2) asSourceCodeInterval.
	expandedInterval := interval expandUntilStatementEndOn: sourceCode.
	expectedInterval := (1 to: 6) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.! !

!SourceCodeIntervalTest methodsFor: 'tests - testing' stamp: 'jmv 31/Jul/2023, 4:22:54 pm (UTC)'!
test22ExpandIntervalToEndOfStatementWhenNoDotIsPresent

	| interval sourceCode expandedInterval expectedInterval |
	
	sourceCode := '1 + 2'.
	interval := (1 to: 2) asSourceCodeInterval.
	expandedInterval := interval expandUntilStatementEndOn: sourceCode.
	expectedInterval := (1 to: 5) asSourceCodeInterval.
	
	self assert: expandedInterval equals: expectedInterval.! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 17/Dec/2019, 3:00:59 pm (UTC)'!
correctFrom: start to: end with: aReplacement
 
	| newSourceCode |
	
	newSourceCode := sourceCode first: start - 1.
	newSourceCode := newSourceCode, aReplacement.
	newSourceCode := newSourceCode, (sourceCode copyFrom: end + 1 to: sourceCode size).
	
	sourceCode := newSourceCode ! !

!CompilerTest methodsFor: 'emulating' stamp: 'cwp 26/Aug/2009, 12:23:00 am (UTC)'!
selectFrom: start to: end 
	! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 18/Jun/2020, 7:27:24 pm (UTC)'!
selectInvisiblyFrom: start to: stop 
! !

!CompilerTest methodsFor: 'emulating' stamp: 'cwp 26/Aug/2009, 12:22:00 am (UTC)'!
selectionInterval
	^ 1 to: 0! !

!CompilerTest methodsFor: 'emulating' stamp: 'HAW 17/Dec/2019, 2:53:27 pm (UTC)'!
text

	^ sourceCode ! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 7:44:13 pm (UTC)'!
testCanNotDefineVarsInsideBackTick

	self 
		should: [ self class compile: 'm1 `|v1| v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 8:08:17 pm (UTC)'!
testCanNotHaveMoreThanOneExpressionInsideBackTick

	self 
		should: [ self class compile: 'm1 `1 factorial. 1 factorial`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'backtick expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 8:09:28 pm (UTC)'!
testCanNotHaveReturnInsideBackTick

	self 
		should: [ self class compile: 'm1 `^10`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'expression expected') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 7:41:08 pm (UTC)'!
testCanNotReferenceSelfInsideBackTick

	self 
		should: [ self class compile: 'm1 `self`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'self')) ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 7:41:13 pm (UTC)'!
testCanNotReferenceSuperInsideBackTick

	self 
		should: [ self class compile: 'm1 `super`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'super')) ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 7:45:04 pm (UTC)'!
testCanNotReferenceTempVarsInsideBackTick

	self 
		should: [ self class compile: 'm1 |v1| `v1 := 1`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 'Can not evaluate code') ]! !

!CompilerTest methodsFor: 'test - backtick' stamp: 'HAW 23/Jun/2020, 7:41:18 pm (UTC)'!
testCanNotReferenceThisContextInsideBackTick

	self 
		should: [ self class compile: 'm1 `thisContext`' ]
		raise: SyntaxErrorNotification 
		withExceptionDo: [ :anError |
			self assert: (anError messageText includesSubString: 
				(BacktickNode canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: 'thisContext')) ]! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:31:13 pm (UTC)'!
testAddsMoreThanOneUndeclaredVariableCorrectlyInBlock
	
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ var1 := 1. goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ | goo var1 | var1 := 1. goo := 1. ^goo + var1 ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:31:30 pm (UTC)'!
testAddsUndeclaredVariableCorrectlyInBlockWhenNoSpaceBetweenLastOneAndPipe

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|var1| var1 := 1.goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [|var1 goo | var1 := 1.goo := 1.^ goo + var1]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:31:41 pm (UTC)'!
testAddsUndeclaredVariableCorrectlyInBlockWhenTheresAnArgumentAlready

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:31:50 pm (UTC)'!
testAddsUndeclaredVariableCorrectlyInBlockWhenTheresArgumentsAndTempDeclarationPipesWithNoTemps

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | | goo := 1.^ goo + var1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | goo | goo := 1.^ goo + var1]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:31:59 pm (UTC)'!
testAddsUndeclaredVariableCorrectlyInMethodAndInBlock

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle var1 := 1. [ goo := 1. ^goo + var1 ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle | var1 |', String newLineString, 'var1 := 1. [ | goo | goo := 1. ^goo + var1 ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:32:08 pm (UTC)'!
testAddsUndeclaredVariableCorrectlyInMethodWhenNoSpaceBetweenLastOneAndPipe

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle |var1| var1 := 1.goo := 1.^ goo + var1'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle |var1 goo | var1 := 1.goo := 1.^ goo + var1'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:32:20 pm (UTC)'!
testAddsUndeclaredVariableInRightBlock
	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [|goo| goo := 1. ^goo ]. [goo1:=1.^ goo1]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [|goo| goo := 1. ^goo ]. [ | goo1 |goo1:=1.^ goo1]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:32:29 pm (UTC)'!
testAddsUndeclaredVariableWithAnArgumentAndTempAlreadyInBlock

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ :var1 | | foo | foo := 2. goo := 1.^ goo + var1 + foo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ :var1 | | foo goo | foo := 2. goo := 1.^ goo + var1 + foo ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:32:42 pm (UTC)'!
testDoesNotConfusesVariablesWithSameNameOfDifferentBlocksWhenRemoving

	| testClass counter |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]. [ | goo | ]'.
	counter := 0.
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			counter := counter + 1.
			unusedVariable resume: counter = 1 ].
		
	self assert: sourceCode equals: 'griffle [  ]. [ | goo | ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:32:51 pm (UTC)'!
testDoesNotRemoveUnusedVariableIfSaidSo

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: false ].
		
	self assert: sourceCode equals: 'griffle | goo |'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:33:03 pm (UTC)'!
testKeepsVariableDefinitionWhenThereAreUsedTemporaries

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo a | a := 1. ^a'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle | a | a := 1. ^a'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:33:11 pm (UTC)'!
testRemovesUnusedVariablesFromBlocks

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ | goo | ]'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle [  ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:33:20 pm (UTC)'!
testRemovesVariableDefinitionWhenNoMoreUnusedTemporaries

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	[testClass compile: sourceCode notifying: self]
		on: UnusedVariable
		do: [ :unusedVariable |
			self assert: 'goo' equals: unusedVariable name.
			unusedVariable resume: true ].
		
	self assert: sourceCode equals: 'griffle '.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:33:31 pm (UTC)'!
testReusesBlockTempDeclarationWhenEmptyForUndeclaredVariable

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle [ || goo := 1.^ goo ]'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareBlockTempAction ].
		
	self assert: sourceCode equals: 'griffle [ | goo | goo := 1.^ goo ]'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:33:42 pm (UTC)'!
testReusesMethodTempDeclarationWhenEmptyForUndeclaredVariable

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle || goo := 1.^ goo'.
	
	[ testClass compile: sourceCode notifying: self ]
		on: UndeclaredVariable
		do: [ :anUndeclaredVariable |
			anUndeclaredVariable resume: anUndeclaredVariable declareMethodTempAction ].
		
	self assert: sourceCode equals: 'griffle | goo | goo := 1.^ goo'.! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:34:10 pm (UTC)'!
testUndeclaredVariableIsSignaledForUndeclaredVariables

	| testClass |
	
	testClass := self createTestClass.
	
	self 
		should:  [ testClass compile: 'griffle ^ goo' notifying: self ]
		raise: UndeclaredVariable
		withExceptionDo: [ :anUndeclaredVariable |
			self assert: anUndeclaredVariable name equals: 'goo' ].! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:34:37 pm (UTC)'!
testUndefinedVariableIsSignaledForVariablesDefinedButNotInitialized

	| testClass |
	
	testClass := self createTestClass.

	self 
		should: [testClass compile: 'griffle | goo | ^ goo' notifying: self]
		raise: UndefinedVariable
		withExceptionDo: [ :anUndefinedVariable |
			self assert: anUndefinedVariable name equals: 'goo' ].! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:34:56 pm (UTC)'!
testUnknownSelectorIsSignalForMessagesSendNotImplemented

	| testClass unknowSelector |
	
	testClass := self createTestClass.
	unknowSelector := 'reallyHopeThisIsntImplementedAnywhere'.
	
	self 
		should: [ testClass
				compile: 'griffle self ', unknowSelector
				notifying: self]
		raise: UnknownSelector
		withExceptionDo: [ :anUnknowSelector |
			self assert: anUnknowSelector name equals: unknowSelector ].! !

!CompilerTest methodsFor: 'tests - exceptions' stamp: 'jmv 31/Jul/2023, 4:35:16 pm (UTC)'!
testUnusedVariableIsSignaledForUnusedVariables

	| testClass |
	
	testClass := self createTestClass.
	sourceCode := 'griffle | goo |'.
	
	self 
		should: [testClass compile: sourceCode notifying: self]
		raise: UnusedVariable
		withExceptionDo: [ :unusedVariable |
			self assert: unusedVariable name equals: 'goo' ].! !

!CompilerTest methodsFor: 'private' stamp: 'HAW 17/Dec/2019, 2:38:10 pm (UTC)'!
createTestClass
	
	^self createClassNamed: #CompilerTestDataClass__! !

!CompilerTest methodsFor: 'private' stamp: 'cwp 26/Aug/2009, 12:28:00 am (UTC)'!
unusedVariableSource
	^ 'griffle 
		| goo |
		^ nil'! !

!CompilerTest methodsFor: 'tests - limits' stamp: 'eem 28/Dec/2024, 5:58:04 pm (UTC)'!
testMaxLiterals
	"Document the maximum number of literals in a compiled method"

	| bytecodeSetEncoder maxLiterals stringThatCanBeCompiled stringWithOneTooManyLiterals |
	"Why 6?  It's rather implementation dependent.  But the {... construct is compiled as
		(Array braceStream: size)
			nextPut: expr;
			...;
			braceArray
	 where nextPut: is a special selector.  So one each for Array binding, #braceStream, #braceArray and possibly the size,
	 one for the selector and one for the methodClass makes 5 or 6."
	bytecodeSetEncoder := CompiledMethod preferredBytecodeSetEncoderClass new.
	maxLiterals := bytecodeSetEncoder maxNumLiterals - 6.
	"If the bytecode set can push the size of the brace stream with a bytecode (not needing a literal) then we need one more literal."
	(bytecodeSetEncoder isSpecialLiteralForPush: maxLiterals) ifTrue:
		[maxLiterals := maxLiterals + 1].
	stringThatCanBeCompiled := '{ ', (String streamContents: [:strm |
					1 to: maxLiterals do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	stringWithOneTooManyLiterals := '{ ', (String streamContents: [:strm |
					1 to: maxLiterals + 1 do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	self assert: ((1 to: maxLiterals) collect: #printString) equals: (Compiler evaluate: stringThatCanBeCompiled).
	
	"If the following test fails, it means that the limit has been raised or eliminated,
	and this test should be updated to reflect the improvement."
	self should: [Compiler evaluate: stringWithOneTooManyLiterals for: nil logged: false] raise: SyntaxErrorNotification.
! !

!CompilerTest methodsFor: 'tests - limits' stamp: 'eem 28/Dec/2024, 5:57:19 pm (UTC)'!
testMaxLiteralsWithClassReferenceInClosure
	"Document the maximum number of literals in a compiled method. A class
	reference in a closure reduces the maximum literals."

	| bytecodeSetEncoder maxLiterals stringThatCanBeCompiled stringWithOneTooManyLiterals |
	bytecodeSetEncoder := CompiledMethod preferredBytecodeSetEncoderClass new.
	maxLiterals := bytecodeSetEncoder maxNumLiterals
					- 2 "for selector & methodClass"
					- (bytecodeSetEncoder supportsFullBlocks ifTrue: [1] ifFalse: [5 "For this example. See below"])
					- 4 "for the implicit stream processing into which the brace construct is compiled: (Array braceStream: size) braceArray".
	"If the bytecode set can push the size of the brace stream with a bytecode (not needing a literal) then we need one more literal."
	(bytecodeSetEncoder isSpecialLiteralForPush: maxLiterals) ifTrue:
		[maxLiterals := maxLiterals + 1].
	stringThatCanBeCompiled := '[ DateAndTime now. Date today. Time ]. { ',
			(String streamContents: [:strm |
					1 to: maxLiterals do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	stringWithOneTooManyLiterals := '[ DateAndTime now. Date today. Time ]. { ',
			(String streamContents: [:strm |
					1 to: maxLiterals + 1 do: [:e | strm nextPutAll: '''', e asString, '''', ' . ']]), '}'.
	self assert: maxLiterals equals: (Compiler evaluate: stringThatCanBeCompiled) size.
	
	"If the following test fails, it means that the limit has been raised or eliminated,
	and this test should be updated to reflect the improvement."
	self should: [Compiler evaluate: stringWithOneTooManyLiterals for: nil logged: false] raise: SyntaxErrorNotification.
! !

!PreferenceSet methodsFor: '*CompilerTests' stamp: 'jmv 16/May/2022, 12:57:42 pm (UTC)'!
withTemporaryValue: temporaryValue of: aPreferenceName do: aBlock
	"Allows to execute a piece of code with a temporary value of the given preference. Useful for testing purposes."
	
	| currentValue |
	currentValue := self at: aPreferenceName.
	self at: aPreferenceName put: temporaryValue.
	[ aBlock value ] ensure: [ self at: aPreferenceName put: currentValue ]! !
