'From Cuis7.3 [latest update: #7005] on 25 January 2025 at 10:32:22 am'!
!classDefinition: #ImplicitLiteralFinder category: #'Kernel-Methods'!
InstructionClient subclass: #ImplicitLiteralFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!ImplicitLiteralFinder commentStamp: 'eem 9/12/2021 14:44' prior: 0!
ImplicitLiteralFinder is a means to extract special literals from an instruction stream.  Instances of InstructionClient answer self in response to all bytecode decode messages.  Instances of this class answer the literal itself for implicit literal bytecode messages.

Instance Variables
!


!Object methodsFor: 'literals' stamp: 'mt 12/15/2023 12:17'!
allLiteralsDo: aBlock
	"Enumerate all literals in the receiver. Note that subclasses may override this to expose the literals they hold such as Array or Pragma. Also note that CompiledCode, being the typical entry point for such an operation, will rather call #allLiteralsDo:#scanForSpecial: on all code literals (and regular literals) to avoid expensive byte-code scanning if possible."

	self flag: #discuss. "mt: Better implement this only on Symbol, Array, Number, Binding, etc. instead of Object?"
	aBlock value: self.! !

!Object methodsFor: 'literals' stamp: 'mt 12/15/2023 10:22'!
allLiteralsDo: aBlock scanForSpecial: aBoolean
	"Backstop. Avoid bytecode scanning in CompiledCode and below. See #hasLiteral:scanForSpecial:."
	
	self allLiteralsDo: aBlock.! !

!Object methodsFor: 'literals' stamp: 'eem 7/8/2024 15:25'!
hasLiteral: literal

	^ self
		hasLiteralSuchThat: [:lit | lit literalEqual: literal]
		scanForSpecial: false! !

!Object methodsFor: 'literals' stamp: 'mt 12/15/2023 12:13'!
hasLiteralSuchThat: testBlock
	"Also see #hasLiteral:. Do not scan for implicit (i.e. special) literals by default to get a faster response."

	^ self hasLiteralSuchThat: testBlock scanForSpecial: false! !

!Object methodsFor: 'literals' stamp: 'mt 12/15/2023 10:23'!
hasLiteralSuchThat: testBlock scanForSpecial: aBoolean
	"Backstop. Avoid bytecode scanning in CompiledCode and below. See #hasLiteral:scanForSpecial:."

	self
		allLiteralsDo: [:literal | (testBlock value: literal) ifTrue: [^ true]]
		scanForSpecial: aBoolean.
	
	^ false! !


!Array methodsFor: '*Kernel-literals' stamp: 'mt 6/24/2019 09:25'!
allLiteralsDo: aBlock

	"I am a literal."
	aBlock value: self.
	
	"All my elements are literals."
	self do: [:each | each allLiteralsDo: aBlock].! !


!CompiledMethod methodsFor: 'literals' stamp: 'mt 12/15/2023 10:52'!
allImplicitLiteralsDo: aBlock

	self codeLiteralsDo: [:compiledCode |
		compiledCode implicitLiteralsDo: aBlock].! !

!CompiledMethod methodsFor: 'literals' stamp: 'mt 12/15/2023 10:29'!
allLiteralsDo: aBlock
	"Overwritten to really enumerate all literals. Also the implicit ones."
	
	self
		allLiteralsDo: aBlock
		scanForSpecial: true.! !

!CompiledMethod methodsFor: 'literals' stamp: 'mt 12/15/2023 10:58'!
allLiteralsDo: aBlock scanForSpecial: aBoolean
	"Overwritten to skip certain (raw) literals."
		
	"Exclude method selector (or properties) and the method's class. Include code literals."
	1 to: self numLiterals - 2 do: [:index |
		(self literalAt: index) allLiteralsDo: aBlock scanForSpecial: aBoolean].

	"Enumerate the implicit literals in bytecodes of the receiver."
	aBoolean ifTrue: [self implicitLiteralsDo: aBlock].

	"Enumerate method selector only through additional method state."
	self penultimateLiteral isMethodProperties
		ifTrue: [self penultimateLiteral allLiteralsDo: aBlock scanForSpecial: aBoolean]! !

!CompiledMethod methodsFor: 'literals' stamp: 'mt 7/4/2019 08:46'!
codeLiterals

	^ Array streamContents: [:stream |
		self codeLiteralsDo: [:compiledCode | stream nextPut: compiledCode]]! !

!CompiledMethod methodsFor: 'literals' stamp: 'mt 7/4/2019 08:37'!
codeLiteralsDo: aBlock
	"Enumerate all literals that represent instances of CompiledCode. This is especially required for SistaV1."
	
	aBlock value: self.
	
	self literalsDo: [:literal | literal isCompiledCode ifTrue: [
		literal codeLiteralsDo: aBlock]].! !

!CompiledMethod methodsFor: 'literals' stamp: 'eem 9/12/2021 15:03'!
implicitLiteralsDo: aBlock
	"Enumerate the implicit literals in bytecodes of the receiver."
	
	| stream client encoderClass endPC |
	"Enumerate special selectors & special literals such as true and false."
	stream := InstructionStream on: self.
	encoderClass := self encoderClass.
	"cache endPC for methods with embedded source; finding out the endPC is very slow in this case..."
	endPC := self endPC.
	client := ImplicitLiteralFinder new.
	[stream pc <= endPC] whileTrue:
		[| literalOrClient |
		literalOrClient := encoderClass interpretNextInstructionFor: client in: stream.
		literalOrClient ~~ client ifTrue:
			[aBlock value: literalOrClient]]! !


!AdditionalMethodState methodsFor: 'literals' stamp: 'mt 6/5/2019 11:12'!
allLiteralsDo: aBlock

	1 to: self basicSize do: [:index |
		(self basicAt: index) allLiteralsDo: aBlock].! !


!ImplicitLiteralFinder methodsFor: 'instruction decoding - implicit literals' stamp: 'mt 12/20/2023 13:19'!
methodReturnSpecialConstant: value
	^value! !

!ImplicitLiteralFinder methodsFor: 'instruction decoding - implicit literals' stamp: 'eem 9/12/2021 14:45'!
pushSpecialConstant: value
	^value! !

!ImplicitLiteralFinder methodsFor: 'instruction decoding - implicit literals' stamp: 'eem 9/12/2021 14:45'!
sendSpecial: selector numArgs: numArgs
	^selector! !


!Pragma methodsFor: 'literals' stamp: 'mt 6/5/2019 10:59'!
allLiteralsDo: aBlock

	keyword allLiteralsDo: aBlock.
	arguments allLiteralsDo: aBlock.! !


!LookupKey methodsFor: '*Kernel-literals' stamp: 'mt 7/22/2019 09:40'!
allLiteralsDo: aBlock
	"Treat myself or my key as literal. Do not treat any value (i.e., in Association or Binding) as literal to avoid decending too thoroughly. As a consequence, for example, classes have to be resolved to their class bindings or class names before using them as input in this interface."
	
	aBlock value: self.	
	self key allLiteralsDo: aBlock.! !


!CompiledMethod methodsFor: 'literals' stamp: 'mt 7/4/2019 09:27'!
allLiterals
	"Skip compiled-code objects. Keep literal arrays, bindings, etc."
	
	^ Array streamContents: [:result |
		self allLiteralsDo: [:literal | result nextPut: literal]]! !

!CompiledMethod methodsFor: 'literals' stamp: 'eem 7/8/2024 15:15'!
hasLiteral: aLiteral
	"Overwrite this method to invoke the bytecode encoder scanner explicitly.
	 This might be removed if there would be a way to enumerate special literals in a compiled-code object. See #allLiteralsDo:."

	^ self
		hasLiteralSuchThat: [:lit | lit literalEqual: aLiteral]
		scanForSpecial: (self encoderClass canBeSpecialLiteral: aLiteral)! !

!CompiledMethod methodsFor: 'literals' stamp: 'mt 5/28/2019 14:08'!
literals
	
	^ Array streamContents: [:result |
		self literalsDo: [:lit | result nextPut: lit]]! !


!Pragma methodsFor: 'literals' stamp: 'mt 5/28/2019 12:34'!
hasLiteral: aLiteral
	"Extra support for #hasLiteralTest:."
	
	^ (super hasLiteral: aLiteral)
		or: [keyword == #hasLiteralTest: and: [
			self methodClass theNonMetaClass perform: arguments first with: aLiteral]]! !

!methodRemoval: Array #hasLiteral: stamp: 'eem 1/25/2025 10:29:22'!
Array removeSelector: #hasLiteral:!

!Pragma reorganize!
('accessing-method' method methodClass selector)
('accessing-pragma' argumentAt: arguments key keyword message numArgs)
('initialization' setArguments: setKeyword: setMethod:)
('printing' printOn:)
('testing' hasLiteralSuchThat: is:)
('comparing' = analogousCodeTo: hash)
('literals' allLiteralsDo: hasLiteral:)
!

