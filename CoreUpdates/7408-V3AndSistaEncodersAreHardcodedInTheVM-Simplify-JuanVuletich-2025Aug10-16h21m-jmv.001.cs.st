'From Cuis7.5 [latest update: #7399] on 10 August 2025 at 4:28:19 pm'!
!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 8/10/2025 16:24:22'!
encoderClass
	"Answer the encoder class that encoded the bytecodes in this method.
	 The sign flag bit is used by the VM to select a bytecode set.  This formulation
	 may seem odd but this has to be fast, so no property probe unless needed."

	^self header >= 0
		ifTrue: 
			[ EncoderForV3PlusClosures ]
		ifFalse:
			[ EncoderForSistaV1 ]! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/10/2025 16:26:55'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PreferredBytecodeSetEncoderClass ifNil:
		[PreferredBytecodeSetEncoderClass := EncoderForV3PlusClosures]! !

!CompiledMethod class methodsFor: 'method encoding' stamp: 'jmv 8/10/2025 16:24:28'!
headerFlagForEncoderClass: anEncoderClass
	
	(anEncoderClass includesBehavior: EncoderForV3PlusClosures) ifTrue: [^0].
	(anEncoderClass includesBehavior: EncoderForSistaV1) ifTrue: [^SmallInteger minVal].
	
	self error: 'The encoder is not one of the two installed bytecode sets'! !

!CompiledMethod class methodsFor: 'preferences' stamp: 'jmv 8/10/2025 16:27:45'!
preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass
	"Set the class that determines the bytecode set used to compile methods with.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self assert: (aBytecodeEncoderSubclass includesBehavior: BytecodeEncoder).
	(aBytecodeEncoderSubclass == EncoderForV3PlusClosures
	 or: [aBytecodeEncoderSubclass == EncoderForSistaV1]) ifTrue:
		[PreferredBytecodeSetEncoderClass := aBytecodeEncoderSubclass.
		 ^self].
	self error: 'Cannot set preferred bytecode set.  Both of the current sets appear to be in use.'! !


!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 8/10/2025 16:25:32'!
browseMethodsWithEmptyClosures
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| pattern1 pattern2 |
	pattern1 := EncoderForV3PlusClosures emptyClosurePattern.
	pattern2 := EncoderForSistaV1 emptyClosurePattern.
	self
		browseAllSelect: [ :m | | s |
			s := InstructionStream on: m.
			s scanFor: [ :bytecode |
				(s method is: pattern1 subcollectionAt: s pc) or:
					[s method is: pattern2 subcollectionAt: s pc]]]
		name:  'Methods with empty closures'
		autoHighlight: '[]'! !

!methodRemoval: CompiledMethod class #installSecondaryBytecodeSet: stamp: 'jmv 8/10/2025 16:26:41'!
CompiledMethod class removeSelector: #installSecondaryBytecodeSet:!
!methodRemoval: CompiledMethod class #primaryBytecodeSetEncoderClass stamp: 'jmv 8/10/2025 16:25:38'!
CompiledMethod class removeSelector: #primaryBytecodeSetEncoderClass!
!methodRemoval: CompiledMethod class #secondaryBytecodeSetEncoderClass stamp: 'jmv 8/10/2025 16:25:35'!
CompiledMethod class removeSelector: #secondaryBytecodeSetEncoderClass!
!methodRemoval: CompiledMethod class #installPrimaryBytecodeSet: stamp: 'jmv 8/10/2025 16:26:38'!
CompiledMethod class removeSelector: #installPrimaryBytecodeSet:!
!classDefinition: #CompiledMethod category: #'MinimalKernel-Methods'!
ByteArray variableByteSubclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: 'LargeFrame PreferredBytecodeSetEncoderClass SmallFrame'
	poolDictionaries: ''
	category: 'MinimalKernel-Methods'!
