'From Cuis7.5 [latest update: #7270] on 17 June 2025 at 5:25:12 pm'!

!BytecodeEncoder class methodsFor: 'compiled method support' stamp: 'jmv 6/17/2025 16:51:19'!
nonLocalReturnScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for non-local returns."
	self subclassResponsibility! !


!EncoderForSistaV1 class methodsFor: 'instruction stream support' stamp: 'jmv 6/17/2025 16:54:59'!
nonLocalReturnScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for non-local returns."

	"88-91		010110 ii			Return Receiver/true/false/nil
	 92			01011100			Return top"

	^[ :instr |
		instr between: 88 and: 92 ]! !


!EncoderForV3PlusClosures class methodsFor: 'compiled method support' stamp: 'jmv 6/17/2025 16:53:09'!
nonLocalReturnScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for non-local returns."

	^[ :instr |
		instr between: 120 and: 124 ]! !


!CompiledMethod methodsFor: 'closures' stamp: 'jmv 6/9/2025 17:12:52'!
containsBlockClosures

	| scanner |
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		self encoderClass createClosureScanBlock! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 6/9/2025 17:09:09'!
readsField: varIndex 
	"Answer whether the receiver loads the instance variable indexed by the 
	 argument."
	"eem 5/24/2008 Rewritten to no longer assume the compiler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	self isReturnField ifTrue: [^self returnField = varIndexCode].
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass instVarReadScanBlockFor: varIndexCode using: scanner)! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 6/9/2025 17:09:22'!
readsRef: literalAssociation 
	"Answer whether the receiver loads the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass bindingReadScanBlockFor: litIndex using: scanner)! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 6/9/2025 17:30:36'!
sendsToSuper
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass superSendScanBlockUsing: scanner)! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 6/9/2025 17:08:55'!
writesField: varIndex
	"Answer whether the receiver stores into the instance variable indexed
	 by the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| varIndexCode scanner |
	self isQuick ifTrue: [^false].
	varIndexCode := varIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass instVarWriteScanBlockFor: varIndexCode using: scanner)! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 6/9/2025 17:10:28'!
writesRef: literalAssociation 
	"Answer whether the receiver stores into the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	scanner := InstructionStream on: self.
	^ scanner scanFor:
		(self encoderClass bindingWriteScanBlockFor: litIndex using: scanner)! !

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 6/17/2025 16:31:27'!
usesClosureBytecodes
	"Answer whether the receiver was compiled using the closure compiler.
	 This is used to help DebuggerMethodMap choose which mechanisms to
	 use to inspect activations of the receiver.
	In Cuis all the subclasses of BytecodeEncoder #supportsClosures."
	
	^true! !


!BlockClosure methodsFor: 'testing' stamp: 'jmv 6/17/2025 16:55:24'!
hasNonLocalReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass nonLocalReturnScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 6/17/2025 16:39:58'!
sendsToSuper
	"Answer whether the receiver sends any message to super."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass superSendScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end! !

!methodRemoval: Decompiler #blockScopeRefersOnlyOnceToTemp: stamp: 'jmv 6/17/2025 17:24:41'!
Decompiler removeSelector: #blockScopeRefersOnlyOnceToTemp:!
!methodRemoval: CompiledMethod #scanLongStore: stamp: 'jmv 6/17/2025 17:24:42'!
CompiledMethod removeSelector: #scanLongStore:!
!methodRemoval: CompiledMethod #writesFieldCode:with:using: stamp: 'jmv 6/17/2025 17:24:43'!
CompiledMethod removeSelector: #writesFieldCode:with:using:!
!methodRemoval: CompiledMethod #scanVeryLongStore:offset: stamp: 'jmv 6/17/2025 17:24:44'!
CompiledMethod removeSelector: #scanVeryLongStore:offset:!
!methodRemoval: CompiledMethod #scanLongLoad: stamp: 'jmv 6/17/2025 17:24:45'!
CompiledMethod removeSelector: #scanLongLoad:!
!methodRemoval: CompiledMethod #scanVeryLongLoad:offset: stamp: 'jmv 6/17/2025 17:24:46'!
CompiledMethod removeSelector: #scanVeryLongLoad:offset:!
!methodRemoval: EncoderForV3PlusClosures class #method:readsField: stamp: 'jmv 6/17/2025 17:24:53'!
EncoderForV3PlusClosures class removeSelector: #method:readsField:!
!methodRemoval: EncoderForV3PlusClosures class #method:writesField: stamp: 'jmv 6/17/2025 17:24:53'!
EncoderForV3PlusClosures class removeSelector: #method:writesField:!
!methodRemoval: BytecodeEncoder class #method:readsField: stamp: 'jmv 6/17/2025 17:24:54'!
BytecodeEncoder class removeSelector: #method:readsField:!
!methodRemoval: BytecodeEncoder class #method:writesField: stamp: 'jmv 6/17/2025 17:24:54'!
BytecodeEncoder class removeSelector: #method:writesField:!
