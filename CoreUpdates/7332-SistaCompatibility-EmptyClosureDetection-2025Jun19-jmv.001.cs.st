'From Cuis7.5 [latest update: #7279] on 19 June 2025 at 2:48:05 pm'!

!EncoderForSistaV1 class methodsFor: 'bytecode decoding' stamp: 'jmv 6/19/2025 10:35:26'!
emptyClosurePattern
	"Answer the byte pattern for an empty closure. For instance (verbatim from the bytecode view):
		45 <FA 00 02> closureNumCopied: 0 numArgs: 0 bytes 48 to 49
		48 	<4F> pushConstant: nil
		49 	<5E> blockReturn"
	"ByteArray readHexFrom: 'FA00024F5E'"
	^ #[250 0 2 79 94]! !


!EncoderForV3PlusClosures class methodsFor: 'bytecode decoding' stamp: 'jmv 6/19/2025 10:35:30'!
emptyClosurePattern
	"Answer the byte pattern for an empty closure. For instance (verbatim from the bytecode view):
		33 <8F 00 00 02> closureNumCopied: 0 numArgs: 0 bytes 37 to 38
		37 	<73> pushConstant: nil
		38 	<7D> blockReturn"
	"ByteArray readHexFrom: '8F000002737D'"
	^ #[143 0 0 2 115 125]! !


!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 6/19/2025 10:51:33'!
browseMethodsWithEmptyClosures
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| pattern1 pattern2 |
	pattern1 := CompiledMethod primaryBytecodeSetEncoderClass emptyClosurePattern.
	pattern2 := CompiledMethod secondaryBytecodeSetEncoderClass emptyClosurePattern.
	self
		browseAllSelect: [ :m | | s |
			s := InstructionStream on: m.
			s scanFor: [ :bytecode |
				(s method is: pattern1 subcollectionAt: s pc) or:
					[s method is: pattern2 subcollectionAt: s pc]]]
		name:  'Methods with empty closures'
		autoHighlight: '[]'! !

