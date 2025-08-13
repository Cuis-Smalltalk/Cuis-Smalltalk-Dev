'From Cuis7.5 [latest update: #7398] on 5 August 2025 at 4:11:46 pm'!

!CompiledMethod class methodsFor: 'method encoding' stamp: 'dtl 5/6/2022 19:03'!
byteCodeSetsKnownToTheVM
	"Answer the encoder names for the bytecode sets reported to be supported
	by the virtual machine. Informational, failure indicates only that the VM does
	not support the primitive query."

	"CompiledMethod byteCodeSetsKnownToTheVM"

	<primitive: 'primitiveBytecodeSetsAvailable'>
	^ self primitiveFailed! !

