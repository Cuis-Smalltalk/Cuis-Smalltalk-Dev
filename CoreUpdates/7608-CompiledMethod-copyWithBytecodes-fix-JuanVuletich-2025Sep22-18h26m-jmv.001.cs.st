'From Cuis7.5 [latest update: #7604] on 22 September 2025 at 6:26:59 pm'!

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 9/22/2025 17:39:30'!
copyWithBytecodes: bytecodes
	"Assume bytecodes are regular bytecodes, even if we ourselves are a primitive method.
	The new bytecodes are in the same bytecode set as the current ones.
	Remove the primitive bit from the header if present. Answer will not be a primitive."

	| copy header |
	header := self header bitAt: 17 put: 0.
	copy := CompiledMethod newMethod: bytecodes size+4 header: header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	bytecodes withIndexDo: [ :byte :i |
		copy at: i+self initialPC-1 put: (bytecodes at: i) ].
	^ copy! !

