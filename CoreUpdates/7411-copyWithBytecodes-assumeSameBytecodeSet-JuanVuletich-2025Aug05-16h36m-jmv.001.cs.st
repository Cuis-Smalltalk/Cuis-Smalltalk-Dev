'From Cuis7.5 [latest update: #7399] on 5 August 2025 at 4:44:36 pm'!

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 8/5/2025 16:36:42'!
copyWithBytecodes: bytecodes
	"Assume bytecodes are regular bytecodes, even if we ourselves are a primitive method.
	The new bytecodes are in the same bytecode set as the current ones.
	This means the header word remains the same."

	| copy |
	copy := CompiledMethod newMethod: bytecodes size+4 header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	bytecodes withIndexDo: [ :byte :i |
		copy at: i+self initialPC-1 put: (bytecodes at: i) ].
	^ copy! !

