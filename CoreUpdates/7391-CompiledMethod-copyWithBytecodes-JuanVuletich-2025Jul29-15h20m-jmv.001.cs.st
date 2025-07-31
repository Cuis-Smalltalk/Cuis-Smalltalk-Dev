'From Cuis7.5 [latest update: #7389] on 29 July 2025 at 3:21:03 pm'!

!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 7/23/2025 10:51:29'!
copyWithBytecodes: bytecodes
	"Assume bytecodes are regular bytecodes, even if we are a primitive method."

	| header copy |
	header := self header.
	"Take dawn the primitive flag if present."
	(header anyMask: 65536) ifTrue: [
		header := header - 65536 ].
	copy := CompiledMethod newMethod: bytecodes size+4 header: header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	bytecodes withIndexDo: [ :byte :i |
		copy at: i+self initialPC-1 put: (bytecodes at: i) ].
	^ copy! !

