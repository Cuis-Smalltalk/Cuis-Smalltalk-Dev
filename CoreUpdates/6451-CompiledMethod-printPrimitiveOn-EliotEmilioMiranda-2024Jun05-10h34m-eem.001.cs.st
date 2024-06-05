'From Cuis7.1 [latest update: #6449] on 5 June 2024 at 10:56:36 am'!

!CompiledMethod methodsFor: 'printing' stamp: 'eem 6/5/2024 10:55:56'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primIndex primDecl |
	(primIndex := self primitive) = 0 ifTrue:
		[^self].
	primIndex = 120 ifTrue: "External call spec"
		[^aStream print: (self literalAt: 1); newLine].
	aStream nextPutAll: '<primitive: '.
	primIndex = 117
		ifTrue:
			[primDecl := self literalAt: 1.
			(primDecl at: 2) printOn: aStream.
			(primDecl at: 1) ifNotNil: [ :moduleName |
				aStream nextPutAll:' module: '.
				moduleName printOn: aStream]]
		ifFalse:
			[aStream print: primIndex].
	self primitiveErrorVariableName ifNotNil: [ :primitiveErrorVariableName |
		aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; newLine! !
