'From Cuis 5.0 [latest update: #4910] on 12 October 2021 at 10:22:15 am'!

!MethodNode methodsFor: 'printing' stamp: 'jmv 10/12/2021 09:48:02'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', ((interpreterClass primitiveTable) at: primitive + 1), '" ']! !

