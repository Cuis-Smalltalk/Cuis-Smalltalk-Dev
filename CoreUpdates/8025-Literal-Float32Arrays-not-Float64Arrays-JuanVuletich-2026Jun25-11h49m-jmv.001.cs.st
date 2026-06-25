'From Cuis7.9 [latest update: #8024] on 25 June 2026 at 11:52:13 am'!

!Float32Array commentStamp: '<historical>' prior: 0!
Float32rrays store 32bit IEEE floating point numbers, i.e. what usually called float in the C world.

Can be created as literals like:
#[ 1.0 2.0 3.0 ]!


!Float64Array commentStamp: '<historical>' prior: 0!
Float64Arrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.!


!Float32Array methodsFor: 'converting' stamp: 'jmv 25/Jun/2026, 2:39:45 pm (UTC)'!
asFloat32Array
	"Answer a Float32Array whose elements are the elements of the receiver"

	^self! !

!Float32Array methodsFor: 'printing' stamp: 'jmv 25/Jun/2026, 2:41:33 pm (UTC)'!
printAsLiteralOn: aStream

	aStream nextPut: $#; nextPut: $[.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]! !

!Float32Array methodsFor: 'printing' stamp: 'jmv 25/Jun/2026, 2:41:20 pm (UTC)'!
printOn: aStream

	self storeOn: aStream! !

!Float32Array methodsFor: 'printing' stamp: 'jmv 25/Jun/2026, 2:41:26 pm (UTC)'!
storeOn: aStream

	self isLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]! !

!Float32Array methodsFor: 'testing' stamp: 'jmv 25/Jun/2026, 2:43:11 pm (UTC)'!
isLiteral
	"so that 
	#(1 #[1.0 2 3] 5)
	prints itself"
	^self class == Float32Array and: [self allSatisfy: [:ea| ea isFinite]]! !


!Float64Array methodsFor: 'converting' stamp: 'jmv 25/Jun/2026, 2:39:35 pm (UTC)'!
asFloat64Array
	"Answer a Float64Array whose elements are the elements of the receiver"

	^self! !

!Float64Array methodsFor: 'testing' stamp: 'jmv 25/Jun/2026, 2:43:41 pm (UTC)'!
is: aSymbol
	^ aSymbol == #Float64Array or: [ super is: aSymbol ]! !


!Scanner methodsFor: 'expression types' stamp: 'jmv 25/Jun/2026, 2:24:16 pm (UTC)'!
scanLitByteVec
	"Also accept Floats besides bytes!!
	#[1 2 3 255]
	#[1.0 0.2 1.0]
	#[1.0 -0.2e-23 1.0e4]
	"
	| stream |
	stream := nil.
	[ tokenType == #rightBracket or: [ tokenType == #doIt ] ] whileFalse: [
		(token == #- and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
			self scanToken.
			token := token negated ].
		((token isInteger and: [ token between: 0 and: 255 ]) or: [token isFloat])
			ifFalse: [ ^ self offEnd: '8-bit integer, floating point number, or right bracket expected' ].
		stream ifNil: [
			stream := ((token isFloat ifTrue: [Float32Array] ifFalse: [ByteArray]) new: 16) writeStream ].
		stream nextPut: token.
		self scanToken ].
	token := stream
		ifNotNil: [ stream contents ]
		ifNil: [
			"For back compatibility, if empty, assume ByteArray"
			ByteArray new ]! !

!methodRemoval: Float64Array #isLiteral stamp: 'jmv 25/Jun/2026, 2:50:31 pm (UTC)'!
Float64Array removeSelector: #isLiteral!
!methodRemoval: Float64Array #storeOn: stamp: 'jmv 25/Jun/2026, 2:50:31 pm (UTC)'!
Float64Array removeSelector: #storeOn:!
!methodRemoval: Float64Array #printAsLiteralOn: stamp: 'jmv 25/Jun/2026, 2:50:31 pm (UTC)'!
Float64Array removeSelector: #printAsLiteralOn:!
!methodRemoval: Float64Array #printOn: stamp: 'jmv 25/Jun/2026, 2:50:31 pm (UTC)'!
Float64Array removeSelector: #printOn:!

!Float32Array reorganize!
('accessing' at: at:put: atWrap: atWrap:put: length)
('arithmetic' divideByArray:ifDivisorZero:ifBothZero: dot: normalize sum)
('comparing' hashFull primitiveEqual:)
('converting' asFloat32Array)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('printing' printAsLiteralOn: printElementsOn: printOn: storeOn:)
('testing' is: isLiteral)
('private')
!


!Float64Array reorganize!
('accessing' at: at:put: atWrap: atWrap:put: length)
('arithmetic' dot: normalize)
('comparing' hashFull primitiveEqual:)
('converting' asFloat64Array asIEEE32BitPrecisionFloat)
('objects from disk' writeOn:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primFromFloat32Array: primMulArray: primMulScalar: primSubArray: primSubScalar: sum)
('testing' is:)
!

