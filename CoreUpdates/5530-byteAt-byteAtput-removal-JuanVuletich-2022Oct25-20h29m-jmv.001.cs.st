'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 8:32:39 pm'!

!SmallInteger methodsFor: 'printing' stamp: 'jmv 10/5/2022 11:03:54'!
printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len _ self decimalDigitLength.
	result _ ByteArray new: len.
	integer _ self.
	len to: 1 by: -1 do: [:i |
		next _ integer // 10.
		result at: i put: 48 + (integer - (next * 10)).
		integer _ next].
	^result asString! !

!SmallInteger methodsFor: 'printing' stamp: 'jmv 10/5/2022 11:04:49'!
printStringBase: b nDigits: n
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."
	
	| integer next result |
	result _ ByteArray new: n.
	integer _ self.
	n to: 1 by: -1 do: [:i |
		next _ integer // b.
		result at: i put: (Character digitValue: (integer - (next * b))) numericValue.
		integer _ next].
	^result asString! !


!InstructionStream methodsFor: 'decoding' stamp: 'jmv 10/4/2022 17:12:52'!
skipCallPrimitive
	"If the receiver's method starts with a callPrimitive: bytecode, skip it."
	| method encoderClass callPrimitiveCode |
	method := self method.
	encoderClass := method  encoderClass.
	callPrimitiveCode := encoderClass callPrimitiveCode.
	(method at: pc) = callPrimitiveCode ifTrue:
		[pc := pc + (encoderClass bytecodeSize: callPrimitiveCode)]! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 10/4/2022 17:10:27'!
getTableDirEntry: tagString from: fontData offset: offset
	"Find the table named tagString in fontData and return a table directory entry for it."

	| numTables pos currentTag tag |
	numTables := fontData shortAt: 5 + offset bigEndian: true.
	tag _ ByteArray new: 4.
	1 to: 4 do: [ :i | tag at: i put: (tagString at: i) asciiValue ].
	tag _ tag longAt: 1 bigEndian: true.
	pos _ 13 + offset.
	1 to: numTables do:[:i|
		currentTag _ fontData longAt: pos bigEndian: true.
		currentTag = tag ifTrue: [^TTFontTableDirEntry on: fontData at: pos].
		pos := pos+16].
	^nil! !


!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 10/4/2022 17:11:22'!
nextByte

	| value |
	value := fontData at: offset.
	offset := offset + 1.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 10/4/2022 17:11:18'!
nextBytes: numBytes into: array startingAt: byteOffset

	1 to: numBytes do:[:i|
		array at: i put: (fontData at: byteOffset + i - 1)].! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 10/4/2022 17:11:13'!
stringAt: stringOffset length: byteLength multiByte: aBoolean

	| string index stringLength |
	aBoolean ifFalse:[
		stringLength := byteLength.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character asciiValue: (fontData at: index + i - 1))].
		^string
	] ifTrue:[
		"UTF-16BE ???
		https://docs.microsoft.com/en-us/typography/opentype/spec/name#enc3
		This code appears to assume only lower byte is needed, most likely only ascii is supported here (jmv)"
		stringLength := byteLength // 2.
		string := String new: stringLength.
		index := stringOffset.
		1 to: stringLength do:[:i|
			string at: i put: (Character asciiValue: (fontData at: index + 1)).
			index := index + 2].
		^string]! !

!methodRemoval: ByteArray #byteAt: stamp: 'jmv 10/25/2022 20:32:09'!
ByteArray removeSelector: #byteAt:!
!methodRemoval: ByteArray #byteAt:put: stamp: 'jmv 10/25/2022 20:32:16'!
ByteArray removeSelector: #byteAt:put:!
!methodRemoval: String #byteAt: stamp: 'jmv 10/25/2022 20:32:09'!
String removeSelector: #byteAt:!
!methodRemoval: String #byteAt:put: stamp: 'jmv 10/25/2022 20:32:15'!
String removeSelector: #byteAt:put:!
