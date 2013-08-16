'From Cuis 4.2 of 25 July 2013 [latest update: #1802] on 16 August 2013 at 11:17:13.405764 am'!

!ByteArray methodsFor: 'initialize' stamp: 'KenD 8/10/2013 09:01'!
readHexFrom: aStream
	"Initialize the receiver from a hexadecimal string representation
		ByteArray readHexFrom: '1E1e'
		ByteArray readHexFrom: '1e1E'
		ByteArray readHexFrom: '1e1e'
	Note: lowercase hex digits are supported here, because we are reading strictly bytes.
	But are not really general, because lowercase e is used to denote scientific notation:
		16r1e0 = 1
		16r1E0 = 480
	"
	| map v ch value |
	map := '0123456789ABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		 (v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := v bitShift: 4.
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		(v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := value + v.
		self at: i put: value.
	]! !

