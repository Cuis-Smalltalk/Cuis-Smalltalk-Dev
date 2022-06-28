'From Cuis 6.0 [latest update: #5325] on 27 June 2022 at 4:03:25 pm'!

!String class methodsFor: 'primitives' stamp: 'jmv 6/27/2022 15:43:46'!
findFirstInString: aByteStringOrByteArray inSet: inclusionMap startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i _ start.
	stringSize _ aByteStringOrByteArray size.
	[ i <= stringSize and: [ (inclusionMap at: (aByteStringOrByteArray basicAt: i) +1) = 0 ] ] whileTrue: [ 
		i _ i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !

!String class methodsFor: 'primitives' stamp: 'jmv 6/27/2022 15:44:56'!
indexOfByte: anInteger inString: aString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	start > 0 ifFalse: [ ^ 0 ].
	stringSize _ aString size.
	start to: stringSize do: [:pos |
		(aString basicAt: pos) = anInteger ifTrue: [^ pos]].
	^ 0
! !

!String class methodsFor: 'primitives' stamp: 'jmv 6/27/2022 15:45:23'!
translate: aString from: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString basicAt: i) +1) ]! !

!String class methodsFor: 'services' stamp: 'jmv 6/27/2022 15:40:39'!
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>

	len1 _ string1 size.
	len2 _ string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 _ order at: (string1 basicAt: i) + 1.
		c2 _ order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
! !

!String class methodsFor: 'services' stamp: 'jmv 6/27/2022 15:40:27'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>

	key isEmpty ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body basicAt: startIndex+index-1) + 1)
				= (matchTable at: (key basicAt: index) + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !


!ByteArray class methodsFor: 'byte based hash' stamp: 'jmv 6/27/2022 15:45:37'!
hashBytes: aByteArray startingWith: speciesHash
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	byteArraySize _ aByteArray size.
	hash _ speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [ :pos |
		hash _ hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low _ hash bitAnd: 16383.
		hash _ (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash! !

