'From Cuis 6.0 [latest update: #5255] on 10 June 2022 at 9:06:19 am'!

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 11:11:34'!
ascii
	"return a set containing just all the 7 bit ASCII Characters"

	| set |
	set _ self new.
	0 to: 127 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 11:11:23'!
nonAscii
	"return a set containing all Characters that are not 7 bit ASCII"

	| set |
	set _ self new.
	128 to: 255 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !


!String methodsFor: 'testing' stamp: 'jmv 6/9/2022 15:01:48'!
isAscii
	"Return true when I am pure 7-bit ASCII.
	This means, for example, that my bytes are also my UTF-8 representation."

	^ String isAscii: self! !


!String class methodsFor: 'services' stamp: 'jmv 6/9/2022 15:01:34'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8."

	^ (String
		findFirstInString: aStringOrByteArray
		inSet: `(CharacterSet nonAscii) byteArrayMap`
		startingAt: 1)
			= 0! !

