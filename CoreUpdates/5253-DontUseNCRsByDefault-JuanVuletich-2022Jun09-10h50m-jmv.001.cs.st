'From Cuis 6.0 [latest update: #5254] on 9 June 2022 at 10:59:53 am'!

!String methodsFor: 'converting' stamp: 'jmv 6/9/2022 10:52:26'!
asUtf32Words
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15).
	Answer a WordArray.
	Ignore any NCRs (Numeric Character References). See #fromUtf32Words: "

	| inStream nextChar codePoint |
	^WordArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			outStream nextPut: codePoint ]]! !

!String methodsFor: 'converting' stamp: 'jmv 6/9/2022 10:56:28'!
asUtf32Words: convertEmbeddedNCRs
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a WordArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	see #asUtf8Bytes:
	"

	| str |
	^WordArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrEnd codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					ncrEnd _ 0.
					maybeUnicodeNCR _ inStream next: 9.
					(maybeUnicodeNCR notEmpty and: [ maybeUnicodeNCR first = $# ]) ifTrue: [
						ncrEnd _ maybeUnicodeNCR indexOf: $; ].
					ncrEnd = 0
						ifFalse: [
							maybeUnicodeNCR second = $x
								ifTrue: [
									str _ (maybeUnicodeNCR copyFrom: 3 to: ncrEnd) asUppercase.
									(str first isDigit or: [ str first asciiValue between: $A asciiValue and: $F asciiValue])
										ifTrue: [ codePoint _  ('16r', str) asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]
								ifFalse: [
									str _ maybeUnicodeNCR copyFrom: 2 to: ncrEnd.
									str first isDigit
										ifTrue: [ codePoint _  codePoint _ str asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]].
					ncrEnd = 0
						ifTrue: [
							"Not an NCR after all. Just add the $& and continue from there"
							codePoint _ nextChar codePoint ].
					outStream nextPut: codePoint.
					inStream position: prevPos + ncrEnd ]
				ifFalse: [
					codePoint _ nextChar codePoint.
					outStream nextPut: codePoint ]]]! !

!String methodsFor: 'converting' stamp: 'jmv 6/9/2022 10:58:04'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray with a valid UTF-8 sequence.
	Ignore any NCRs (Numeric Character References). See #fromUtf8Bytes: "

	| inStream nextChar codePoint |
	^ByteArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !

!String methodsFor: 'converting' stamp: 'jmv 6/9/2022 10:57:55'!
asUtf8Bytes: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8Bytes:hex:trimLastNull:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it.

You can hack #asUtf8Bytes to call this method with argument true, and then copy the following and paste in your external text editor
&#913;&#945; 	Alpha 	
&#914;&#946; 	Beta 	
&#915;&#947; 	Gamma 	
&#916;&#948; 	Delta 	
&#917;&#949; 	Epsilon 	
&#918;&#950; 	Zeta 	
&#919;&#951; 	Eta 	
&#920;&#952; 	Theta 	
&#921;&#953; 	Iota 	
&#922;&#954; 	Kappa 	
&#923;&#955; 	Lambda 	
&#924;&#956; 	Mu 	
&#925;&#957; 	Nu
&#926;&#958; 	Xi
&#927;&#959; 	Omicron
&#928;&#960; 	Pi
&#929;&#961; 	Rho
&#931;&#963;&#962; 	Sigma
&#932;&#964; 	Tau
&#933;&#965; 	Upsilon
&#934;&#966; 	Phi
&#935;&#967; 	Chi
&#936;&#968; 	Psi
&#937;&#969; 	Omega
&# 937;&# 969; 	Not a NCR, just regular ASCII chars!!
	"

	| str |
	^ByteArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrEnd codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					ncrEnd _ 0.
					maybeUnicodeNCR _ inStream next: 9.
					(maybeUnicodeNCR notEmpty and: [ maybeUnicodeNCR first = $# ]) ifTrue: [
						ncrEnd _ maybeUnicodeNCR indexOf: $; ].
					ncrEnd = 0
						ifFalse: [
							maybeUnicodeNCR second = $x
								ifTrue: [
									str _ (maybeUnicodeNCR copyFrom: 3 to: ncrEnd) asUppercase.
									(str first isDigit or: [ str first asciiValue between: $A asciiValue and: $F asciiValue])
										ifTrue: [ codePoint _  ('16r', str) asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]
								ifFalse: [
									str _ maybeUnicodeNCR copyFrom: 2 to: ncrEnd.
									str first isDigit
										ifTrue: [ codePoint _  codePoint _ str asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]].
					ncrEnd = 0
						ifTrue: [
							"Not an NCR after all. Just add the $& and continue from there"
							codePoint _ nextChar codePoint ].
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					inStream position: prevPos + ncrEnd ]
				ifFalse: [
					codePoint _ nextChar codePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/9/2022 10:54:48'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	aString isByteString ifTrue: [
		^bytes = aString asUtf8Bytes ].

	^ bytes = aString asUtf8Bytes.! !

