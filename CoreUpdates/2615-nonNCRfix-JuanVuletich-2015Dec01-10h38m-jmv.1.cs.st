'From Cuis 4.2 of 25 July 2013 [latest update: #2613] on 1 December 2015 at 10:38:52.999137 am'!

!String methodsFor: 'converting' stamp: 'jmv 12/1/2015 10:15'!
asUtf8: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8:hex:trimLastNull:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it.

Copy the following and paste in your external text editor
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
							codePoint _ nextChar unicodeCodePoint ].
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					inStream position: prevPos + ncrEnd ]
				ifFalse: [
					codePoint _ nextChar unicodeCodePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !

