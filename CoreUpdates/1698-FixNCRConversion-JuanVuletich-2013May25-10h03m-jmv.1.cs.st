'From Cuis 4.1 of 12 December 2012 [latest update: #1696] on 25 May 2013 at 10:09:25.599852 am'!

!String methodsFor: 'converting' stamp: 'jmv 5/24/2013 16:22'!
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
	"

	^ByteArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrSize codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					ncrSize _ 0.
					maybeUnicodeNCR _ inStream next: 9.
					maybeUnicodeNCR first = $# ifTrue: [
						ncrSize _ maybeUnicodeNCR indexOf: $; ].
					ncrSize = 0
						ifFalse: [
							codePoint _ maybeUnicodeNCR second = $x
								ifTrue: [ ('16r', (maybeUnicodeNCR copyFrom: 3 to: ncrSize) asUppercase) asNumber ]
								ifFalse: [ (maybeUnicodeNCR copyFrom: 2 to: ncrSize) asNumber ]]
						ifTrue: [
							"Not an NCR after all. Just add the $& and continue from there"
							codePoint _ nextChar unicodeCodePoint ].
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					inStream position: prevPos + ncrSize ]
				ifFalse: [
					codePoint _ nextChar unicodeCodePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !

