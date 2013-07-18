'From Cuis 4.1 of 12 December 2012 [latest update: #1609] on 22 February 2013 at 9:09:47 am'!

!Clipboard methodsFor: 'accessing' stamp: 'jmv 2/22/2013 09:07'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times."
	contents _ (anObject isString or: [ anObject is: #Text])
		ifTrue: [ anObject withCuisLineEndings ]
		ifFalse: [ anObject copyForClipboard ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface | | interface |
		interface _ clipboardInterface current.
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat! !


!String methodsFor: 'converting' stamp: 'jmv 2/22/2013 09:06'!
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
					maybeUnicodeNCR _ inStream next: 9.
					maybeUnicodeNCR first = $# ifTrue: [
						ncrSize _ maybeUnicodeNCR indexOf: $;.
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
						inStream position: prevPos + ncrSize ]]
				ifFalse: [
					codePoint _ nextChar unicodeCodePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !

