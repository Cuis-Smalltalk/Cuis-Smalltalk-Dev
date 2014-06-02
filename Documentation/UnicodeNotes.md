Unicode support in Cuis 4.1
------------------------------

### Note

This document has been updated as Juan Vuletich has implemented Unicode
related changes with change sets 1590 and 1600

https://github.com/jvuletich/Cuis/blob/master/UpdatesSinceLastRelease/1590-InvertibleUTF8Conversion-JuanVuletich-2013Feb08-08h11m-jmv.1.cs.st

https://github.com/jvuletich/Cuis/blob/master/UpdatesSinceLastRelease/1600-WindowsClipboardFix-JuanVuletich-2013Feb14-14h37m-jmv.1.cs.st


Note: More checks needed if everything is updated correctly in this file. Some of the Cuis methods cited here have changed.


### Introduction

Cuis has limited Unicode support. 

Externally, this means for the clipboard UTF8 is used. 
For files it is possible to read and write UTF8 encodes text files losslessly.
Characters which are in the ISO8859-15 character set can be dealt with directly as 
internally the 8 bit ISO8959-15 convention is used (http://en.wikipedia.org/wiki/ISO/IEC_8859-15). This means less 
than 255 code points. 

The rest is converted to numerical character entities when reading from a file or when text is pasted through the clipboard.

Note: if you need to display non-latin characters (Japanese, Chinese, Cyrillic, Tamil,...) there is an experimental 'UniCodes' package at https://github.com/KenDickey/Cuis-Smalltalk-Unicode.

### Construction of example data for UTF8 test


With the on-line converter http://rishida.net/tools/conversion/ example data may be constructed for tests.

abc àè€ αβγ (abc the a with gravis, e with gravis, euro sign, alpha, beta, gamma) 


Decimal NCRs

    abc &#224;&#232;&#8364; &#945;&#946;&#947;


Hexadecimal NCRs

    abc &#x00E0;&#x00E8;&#x20AC; &#x03B1;&#x03B2;&#x03B3;


UTF8 code units


    61 62 63 20 C3 A0 C3 A8 E2 82 AC 20 CE B1 CE B2 CE B3

UTF8 code units as Smalltalk literal array

    #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
	16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3]


JavaScript escapes

    abc \u00E0\u00E8\u20AC \u03B1\u03B2\u03B3


Write the data above as a UTF8 encoded file in binary mode.  

````smalltalk
     | stream |

     stream := (FileStream newFileNamed: 'UTF8abc-test.txt') binary.
     stream nextPutAll: #[16r61 16r62 16r63 16r20 16rC3 16rA0 16rC3 16rA8 16rE2 
	                              16r82 16rAC 16r20 16rCE 16rB1 16rCE 16rB2 16rCE 16rB3].
     stream close.
````
   

Read it back

````smalltalk
    String fromUtf8:
          (FileStream fileNamed: 'UTF8abc-test.txt') contentsOfEntireFile
````

gives the result below. The result appears correctly in the Cuis image but not in this UnicodeNotes.md file as this is a UTF8 file 
and thus does not show ISO8859-15 properly.

      'abc �� &#945;&#946;&#947;'


Whereas

````smalltalk
      (FileStream fileNamed: 'UTF8abc-test.txt') contentsOfEntireFile     
````

gives the result

      'abc àè€ αβγ'

which are the UTF8 bytes. Again here in this UnicodeNotes.md file this appears correctly whereas in the Cuis image it does not.



### Test with ISO8859-15 text file

````smalltalk
    | stream |

     stream  := (FileStream newFileNamed: 'ISO8859-15abc-test.txt').
     32 to: 255 do: [ :code | stream nextPut: code asCharacter].
     stream close.
````

Reading it back

````smalltalk
    (FileStream fileNamed: 'ISO8859-15abc-test.txt') contentsOfEntireFile
````

The default encoding for files is ISO8859-15.    


### Note about this file

Some characters might appear wrongly in this UnicodeNotes.md file. 
It is recommended to do the tests mentioned above in an recent Cuis image with the UTF8 code units 
and the constructed ISO8859 file.


### Implementation in Cuis 4.1


#### Class Character
````smalltalk
    Magnitude subclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit 
	                     UnaccentedTable UnicodeCodePoints UppercaseBit'
	poolDictionaries: ''
	category: 'Kernel-Text'
````

Comment:
I represent a character by storing its associated Latin-9 code (ISO 8859-15). My instances are created uniquely, 
so that all instances of a character ($R, for example) are identical.


The class variable UnicodeCodePoints contains the Unicode values with which Cuis can deal. It is initialized with

````smalltalk
    Character initializeUnicodeCodePoints
````    

In a pristine Cuis image initialization has been done.

    initializeUnicodeCodePoints

	"Initialize the table of Unicode code points"
	UnicodeCodePoints _ Array new: 256.
	0 to: 255 do: [ :code |
		UnicodeCodePoints at: code + 1 put: code ].
	
	"The following codes are different in ISO 8859-15 from those in ISO 8859-1,
	so the character code is not equal to the Unicode code point"
	UnicodeCodePoints at: 16rA4+1 put: 16r20AC.		"euro sign"
	UnicodeCodePoints at: 16rA6+1 put: 16r160.		"latin capital letter S with caron"
	UnicodeCodePoints at: 16rA8+1 put: 16r161.		"latin small letter s with caron"
	UnicodeCodePoints at: 16rB4+1 put: 16r17D.		"latin capital letter Z with caron"
	UnicodeCodePoints at: 16rB8+1 put: 16r17E.		"latin small letter z with caron"
	UnicodeCodePoints at: 16rBC+1 put: 16r152.		"latin capital ligature OE"
	UnicodeCodePoints at: 16rBD+1 put: 16r153.		"latin small ligature oe"
	UnicodeCodePoints at: 16rBE+1 put: 16r178.		"latin capital letter Y with diaeresis"
	


Method Character class>>unicodeCodePoint:

    unicodeCodePoint: codePoint
    	"
    	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
    	self assert: (Character unicodeCodePoint: 16r41) = $A.
    	self assert: (Character unicodeCodePoint: 16r20AC) = $€.
    	"
    	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
    		ifNotNil: [ :code | Character value: code ]
		


Method Character class>>iso8859s15CodeForUnicodeCodePoint:

    iso8859s15CodeForUnicodeCodePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r41) = $A iso8859s15Code.
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r20AC) = $€ iso8859s15Code.
	"
	| code |
	code _ (UnicodeCodePoints indexOf: codePoint) -1.
	code = -1 ifTrue: [ ^nil ].
	^code


In Cuis 4.1 the value instance variable for instances of Character is restricted to be 8 bit only. 
But the value as such is a 32bit integer value.

The implementation of Character class>>value:

    value: anInteger 
        "Answer the Character whose value is anInteger."

        ^CharacterTable at: anInteger + 1

The CharacterTable class variable has 256 entries and a Character _must_ be included there.

Contrariwise in Squeak 4.4. (and earlier versions) the method Character class>> value: is implemented as

    value: anInteger 
        "Answer the Character whose value is anInteger."

        anInteger > 255 ifTrue: [^self basicNew setValue: anInteger].
        ^ CharacterTable at: anInteger + 1.



Method Character class>>utf8BytesOfUnicodeCodePoint:

    utf8BytesOfUnicodeCodePoint: aCodePoint

    	^ ByteArray streamContents: [ :strm |
    		Character
    			evaluate: [ :byte |
    				strm nextPut: byte ]
    			withUtf8BytesOfUnicodeCodePoint: aCodePoint ]



Method Character class>>evaluate:withUtf8BytesOfUnicodeCodePoint:


    evaluate: aBlock withUtf8BytesOfUnicodeCodePoint: aCodePoint
    	"See senders for typical usage"

    	| mask nBytes shift |
    	aCodePoint < 128 ifTrue: [
    		^aBlock value: aCodePoint ].
    	nBytes _ aCodePoint highBit + 3 // 5.
    	mask _ #(128 192 224 240 248 252 254 255) at: nBytes.
    	shift _ nBytes - 1 * -6.
    	aBlock value: (aCodePoint bitShift: shift) + mask.
    	2 to: nBytes do: [ :i | 
    		shift _ shift + 6.
    		aBlock value: ((aCodePoint bitShift: shift) bitAnd: 63) + 128 ]



Method Character class>>nextUnicodeCodePointFromUtf8:

    nextUnicodeCodePointFromUtf8: anUtf8Stream
    	"anUtf8Stream can be over a ByteArray
    	Answer nil if conversion not possible, because of invalid UTF-8"

    	| byte1 byte2 byte3 byte4 |
    	byte1 _ anUtf8Stream next asInteger.
    	byte1 < 128 ifTrue: [	"single byte"
    		^byte1 ].
	
    	"At least 2 bytes"
    	byte2 _ anUtf8Stream next asInteger.
    	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
    	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
    		^ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63) ].
	
    	"At least 3 bytes"
    	byte3 _ anUtf8Stream next asInteger.
    	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
    	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
    		^ ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63) ].

    	"4 bytes"
    	byte4 _ anUtf8Stream next asInteger.
    	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
    	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
    		^ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ].

    	^nil


#### Class String

    ArrayedCollection variableByteSubclass: #String
	instanceVariableNames: ''
	classVariableNames: 'CSLineEnders CSNonSeparators CSSeparators CaseInsensitiveOrder
	                     CaseSensitiveOrder LowercasingTable Tokenish UppercasingTable'
	poolDictionaries: ''
	category: 'Kernel-Text'

Comment:
A String is an indexed collection of Characters, compactly encoded as 8-bit bytes.

String support a vast array of useful methods, which can best be learned by browsing and trying out 
examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:
	


Method String>>asUtf8

    asUtf8
    	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
    	Answer a ByteArray.
	
    	See #fromUtf8: "

    	^self asUtf8: false
    	
    	
Method String>>asUtf8


    asUtf8: convertEmbeddedNCRs
    	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
    	Answer a ByteArray.
	
    	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
    	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
    	Note: The conversion of NCRs is reversible. See #fromUtf8:hex:
    	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
    	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

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
    						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]


String class>>fromUtf8:

    fromUtf8: aByteArray
    	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
    	See #asUtf8 "
    	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
    	See http://en.wikipedia.org/wiki/Numeric_character_reference"
    
    	^self fromUtf8: aByteArray hex: false


String class>>fromUtf8:hex:

    fromUtf8: aByteArray hex: useHex
    	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
    	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
 	
    	See http://en.wikipedia.org/wiki/Numeric_character_reference
    	See http://rishida.net/tools/conversion/. Tests prepared there.
	
    	Note: The conversion of NCRs is reversible. See #asUtf8:
    	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
    	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

    	^String streamContents: [ :strm | | bytes |
    		bytes _ aByteArray readStream.
    		[ bytes atEnd ] whileFalse: [
    			(Character nextUnicodeCodePointFromUtf8: bytes) ifNotNil: [ :codePoint | 
    				(Character unicodeCodePoint: codePoint)
    					ifNotNil: [ :iso8859m15code | strm nextPut: iso8859m15code]
    					ifNil: [
    						useHex
    							ifTrue: [
    								strm nextPutAll: '&#x'.
    								codePoint printOn: strm base: 16 length: 4 padded: true.
    								strm nextPut: $; ]
    							ifFalse: [
    								strm nextPutAll: '&#'.
    								codePoint printOn: strm base: 10.
    								strm nextPut: $; ]]]]] 



Line endings, method String>>withCuisLineEndings
    
    withCuisLineEndings
	    "assume the string is textual, and that CR, LF, and CRLF are all 
	    valid line endings.  Replace each occurence with a single Lf
	    ('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	    "

	^ self withLineEndings: String newLineString


	
### History

Unicode support needs a VM which supports Unicode. 
A Squeak VM for Windows which supports Unicode was introduced in June 2007 by Andreas Raab and Chris
Petsos

http://forum.world.st/New-Win32-VM-m17n-testers-needed-tc63730.html#none


### References

- http://www.unicode.org/versions/Unicode6.2.0/
- http://en.wikipedia.org/wiki/ISO/IEC_8859-15
- http://en.wikipedia.org/wiki/Plane_%28Unicode%29#Basic_Multilingual_Plane
- http://wiki.squeak.org/squeak/857
- http://wiki.squeak.org/squeak/919
- http://www.is.titech.ac.jp/~ohshima/squeak/m17npaper/index.html
- http://www.is.titech.ac.jp/~ohshima/squeak/squeak-multilingual-e.html
- https://code.google.com/p/chibi-scheme/source/browse/lib/scheme/char.sld
- Unicode treatment in the Scheme language: http://scheme-reports.org/2012/working-group-1.html 
- http://www.cprogramming.com/tutorial/unicode.html; Unicode in C and C++: What You Can Do About It Today
by Jeff Bezanson; keywords: Encoding, display, input method, internationalization (i18n), lexicography, UTF-8
- http://docs.python.org/2/howto/unicode.html support for either 16bit or 32 bit Unicode values, depending on how Python was compiled.
