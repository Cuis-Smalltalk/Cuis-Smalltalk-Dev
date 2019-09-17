'From Cuis 5.0 [latest update: #3873] on 16 September 2019 at 12:35:33 pm'!

!StdIOWriteStream methodsFor: 'open / close' stamp: 'jmv 9/16/2019 11:23:26'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
	buffer1 _ ByteArray new: 1! !

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 9/16/2019 12:34:21'!
nextPut: charOrByte
	"Write the given character or byte to this file.
	We can handle String (possibly including NCRs) and ByteArray (with utf-8 contents)
	
		 StdIOWriteStream stdout nextPutAll: ('--- &#16r2713; ===' asUtf8: true); flush.
		 StdIOWriteStream stdout nextPutAll: ('--- &#10003; ===' asUtf8: true); flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	charOrByte isNumber ifTrue: [
		buffer1 at: 1 put: charOrByte.
		self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
		^ charOrByte ].
	Character
		evaluate: [ :byte | self nextPut: byte ]
		withUtf8BytesOfUnicodeCodePoint: charOrByte codePoint.
	^ charOrByte
"
StdIOWriteStream stdout nextPutAll: ('
&#913;&#945; 	Alpha 	
&#914;&#946; 	Beta 	
&#915;&#947; 	Gamma 	
†&#948; 	Delta 	
&#917;„ 	Epsilon 	
&#918;… 	Zeta 	
&#919;&#951; 	Eta 	
&#920;&#952; 	Theta 	
&#921;&#953; 	Iota 	
&#922;&#954; 	Kappa 	
&#923;&#955; 	Lambda 	
&#924;&#956; 	Mu 	
&#925;&#957; 	Nu
&#926;&#958; 	Xi
&#927;&#959; 	Omicron
&#928;ƒ 	Pi
&#929;&#961; 	Rho
&#931;&#963;&#962; 	Sigma
&#932;&#964; 	Tau
&#933;&#965; 	Upsilon
&#934;&#966; 	Phi
&#935;&#967; 	Chi
&#936;&#968; 	Psi
‡&#969; 	Omega
&# 937;&# 969; 	Not a NCR, just regular ASCII chars!!
' asUtf8: true); flush
"! !

!methodRemoval: StdIOWriteStream #printUtf8CodePoint:!
StdIOWriteStream removeSelector: #printUtf8CodePoint:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
StdIOWriteStream releaseClassCachedState!

