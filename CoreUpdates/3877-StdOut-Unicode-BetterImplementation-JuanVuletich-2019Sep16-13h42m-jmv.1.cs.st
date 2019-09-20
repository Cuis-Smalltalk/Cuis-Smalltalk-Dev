'From Cuis 5.0 [latest update: #3876] on 16 September 2019 at 1:48:19 pm'!
!classDefinition: #StdIOWriteStream category: #'System-Support'!
Stream subclass: #StdIOWriteStream
	instanceVariableNames: 'fileID name buffer1 '
	classVariableNames: 'StdErr StdOut '
	poolDictionaries: ''
	category: 'System-Support'!

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 9/16/2019 13:46:59'!
nextPutAll: aStringOrUTF8Bytes
	"Write the given String (possibly including NCRs) or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes _ aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8: true ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: '
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
'; flush
"! !


!StdIOWriteStream methodsFor: 'open / close' stamp: 'jmv 9/16/2019 13:47:35'!
openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.! !

!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 9/16/2019 13:46:29'!
nextPut: aCharacter
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: aCharacter asString.
	^aCharacter! !

!classDefinition: #StdIOWriteStream category: #'System-Support'!
Stream subclass: #StdIOWriteStream
	instanceVariableNames: 'fileID name'
	classVariableNames: 'StdErr StdOut'
	poolDictionaries: ''
	category: 'System-Support'!
