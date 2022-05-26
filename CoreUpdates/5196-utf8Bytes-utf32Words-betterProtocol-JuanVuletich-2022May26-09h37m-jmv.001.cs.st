'From Cuis 6.0 [latest update: #5195] on 26 May 2022 at 9:40:31 am'!

!String commentStamp: 'jmv 5/23/2022 15:16:49' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-9 (ISO 8859-15) encoding. Each character takes 1 byte.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

Cuis includes limited support for Unicode. A ByteArray can hold any Unicode string in utf-8. There is support for converting to and from instances of String. As String can not hold all Unicode code points, (just the ISO 8859-15 subset), others are converted to NCRs. See #fromUtf8Bytes:hex:trimLastNull: and #asUtf8Bytes: .!


!String methodsFor: 'converting' stamp: 'jmv 5/26/2022 09:34:43'!
asUtf32Words
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a WordArray.
	
	See #fromUtf32Words: "

	^self asUtf32Words: true.! !

!String methodsFor: 'converting' stamp: 'jmv 5/26/2022 09:31:45'!
asUtf32Words: convertEmbeddedNCRs
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a WordArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8Bytes:hex:trimLastNull:
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

!String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:17:02'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	See #fromUtf8Bytes: "

	^self asUtf8Bytes: true.! !

!String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:17:06'!
asUtf8Bytes: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8Bytes:hex:trimLastNull:
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


!String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 09:35:13'!
fromUtf32Words: aWordArray
	"Convert the given bytes from UTF-32 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf32Words "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	^self fromUtf32Words: aWordArray hex: false trimLastNull: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 09:35:24'!
fromUtf32Words: aWordArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-32 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf32Words:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			"Add just one char if possible, or the apropriate sequence for the NCR"
			self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ].
		(doTrimLastNullChar and: [ aWordArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !

!String class methodsFor: 'instance creation' stamp: 'jmv 5/23/2022 15:14:19'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8Bytes "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	^self fromUtf8Bytes: aByteArray hex: false trimLastNull: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 09:24:46'!
fromUtf8Bytes: aByteArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8Bytes:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					"Add just one char if possible, or the apropriate sequence for the NCR"
					self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ].
			byteIndex _ byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!Utf32String methodsFor: 'converting' stamp: 'jmv 5/26/2022 09:25:00'!
asUtf32Words
	"Answer a WordArray."

	^codePoints! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:09:09'!
asUtf8Bytes
	"Answer a ByteArray."

	^String utf8FromUtf32: codePoints! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 09:32:36'!
fromUtf32Words: words

	^self withWords: words! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/23/2022 15:14:43'!
fromUtf8Bytes: aByteArray

	^self withWords: (String utf32FromUtf8: aByteArray).! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 5/26/2022 09:25:04'!
asUtf32Words
	"Answer a WordArray."

	^String utf32FromUtf8: bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:09:13'!
asUtf8Bytes
	"Answer a ByteArray."

	^bytes! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 09:32:40'!
fromUtf32Words: aWordArray

	^self withBytes: (String utf8FromUtf32: aWordArray).! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/23/2022 15:14:46'!
fromUtf8Bytes: bytes

	^self withBytes: bytes! !


!String class methodsFor: 'instance creation' stamp: 'jmv 5/23/2022 15:08:26'!
addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs
	"Convert the given Unicode codePoint to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8Bytes:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	(Character codePoint: codePoint trySimilar: true)
		ifNotNil: [ :char | strm nextPut: char]
		ifNil: [
			useHexForNCRs
				ifTrue: [
					strm nextPutAll: '&#x'.
					codePoint printOn: strm base: 16 length: 4 padded: true.
					strm nextPut: $; ]
				ifFalse: [
					strm nextPutAll: '&#'.
					codePoint printOn: strm base: 10.
					strm nextPut: $; ]]! !


!StandardFileStream methodsFor: 'open/close' stamp: 'jmv 5/26/2022 09:27:13'!
open: fileName forWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ StandardFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ String new: 1.
	self enableReadBuffering
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:10:52'!
primOpen: fileNameAsUtf8Bytes writable: writableFlag
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil
! !


!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 5/26/2022 09:27:50'!
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
	utf8Bytes _ aStringOrUTF8Bytes isStringLike
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
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


!Utf32String methodsFor: 'copying' stamp: 'jmv 5/26/2022 09:25:23'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ startIndex-1.
	epilogFirstIndex _ endIndex+1.
	^Utf32String withWords:
		(codePoints copyFrom: 1 to: prologLastIndex),
		replacement asUtf32Words,
		(codePoints
			copyFrom: epilogFirstIndex
			to: codePoints size)! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:16:00'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8Bytes: self asUtf8Bytes! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 5/23/2022 15:09:39'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	^Utf8String withBytes:
		(bytes copyFrom: 1 to: prologLastIndex),
		replacement asUtf8Bytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size)! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/23/2022 15:16:03'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8Bytes: bytes! !


!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/26/2022 09:28:15'!
createDirectory: fullPathName
	self primCreateDirectory: fullPathName asUtf8Bytes! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/26/2022 09:28:22'!
deleteDirectory: fullPathName
	self primDeleteDirectory: fullPathName asUtf8Bytes! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/26/2022 09:28:31'!
deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName asUtf8Bytes]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/26/2022 09:28:59'!
rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFileEntry exists or: [ newFileFullName asDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 5/26/2022 09:29:13'!
renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName asUtf8Bytes to: newFileFullName asUtf8Bytes ]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 5/26/2022 09:28:02'!
basicDirectoryExists: fullPathName

 	^(
		(self primLookupEntryIn: fullPathName asUtf8Bytes index: 1) 
			== #badDirectoryPath
		) not! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 5/26/2022 09:28:42'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory lookIn |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray _ self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done _ true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				entry name: (entryArray at: 1) parent: parentEntryOrNil.
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index _ index + 1].

	^entries asArray! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 5/26/2022 09:29:52'!
updateEntry: aFileSystemEntry
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primFixedNameLookupEntryIn: aFileSystemEntry parent pathName asUtf8Bytes index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn _ aFileSystemEntry parent pathName.
	index _ 1.
	[
		entryArray _ self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ self].
		entryArray == nil ifTrue: [
			^ self].
		isDirectory _ entryArray at: 4.
		aFileSystemEntry name = (entryArray at: 1) ifTrue: [
			isDirectory == aFileSystemEntry isDirectory ifTrue: [
				aFileSystemEntry updateFrom: entryArray entryInParent: index ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index _ index + 1] repeat! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:11:34'!
primCreateDirectory: fullPathAsUtf8Bytes
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:11:46'!
primDeleteDirectory: fullPathAsUtf8Bytes
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:11:49'!
primDeleteFileNamed: aFileNameAsUtf8Bytes
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:16:07'!
primFixedNameLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer _ self primLookupEntryIn: fullPathAsUtf8Bytes index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (String fromUtf8Bytes: answer first asByteArray) ].
	^answer! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:12:11'!
primLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.
	
	Warning: The 'name' field is an instance of String, but in reality it contains the bytes for its UTF-8 representation.
	For instance, if the real name is 'puño' we'll get  'puÃ±o', as
	(String withAll: ('puño' AsUtf8Bytes asArray collect: [ :n | Character numericValue: n ]))        =        'puÃ±o'
	Senders MUST do appropriate conversion.
	
	Consider calling #primFixedNameLookupEntryIn:index: instead.
	"

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 5/23/2022 15:12:22'!
primRename: oldFileFullNameAsUtf8Bytes to: newFileFullNameAsUtf8Bytes
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 5/26/2022 09:29:39'!
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
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isStringLike or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
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
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'private' stamp: 'jmv 5/23/2022 15:16:10'!
retrieveIdOrStringFromOS: stringClass
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	utf8Bytes _ utf8Bytes fixLineEndings.

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray."
	utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
		ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
	^ stringClass fromUtf8Bytes: utf8Bytes.! !


!DropFilesEvent methodsFor: 'files' stamp: 'jmv 5/23/2022 15:16:14'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | utf8Bytes |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :primUtf8Bytes |
		utf8Bytes _ primUtf8Bytes.
		utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
			ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
			ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
		aBlock value: (String fromUtf8Bytes: utf8Bytes) ]].! !

!methodRemoval: Utf8String class #fromUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
Utf8String class removeSelector: #fromUtf8:!
!methodRemoval: Utf8String class #fromUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
Utf8String class removeSelector: #fromUtf32:!
!methodRemoval: Utf8String #asUtf32 stamp: 'jmv 5/26/2022 09:38:57'!
Utf8String removeSelector: #asUtf32!
!methodRemoval: Utf8String #asUtf8 stamp: 'jmv 5/26/2022 09:37:58'!
Utf8String removeSelector: #asUtf8!
!methodRemoval: Utf8String #asUtf8Bytes: stamp: 'jmv 5/26/2022 09:38:57'!
Utf8String removeSelector: #asUtf8Bytes:!
!methodRemoval: Utf8String #asUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
Utf8String removeSelector: #asUtf8:!
!methodRemoval: Utf8String #asUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
Utf8String removeSelector: #asUtf32:!
!methodRemoval: Utf8String #asUtf32Words: stamp: 'jmv 5/26/2022 09:39:59'!
Utf8String removeSelector: #asUtf32Words:!
!methodRemoval: Utf32String class #fromUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
Utf32String class removeSelector: #fromUtf8:!
!methodRemoval: Utf32String class #fromUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
Utf32String class removeSelector: #fromUtf32:!
!methodRemoval: Utf32String #asUtf32 stamp: 'jmv 5/26/2022 09:38:57'!
Utf32String removeSelector: #asUtf32!
!methodRemoval: Utf32String #asUtf8Bytes: stamp: 'jmv 5/26/2022 09:38:57'!
Utf32String removeSelector: #asUtf8Bytes:!
!methodRemoval: Utf32String #asUtf8 stamp: 'jmv 5/26/2022 09:37:58'!
Utf32String removeSelector: #asUtf8!
!methodRemoval: Utf32String #asUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
Utf32String removeSelector: #asUtf8:!
!methodRemoval: Utf32String #asUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
Utf32String removeSelector: #asUtf32:!
!methodRemoval: Utf32String #asUtf32Words: stamp: 'jmv 5/26/2022 09:39:52'!
Utf32String removeSelector: #asUtf32Words:!
!methodRemoval: String class #fromUtf32:hex:trimLastNull: stamp: 'jmv 5/26/2022 09:38:57'!
String class removeSelector: #fromUtf32:hex:trimLastNull:!
!methodRemoval: String class #fromUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
String class removeSelector: #fromUtf8:!
!methodRemoval: String class #fromUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
String class removeSelector: #fromUtf32:!
!methodRemoval: String class #fromUtf8:hex:trimLastNull: stamp: 'jmv 5/26/2022 09:37:58'!
String class removeSelector: #fromUtf8:hex:trimLastNull:!
!methodRemoval: String #asUtf32 stamp: 'jmv 5/26/2022 09:38:57'!
String removeSelector: #asUtf32!
!methodRemoval: String #asUtf8 stamp: 'jmv 5/26/2022 09:37:58'!
String removeSelector: #asUtf8!
!methodRemoval: String #asUtf32: stamp: 'jmv 5/26/2022 09:38:57'!
String removeSelector: #asUtf32:!
!methodRemoval: String #asUtf8: stamp: 'jmv 5/26/2022 09:37:58'!
String removeSelector: #asUtf8:!
