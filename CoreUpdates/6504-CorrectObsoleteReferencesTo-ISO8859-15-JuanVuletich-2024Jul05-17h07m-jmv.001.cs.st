'From Cuis7.1 [latest update: #6503] on 5 July 2024 at 5:26:42 pm'!

!String commentStamp: 'jmv 7/5/2024 17:07:37' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-1 (ISO 8859-1) encoding. Each character takes 1 byte.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See UnicodeString.!


!UniFileStream commentStamp: '<historical>' prior: 0!
A new FileStream for Cuis.

Writes bytes. Binary content is stored verbatim. String is stored in UTF-8, but invalid UTF-8 sequences are interpreted as Latin-1 Characters (i.e. ISO-8859-1).!


!Character commentStamp: 'jmv 7/5/2024 17:11:09' prior: 0!
I represent a character by storing its associated Latin-1 code (ISO 8859-1). My instances are immediate, so that all occurrences of a character ($R, for example) are identical.!


!String methodsFor: 'converting' stamp: 'jmv 7/5/2024 17:09:33'!
asCodePoints
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin-1 (ISO 8859-1).
	Answer a WordArray."

	| inStream codePoint |
	^WordArray streamContents: [ :outStream |
		inStream := self readStream.
		[ inStream atEnd ] whileFalse: [
			codePoint := inStream next codePoint.
			outStream nextPut: codePoint ]]! !

!String methodsFor: 'converting' stamp: 'jmv 7/5/2024 17:09:13'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin-1 (ISO 8859-1)
	Answer a ByteArray with a valid UTF-8 sequence."

	| inStream codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream := self readStream.
		[ inStream atEnd ] whileFalse: [
			codePoint := inStream next codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: codePoint ]]! !


!String class methodsFor: 'instance creation' stamp: 'jmv 7/5/2024 17:09:53'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin-1 (ISO 8859-1).
	Discard any code points that can not be represented.
	Use with caution. If in doubt, send this message to CharacterSequence instead."

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self fromUtf8Bytes: aByteArray trimLastNull: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 7/5/2024 17:10:13'!
fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin-1 (ISO 8859-1).
	Discard any code points that can not be represented."

	^String streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					codePoint < 256 ifTrue: [
						strm nextPut: (Character codePoint: codePoint) ]].
			byteIndex := byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!UnicodeString methodsFor: 'comparing' stamp: 'jmv 7/5/2024 17:20:45'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859-1 non ASCII characters are present.
	Note: will answer different values for UnicodeString's  that are equal if #canonicalDecomposition is taken."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 7/5/2024 17:12:28'!
fromBytesStream: aByteStream
	"Handle UTF-8 bytes, but convert invalid UTF-8 assuming it means ISO-8859-1.
	Argument can be, for example, a InflateStream."

	| utf8 codePointCount buffer readBytes leftover byteIndex n |
	buffer := ByteArray new: 10000.
	leftover := nil.
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[aByteStream atEnd] whileFalse: [ 
			readBytes := aByteStream nextInto: buffer.
			leftover ifNotNil: [
				readBytes := leftover, readBytes.
				leftover := nil ].
			byteIndex := 1.
			[ byteIndex <= (aByteStream atEnd
						ifFalse: [readBytes size-3] "To be sure we don't break a possible partially read multibyte code point."
						ifTrue: [readBytes size])   						"But do taks _all_ the bytes if they are the last ones."
			] whileTrue: [
				n := Character utf8BytesAndCodePointAt: byteIndex in: readBytes
					into: [ :byte1 :byte2 :byte3 :byte4 |
						utf8Stream nextPut: byte1.
						byte2 ifNotNil: [
							utf8Stream nextPut: byte2.
							byte3 ifNotNil: [
								utf8Stream nextPut: byte3.
								byte4 ifNotNil: [
									utf8Stream nextPut: byte4 ]]].
						codePointCount := codePointCount + 1]
					into: nil.
				byteIndex := byteIndex + n ].
			leftover := byteIndex > readBytes size ifFalse: [
				readBytes copyFrom: byteIndex to: readBytes size ].
		].
	].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/5/2024 17:13:55'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-1 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition := self position.
	bytesConsumed := 0.
	self nextByte ifNotNil: [ :byte1 |
		bytesConsumed := Character
			utf8BytesAndCodePointFor: byte1
			byte2: self nextByte
			byte3: self nextByte
			byte4: self nextByte
			into: bytesBlock
			into: codePointBlock ].
	self position: previousPosition + bytesConsumed.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/5/2024 17:16:29'!
skipBack
	| savePos try |
	savePos := self position.

	"Try going back one byte. If ASCII, then OK, we are done.
	If not, don't assume ISO-8859-1 so lightly!!"
	try := savePos-1.
	self position: try.
	self nextByte < 128 ifTrue: [
		^self position: try ].

	"Try going back two bytes. If doing #next leaves us at savePosition, we found a two byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back three bytes. If doing #next leaves us at savePosition, we found a three byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back four bytes. If doing #next leaves us at savePosition, we found a four byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Havent found a valid UTF-8 sequence that ends just before savePos.
	It means back byte is an ISO-8859-1 Character"
	self position: savePos-1.
	! !

!UniFileStream methodsFor: 'accessing' stamp: 'jmv 7/5/2024 17:13:40'!
contents
	"Return the full contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class).
	Stream bytes as CodePoints (prefer UTF-8, but ISO-8859-1 if invalid UTF-8)."

	| savePos answer |
	savePos := self position.
	self position: 0.
	answer := self upToEnd.
	self position: savePos.
	^answer! !


!Character methodsFor: 'accessing' stamp: 'jmv 7/5/2024 17:16:46'!
asciiValue
	"Answer the value of the receiver that represents its ANSI encoding.
	This is the same as ISO 8859-1 for the first 128 characters.
	Answer nil if out of range."

	| n |
	n := self codePoint.
	^ n < 128 ifTrue: [ n ].! !

!Character methodsFor: 'converting' stamp: 'jmv 7/5/2024 17:17:06'!
withDiacriticalMark: codePointOrCharacter
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-1, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$Á withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i := ((codePointOrCharacter isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $° $v)]) indexOf: codePointOrCharacter
				) + 1.
	answer := (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer = $- ifFalse: [answer] ifTrue: [self]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 7/5/2024 17:16:39'!
accentedLetters
	"Answer all letters with all possible accents (combining diacritical marks).
	Order of this array is:
		unaccented
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
	Do not include letters that can't have any accent in ISO 8859-1
	Denote unsupported combinations (for example A with caron, or N with accute accent) by a $-"
self flag: #unicodeNeedsImprovement.
	^#(
		'AÀÁÂÃÄÅ-'
		'aàáâãäå-'
		'EÈÉÊ-Ë--'
		'eèéê-ë--'
		'IÌÍÎ-Ï--'
		'iìíî-ï--'
		'OÒÓÔÕÖ--'
		'oòóôõö--'
		'UÙÚÛ-Ü--'
		'uùúû-ü--'
		'N---Ñ---'
		'n---ñ---'
		'S------Š'
		's------š'
		'Y-Ý--Ÿ--'
		'y-ý--ÿ--'
		'Z------Ž'
		'z------ž'
	).! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 7/5/2024 17:16:51'!
uppercaseLowercaseAndUnaccentedLetters
	"Answer all uppercase and lowercase letters in standard collation order.
	For accented characters, include the non-accented ones.
	Note that $ß is 'LATIN SMALL LETTER SHARP S' and ISO 8859-1 doesn't have an uppercase version.
	Note: for $ÿ, 'Latin Small Letter Y with diaeresis' the uppercase version is not Latin 1"
self flag: #unicodeNeedsImprovement.
	^#(
		'Aa' 'ÀàAa' 'ÁáAa' 'ÂâAa' 'ÃãAa' 'ÄäAa' 'ÅåAa'
		'Bb' 'Cc' 'Çç' 'Dd'
			'Ee' 'ÈèEe' 'ÉéEe' 'ÊêEe' 'ËëEe'
		'Ff' 'Gg' 'Hh'
			'Ii' 'ÌìIi' 'ÍíIi' 'ÎîIi' 'ÏïIi'
		'Jj' 'Kk' 'Ll' 'Mm' 'Nn' 'Ññ'
			'Oo' 'ÒòOo' 'ÓóOo' 'ÔôOo' 'ÕõOo' 'ÖöOo'
		'Pp' 'Qq' 'Rr'
			'Ss'
		'Tt'
			'Uu' 'ÙùUu' 'ÚúUu' 'ÛûUu' 'ÜüUu'
		'Vv' 'Ww' 'Xx'
			'Yy' 'ÝýYy' 'ÿ'
			'Zz'
		'Ææ' 'Ðð' 'Øø' 'Þþ' 'ß'
	).! !

!Character class methodsFor: 'converting' stamp: 'jmv 7/5/2024 17:21:06'!
utf8BytesAndCodePointAt: byteIndex in: aByteArray into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in utf8ByteArray starting at byte byteIndex.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-1 Character (i.e. Latin-1), and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| i s byte1 byte2 byte3 byte4 |
	
	s := aByteArray size.
	i := byteIndex.
	byte1 := aByteArray at: i.
	i := byteIndex+1.
	byte2 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+2.
	byte3 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+3.
	byte4 := i <= s ifTrue: [aByteArray at: i].

	^self
		utf8BytesAndCodePointFor: byte1
		byte2: byte2
		byte3: byte3
		byte4: byte4
		into: bytesBlock
		into: codePointBlock! !


!ByteStringReadAdaptor methodsFor: 'reading' stamp: 'jmv 7/5/2024 17:17:48'!
next
	"Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters."

	stream nextByte ifNotNil: [ :byte |
		^Character codePoint: byte ].
	^nil! !

!ByteStringReadAdaptor methodsFor: 'reading' stamp: 'jmv 7/5/2024 17:18:08'!
next: n
	"Read n bytes, or until the end of the file.
	Answer them as seen from our perspective:
	Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters.
	Answer is a String (i.e. a byte oriented ISO 8859-1 String)"

	^(stream nextBytes: n) asString! !

!ByteStringReadAdaptor methodsFor: 'reading' stamp: 'jmv 7/5/2024 17:18:20'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters.
	Leave stream at the end position."

	^stream bytesUpToEnd asString! !


!UnicodeReadAdaptor methodsFor: 'reading' stamp: 'jmv 7/5/2024 17:15:36'!
next: n
	"Read n Characters (i.e. n Unicode CodePoints), or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-1 if invalid UTF-8).
	Answer is an instance of UnicodeString."

	| utf8 codePointCount |
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[ codePointCount = n or: [stream atEnd] ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4 ]]].
					codePointCount := codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !

!UnicodeReadAdaptor methodsFor: 'reading' stamp: 'jmv 7/5/2024 17:15:43'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-1 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 codePointCount |
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[ stream atEnd ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4]]].
					codePointCount := codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 7/5/2024 17:18:56'!
codeManagementInCuisContents
	^ self class firstCommentAt: #codeManagementInCuisContents

"
Managing your code in Cuis
==================

(https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CodeManagementInCuis.md)

Cuis includes tools and procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in Packages. New code that are meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any Package, and is therefore automatically stored in ChangeSets.


Packages
-----------

Let's start with Packages. The Package implementation in Cuis is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages (.pck.st files) in Cuis, use the FileList, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on [Install Package].

Cuis includes a tool to manage installed Packages. It is at World / Open / Installed Packages. To create a new package (instead of installing an existing one from a file), click on [Create Package] This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

[Save] Saves a package on the file system. Overwrites any existing version. It is good to save the package from time to time, to reduce the risk of losing code.

[Delete] Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

[Browse unsaved Changes] This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

[Browse package code] This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

[Add requirement] This opens a select list of loaded packages.  Each package provides a Feature.  You can CANCEL, require the current Cuis base version (at a minimum) or require any of the packages on the list.  Required packages will be loaded before the selected package (Feature require: #'your-package'.).  When a package is selected, the lower browser pane shows its requirents, which may be deleted.  Don't forget to Save your package after adding or deleting  requirements!!

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are Package files (.pck.st). Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-Smalltalk-', so it will be easy for anybody to find it. Cuis Package files are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in UTF-8. This means they are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a file-based development environment such as Eclipse or a text editor. Like Cuis, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.


Changes to the Cuis base image
-----------------------------------------

The way ChangeSets are created and managed in Cuis is different from Squeak. This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Packages out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) to the Cuis Core image. Such code is automatically captured in a ChangeSet. The ChangeSet for Core changes is created automatically and named like '1243-CuisCore-JuanVuletich-2012Apr03-22h50m'. The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The 'CuisCore' part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These ChangeSets are created automatically. There is no longer a way to manually create them, or make them 'current' or 'active'. It is best to rename them, replacing 'CuisCore' with some meaningful name. These ChangeSets will not capture any code that belongs in a Package.

Opening a Change Sorter will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper Package or ChangeSet, simply following the class or method category. Gone are the days of messed up change sets and lost code!!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the ChangeSet on disk. It also removes it from the ChangeSorter (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore ChangeSets are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.

Installing ChangeSet files into Cuis

[Install] loads all the code in the file into a separate, new ChangeSet object (viewable in the ChangeSorter tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet used to record the changes we make to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!!

Cherry picking individual changes from ChangeSet or Package files
-----------------------------------------------------------------------------------

Additionally, you can study a Package (.pck.st) or ChangeSet (.cs) file without installing it. To do this, use the FileList, navigate to the appropriate directory, select the file and click on [Contents]. You will get a ChangeList tool with the contents of the file. You can select each change, to see the code, and compare it with what is currently loaded in the system (if that is the case). You can also various filters on the list. See the right-click menu. Once you have one or more changes selected, you can do right-click / 'fileIn selections'. Changes that belong in a package that is already there will be captured by that package, that will now be dirty. Code that doesn't belong in a loaded package will be included in the current ChangeSet, together with code you save in a Browser. A new Package or ChangeSet will not be created. This is especially useful when reviewing code, or when we are combining code from more than one source into a single ChangeSet or Package.
"

"
Utilities codeManagementInCuisContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 7/5/2024 17:24:58'!
cuisAndGitHubContents
	^ self class firstCommentAt: #cuisAndGitHubContents

"
Using Git and GitHub to host and manage Cuis code
===================================

(https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CuisAndGitHub.md)

Cuis includes tools and procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck). But Cuis doesn't do version control. Instead, we suggest using external VCS tools.  In particular, we're using [GitHub](http://www.github.com/), and the first project we're hosting there is [StyledTextEditor](https://github.com/bpieber/Cuis-StyledTextEditor).

The guiding principle is to *not duplicate concepts and behavior*. As we're using an external tool (Git) for version control, then we use it as it meant to be used. Most people use Git for version control and a file based IDE such as Eclipse for development. Such IDEs don't do version control themselves. It is done by Git. Do the same: do not include package version control in Cuis. This is a departure from the Monticello /Git integration (smallsource and MonticelloFileTree) by Otto Behrens, Dale Henrichs, etc.

We use GitHub to host, version, diff and merge external packages (.pck files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis code files are encoded in UTF-8, and use the LF (ascii code 10) newline convention. This allows Git/GitHub to diff versions, and merge branches.

Each GitHub repository has one set of users and permissions. Each GitHub repository has one state (Git commits repositories, not individual files). Branch and merges are done on the whole repository and not on individual files. Therefore, we need a separate GitHub repository for each project, i.e., for each package or set of closely related packages that are always loaded and maintained together as a whole.

Development process for External Packages
--------------------------------------------------------

This is the suggested procedure for developing external packages. Usually do this every day.

* Start with a standard (i.e. fresh) Cuis image. Never save the image.

* Set up Git repositories for external packages (if not already done)

* Install packages from Git repositories.

* Develop. Modify and/or create packages.

* Save own packages (to Git repositories).

* Git add / commit / push as appropriate.

* Fileout changes that are not part of any package. These are automatically captured in numbered changesets, separated from changes to packages.

* Exit the image. Usually without saving.
"

"
Utilities cuisAndGitHubContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 7/5/2024 17:22:58'!
usefulExpressionsContents
	^ self class firstCommentAt: #usefulExpressionsContents

"'Querying the system'.
Smalltalk aboutThisSystem.
SystemVersion current.
SystemDictionary copyright.
SystemOrganization categories.
SystemOrganization categoryOfElement: #Morph.
Editor organization categoryOfElement: #clearSelection.
Smalltalk allUnSentMessages.
Smalltalk browseAllUnSentMessages.
Smalltalk browseAllUnimplementedCalls.


'find selectors
Remember you can also do World / Open / Method Names'.
Symbol selectorsContaining: 'rsCon'.
'Cuis uses ISO 8859-1 for String and UTF8 UnicodeString, Clipboard and Files'.
Symbol selectorsContaining: 'iso8859'.
Symbol selectorsContaining: 'utf8'. 


'Example of loading a package with requirements on other packages'.
Feature require: 'SignalProcessing'.


'To create a *.pck.st file for each category with tests and  then to remove these categories'.
(SystemOrganization categoriesMatching: '*Test*')
	do: [ :cat | (CodePackage named: cat
				createIfAbsent: true
				registerIfNew: false) save.
			SystemOrganization removeSystemCategory: cat.
	].


'List of CodePackages which are not saved and save them in one go'.
'Save dirty packages:' print.
CodePackage installedPackages do: [ :each |
	each hasUnsavedChanges ifTrue: [
		each name print.
		each save ]].


'Globals'.
Cursor wait showWhile: [Sensor waitButton].
Smalltalk  inspectGlobals.


'Source code management'.
ChangeList browseRecentLog.


'Space and object count statistics'.
Smalltalk bytesLeft printStringWithCommas.
Symbol instanceCount.


'Anaylize memory usage by class'.
(String streamContents: [ :strm | SpaceTally new printSpaceAnalysis: 1 on: strm ]) edit.


'Remove a lot of stuff and reduce the image'.
Smalltalk reduceCuis.


'Performance measurement'.
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].
[Smalltalk allCallsOn: #asOop] bench.


'Opening a Text editor'.
'Something' editLabel: 'Title'.


'Opening a workspace'.
Workspace new
		contents: '';
		openLabel: 'Workspace'.

'Morphic stuff'.
LayoutMorph example1.				'Resize it'.
LayoutMorph example11.			'Resize it. Play with separators.'.
LayoutMorph example2.				'Resize it'.
LayoutMorph example3.				'Resize it'.
LayoutMorph example13.			'Resize it. Play with separators.'.
LayoutMorph example6.				'Resize it'.
LayoutMorph launcherExample.		'Have a Transcript open to use this'.

'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !

