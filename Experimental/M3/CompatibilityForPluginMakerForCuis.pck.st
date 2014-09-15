'From Cuis 4.2 of 25 July 2013 [latest update: #1780] on 30 July 2013 at 1:13:41.806454 pm'!
'Description Please enter a description for this package '!
!provides: 'CompatibilityForPluginMakerForCuis' 1 0!
!classDefinition: #SmalltalkImage category: #CompatibilityForPluginMakerForCuis!
Object subclass: #SmalltalkImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompatibilityForPluginMakerForCuis'!
!classDefinition: 'SmalltalkImage class' category: #CompatibilityForPluginMakerForCuis!
SmalltalkImage class
	instanceVariableNames: ''!

!classDefinition: #CrLfFileStream category: #CompatibilityForPluginMakerForCuis!
StandardFileStream subclass: #CrLfFileStream
	instanceVariableNames: 'lineEndConvention'
	classVariableNames: 'Cr CrLf Lf LineEndDefault LineEndStrings LookAheadCount'
	poolDictionaries: ''
	category: 'CompatibilityForPluginMakerForCuis'!
!classDefinition: 'CrLfFileStream class' category: #CompatibilityForPluginMakerForCuis!
CrLfFileStream class
	instanceVariableNames: ''!

!classDefinition: #ClassTestCase category: #CompatibilityForPluginMakerForCuis!
TestCase subclass: #ClassTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CompatibilityForPluginMakerForCuis'!
!classDefinition: 'ClassTestCase class' category: #CompatibilityForPluginMakerForCuis!
ClassTestCase class
	instanceVariableNames: ''!


!CrLfFileStream commentStamp: 'ls 11/10/2002 13:32' prior: 0!
I am the same as a regular file stream, except that when I am in text mode, I will automatically convert line endings between the underlying platform's convention, and Squeak's convention of carriage-return only.  The goal is that Squeak text files can be treated as OS text files, and vice versa.

In binary mode, I behave identically to a StandardFileStream.

To enable CrLfFileStream as the default file stream class for an entire image, modify FileStream class concreteStream .


There are two caveats on programming with CrLfFileStream.

First, the choice of text mode versus binary mode affects which characters are visible in Squeak, and no longer just affects whether those characters are returned as Character's or as Integer's.  Thus the choice of mode needs to be made very carefully, and must be based on intent instead of convenience of representation.  The methods asString, asByteArray, asCharacter, and asInteger can be used to convert between character and integer representations.  (Arguably, file streams should accept either strings or characters in nextPut: and nextPutAll:, but that is not the case right now.)

Second, arithmetic on positions no longer works, because one character that Squeak sees (carriage return) could map to two characters in the underlying file (carriage return plus line feed, on MS Windows and MS DOS).  Comparison between positions still works.  (This caveat could perhaps be fixed by maintaining a map between Squeak positions and positions in the underlying file, but it is complicated.  Consider, for example, updates to the middle of the file.  Also, consider that text files are rarely updated in the middle of the file, and that general random access to a text file is rarely very useful.  If general random access with specific file counts is desired, then the file is starting to sound like a binary file instead of a text file.)

!

!String methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 16:11'!
findLastOccuranceOfString: subString startingAt: start 
	^self findLastOccurrenceOfString: subString startingAt: start ! !

!String methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 16:10'!
withSqueakLineEndings
	^self withCuisLineEndings! !

!String class methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 16:08'!
new: n streamContents: aBlock
	^self streamContents: aBlock! !

!EventSensor methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 15:49'!
cursorPoint
	^self peekMousePt ! !

!WriteStream methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 15:46'!
cr
	^self newLine! !

!WriteStream methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 15:50'!
crtab
	^self newLine; tab! !

!WriteStream methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 15:51'!
crtab: n
	self newLine; tab: n! !

!Transcript class methodsFor: '*CompatibilityForPluginMakerForCuis' stamp: 'jmv 7/30/2012 15:46'!
cr
	^self newLine! !

!SmalltalkImage class methodsFor: 'compatibility' stamp: 'jmv 3/23/2010 17:09'!
current
	^Smalltalk! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:16'!
ascii
	super ascii.
	self detectLineEndConvention! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
binary
	super binary.
	lineEndConvention := nil! !

!CrLfFileStream methodsFor: 'private' stamp: 'stephaneducasse 2/4/2006 20:31'!
convertStringFromCr: aString 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Cr with: Lf].
	"lineEndConvention == #crlf"
	inStream := ReadStream on: aString.
	outStream := WriteStream on: (String new: aString size).
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: [outStream nextPutAll: CrLf]].
	^ outStream contents! !

!CrLfFileStream methodsFor: 'private' stamp: 'stephaneducasse 2/4/2006 20:31'!
convertStringToCr: aString 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Lf with: Cr].
	"lineEndConvention == #crlf"
	inStream := ReadStream on: aString.
	outStream := WriteStream on: (String new: aString size).
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: 
					[outStream nextPut: Cr.
					inStream peek = Lf ifTrue: [inStream next]]].
	^ outStream contents! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
detectLineEndConvention
	"Detect the line end convention used in this stream. The result may be either #cr, #lf or #crlf."
	| char numRead pos |
	self isBinary ifTrue: [^ self error: 'Line end conventions are not used on binary streams'].
	lineEndConvention := LineEndDefault.
	"Default if nothing else found"
	numRead := 0.
	pos := super position.
	[super atEnd not and: [numRead < LookAheadCount]]
		whileTrue: 
			[char := super next.
			char = Lf
				ifTrue: 
					[super position: pos.
					^ lineEndConvention := #lf].
			char = Cr
				ifTrue: 
					[super peek = Lf
						ifTrue: [lineEndConvention := #crlf]
						ifFalse: [lineEndConvention := #cr].
					super position: pos.
					^ lineEndConvention].
			numRead := numRead + 1].
	super position: pos.
	^ lineEndConvention! !

!CrLfFileStream methodsFor: 'access' stamp: 'nk 9/5/2004 12:58'!
lineEndConvention

	^lineEndConvention! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
next
    | char secondChar |
    char := super next.
    self isBinary ifTrue: [^char].
    char == Cr ifTrue:
        [secondChar := super next.
        secondChar ifNotNil: [secondChar == Lf ifFalse: [self skip: -1]].
        ^Cr].
    char == Lf ifTrue: [^Cr].
    ^char! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
next: n

		| string peekChar |
		string := super next: n.
		string size = 0 ifTrue: [ ^string ].
		self isBinary ifTrue: [ ^string ].

		"if we just read a CR, and the next character is an LF, then skip the LF"
		( string last = Character cr ) ifTrue: [
			peekChar := super next.		"super peek doesn't work because it relies on #next"
			peekChar ~= Character lf ifTrue: [
				super position: (super position - 1) ]. ].
 
		string := string withSqueakLineEndings.

		string size = n ifTrue: [ ^string ].

		"string shrunk due to embedded crlfs; make up the difference"
		^string, (self next: n - string size)! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
nextPut: char 
	(lineEndConvention notNil and: [char = Cr])
		ifTrue: [super nextPutAll: (LineEndStrings at: lineEndConvention)]
		ifFalse: [super nextPut: char].
	^ char! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
nextPutAll: aString 
	super nextPutAll: (self convertStringFromCr: aString).
	^ aString
! !

!CrLfFileStream methodsFor: 'open/close' stamp: 'stephaneducasse 2/4/2006 20:31'!
open: aFileName forWrite: writeMode 
	"Open the receiver.  If writeMode is true, allow write, else access will be 
	read-only. "
	| result |
	result := super open: aFileName forWrite: writeMode.
	result ifNotNil: [self detectLineEndConvention].
	^ result! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next pos |
	self atEnd ifTrue: [^ nil].
	pos := self position.
	next := self next.
	self position: pos.
	^ next! !

!CrLfFileStream methodsFor: 'access' stamp: 'stephaneducasse 2/4/2006 20:31'!
upTo: aCharacter
	| newStream char |
	newStream := WriteStream on: (String new: 100).
	[(char := self next) isNil or: [char == aCharacter]]
		whileFalse: [newStream nextPut: char].
	^ newStream contents
! !

!CrLfFileStream methodsFor: 'access' stamp: 'nice 12/7/2009 08:26'!
upToAnyOf: delimiters do: aBlock

	^String new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ (delimiters includes: ch) and: [aBlock value: ch. true] ] ] 
			whileFalse: [ stream nextPut: ch ] ]! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
verbatim: aString 
	super verbatim: (self convertStringFromCr: aString).
	^ aString! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToCR
	"CrLfFileStream defaultToCR"
	LineEndDefault := #cr.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToCRLF
	"CrLfFileStream defaultToCRLF"
	LineEndDefault := #crlf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToLF
	"CrLfFileStream defaultToLF"
	LineEndDefault := #lf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'nice 11/27/2009 21:56'!
guessDefaultLineEndConvention
	"Lets try to guess the line end convention from what we know about the
	path name delimiter from FileDirectory."
	FileDirectory pathNameDelimiter = $:
		ifTrue: [^ self defaultToCR].
	FileDirectory pathNameDelimiter = $/
		ifTrue: [((SmalltalkImage current osVersion) beginsWith: 'darwin')
				ifTrue: [^ self defaultToCR]
				ifFalse: [^ self defaultToLF]].
	FileDirectory pathNameDelimiter = $\
		ifTrue: [^ self defaultToCRLF].
	"in case we don't know"
	^ self defaultToCR! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'di 2/4/1999 09:16'!
initialize
	"CrLfFileStream initialize"
	Cr := Character cr.
	Lf := Character lf.
	CrLf := String with: Cr with: Lf.
	LineEndStrings := Dictionary new.
	LineEndStrings at: #cr put: (String with: Character cr).
	LineEndStrings at: #lf put: (String with: Character lf).
	LineEndStrings at: #crlf put: (String with: Character cr with: Character lf).
	LookAheadCount := 2048.
	Smalltalk addToStartUpList: self.
	self startUp.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'djp 1/28/1999 22:08'!
startUp
	self guessDefaultLineEndConvention! !
CrLfFileStream initialize!
