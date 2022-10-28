'From Cuis 6.0 [latest update: #5527] on 26 October 2022 at 3:57:55 pm'!

!String commentStamp: 'jmv 10/26/2022 15:51:55' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-9 (ISO 8859-15) encoding. Each character takes 1 byte.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See Utf8String, that supports Unicode.!


!String class methodsFor: 'instance creation' stamp: 'jmv 10/26/2022 15:43:16'!
fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	^String streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					(Character codePoint: codePoint) ifNotNil: [ :char | strm nextPut: char ]].
			byteIndex _ byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!Clipboard methodsFor: 'private' stamp: 'jmv 10/26/2022 15:35:45'!
retrieveIdOrStringFromOS
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
	^ Utf8String fromUtf8Bytes: utf8Bytes.! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 10/26/2022 15:16:50'!
codePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character codePoint: 16r41) = $A.
	self assert: (Character codePoint: 16r20AC) = $€.
	"
	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
		ifNotNil: [ :code | Character numericValue: code ]! !


!String methodsFor: 'converting' stamp: 'jmv 10/26/2022 15:50:45'!
asUtf32Words
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15).
	Answer a WordArray."

	| inStream nextChar codePoint |
	^WordArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			outStream nextPut: codePoint ]]! !

!String methodsFor: 'converting' stamp: 'jmv 10/26/2022 15:49:03'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray with a valid UTF-8 sequence."

	| inStream nextChar codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!String class methodsFor: 'instance creation' stamp: 'jmv 10/26/2022 15:48:17'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self fromUtf8Bytes: aByteArray trimLastNull: false! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 10/26/2022 15:24:33'!
computeHash
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"
	| basicHash byteString |
	basicHash _ self basicHash.
	self isAscii ifTrue: [
		^basicHash ].
	"If there's a String that is equal to us (i.e. no characters were lost during conversion)"
	byteString _ self asByteString.
	byteString size = self size ifTrue: [
		^byteString hash ].
	"Otherwise, use a hash that doesn't need to equal that of a String"
	^basicHash
! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 10/26/2022 15:52:48'!
asByteString
	"Answer an instance of the Byte oriented String class"

	"Fast lane."
	self isAscii ifTrue: [
		^bytes asString ].

	"Need to do UTF-8 decoding."
	^super asByteString! !


!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 10/26/2022 15:55:49'!
nextPutAll: aStringOrUTF8Bytes
	"Write the given String  or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes _ aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: 'Лорем ипсум долор сит амет, дебет салутанди меи еи. Ан хис граеце утамур долорум, лорем принципес молестиае дуо ин. Еи мовет ностро оптион нам, еос епицури омиттантур ут. Ут мунере лаборе интеллегебат усу, еа сед дебет луптатум, видит иллуд иудицо яуо ут.'; flush
"! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 10/26/2022 15:30:46'!
clipboardStringOrText

	| clipContents |
	clipContents _ Clipboard retrieveStringOrText.
	^ ((clipContents is: #Text) and: [ clipContents respondsTo: #asNonStyledText ])
		ifTrue: [ clipContents asNonStyledText ]
		ifFalse: [ clipContents ]! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/26/2022 15:31:51'!
newLine: aKeyboardEvent

	self addString: (Utf8String with: Character newLineCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/26/2022 15:32:06'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self addString: (Utf8String with: aKeyboardEvent keyCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/26/2022 15:32:26'!
returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: Utf8String crString.
			^false ].
		self addString: Utf8String crlfString.
		^false ].
	^ self newLine: aKeyboardEvent! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 10/26/2022 15:32:44'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (Utf8String with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (Utf8String with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 10/26/2022 15:33:11'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection.
	This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart size inStream outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	size _  stop + 1 - start.
	inStream _ self privateCurrentString readStreamFrom: start to: stop.
	outStream _ Utf8String writeStream.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: outStream contents.
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/26/2022 15:32:53'!
firstOfBeginningOfLineOrEndOfIndentationLeftOf: position
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine _ textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine _ currentLine first.
	endOfIndentation _ self privateCurrentString
		skipDelimiters: (Utf8String with: Character tab)
		startingAt: beginningOfLine.
		
	stops _ OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/26/2022 15:33:21'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^Utf8String streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: (CodeProvider normalizeCode: aSourceCode)		]! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/26/2022 15:33:26'!
newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString _ Utf8String streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/26/2022 15:33:30'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| stopIndex startIndex string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	stopIndex _ self pointIndex - 1.
	(stopIndex between: 2 and: string size)
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	startIndex _ stopIndex.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	[startIndex > 0 and: [stopIndex - startIndex < 20 and: [(string at: startIndex) isLetter]]] whileTrue: [startIndex _ startIndex - 1].
	(startIndex > 0 and: [(string at: startIndex) = $\])
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	key _ string copyFrom: startIndex+1 to: stopIndex.
	(UnicodeCodePoint namedCharactersTable detect: [:one| key = one first] ifNone: [])
		ifNotNil: [:aPair| self selectFrom: startIndex to: stopIndex; replaceSelectionWith: (Utf8String with: aPair second)].
	^ super normalCharacter: aKeyboardEvent! !


!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 10/26/2022 15:57:23'!
primFixedNameLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer _ self primLookupEntryIn: fullPathAsUtf8Bytes index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (Utf8String fromUtf8Bytes: answer first asByteArray) ].
	^answer! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 10/26/2022 15:34:39'!
retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph new image: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 10/26/2022 15:35:51'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil _ self retrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents or: [(contents is: #Text) and: [stringOrNil = contents string]]])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 10/26/2022 15:35:09'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |

	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: (Utf8String with: $*) attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ Utf8String new ] ifNotNil: [ answer printString ]! !


!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 10/26/2022 15:30:30'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	^self default retrieveStringOrText! !


!DropFilesEvent methodsFor: 'files' stamp: 'jmv 10/26/2022 15:38:09'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | utf8Bytes |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :primUtf8Bytes |
		utf8Bytes _ primUtf8Bytes.
		utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
			ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
			ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
		aBlock value: (Utf8String fromUtf8Bytes: utf8Bytes) ]].! !

!methodRemoval: Clipboard class #retrieveStringOrText: stamp: 'jmv 10/26/2022 15:30:52'!
Clipboard class removeSelector: #retrieveStringOrText:!
!methodRemoval: Clipboard #retrieveStringOrText: stamp: 'jmv 10/26/2022 15:30:59'!
Clipboard removeSelector: #retrieveStringOrText:!
!methodRemoval: Clipboard #retrieveObject: stamp: 'jmv 10/26/2022 15:35:25'!
Clipboard removeSelector: #retrieveObject:!
!methodRemoval: Clipboard #retrieveIdOrStringFromOS: stamp: 'jmv 10/26/2022 15:35:55'!
Clipboard removeSelector: #retrieveIdOrStringFromOS:!
!methodRemoval: TextEditor #stringClassToUse stamp: 'jmv 10/26/2022 15:28:54'!
TextEditor removeSelector: #stringClassToUse!
!methodRemoval: Editor #stringClassToUse stamp: 'jmv 10/26/2022 15:33:37'!
Editor removeSelector: #stringClassToUse!
!methodRemoval: Text #asByteStringOrText stamp: 'jmv 10/26/2022 15:23:26'!
Text removeSelector: #asByteStringOrText!
!methodRemoval: Utf32String class #fromUtf32Words: stamp: 'jmv 10/26/2022 15:21:22'!
Utf32String class removeSelector: #fromUtf32Words:!
!methodRemoval: String class #fromUtf32Words: stamp: 'jmv 10/26/2022 15:21:34'!
String class removeSelector: #fromUtf32Words:!
!methodRemoval: String class #addUnicodeCodePoint:to:hex: stamp: 'jmv 10/26/2022 15:43:32'!
String class removeSelector: #addUnicodeCodePoint:to:hex:!
!methodRemoval: String class #fromUtf8Bytes:hex:trimLastNull: stamp: 'jmv 10/26/2022 15:42:18'!
String class removeSelector: #fromUtf8Bytes:hex:trimLastNull:!
!methodRemoval: String class #fromUtf32Words:hex:trimLastNull: stamp: 'jmv 10/26/2022 15:22:05'!
String class removeSelector: #fromUtf32Words:hex:trimLastNull:!
!methodRemoval: String #asUtf8Bytes: stamp: 'jmv 10/26/2022 15:45:39'!
String removeSelector: #asUtf8Bytes:!
!methodRemoval: String #asUtf32Words: stamp: 'jmv 10/26/2022 15:45:38'!
String removeSelector: #asUtf32Words:!
!methodRemoval: CharacterSequence #asByteStringOrText stamp: 'jmv 10/26/2022 15:23:29'!
CharacterSequence removeSelector: #asByteStringOrText!
!methodRemoval: Character class #codePoint:trySimilar: stamp: 'jmv 10/26/2022 15:43:41'!
Character class removeSelector: #codePoint:trySimilar:!
