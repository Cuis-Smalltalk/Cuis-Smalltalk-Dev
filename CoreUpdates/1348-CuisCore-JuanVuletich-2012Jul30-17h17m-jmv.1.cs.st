'From Cuis 4.0 of 21 April 2012 [latest update: #1347] on 30 July 2012 at 5:26:56 pm'!

!Character class methodsFor: 'instance creation' stamp: 'jmv 4/28/2011 15:35'!
safeValue: asciiCodeOrCodePoint 
	"Answer the Character whose value is anInteger.
	Handle unicode code points > 255 without errors, trying to answer something reasonable"

	"Note: senders of #value:or: in '1002-RTFParser.cs' has many automatic conversion to ISO-8859-15 characters, that would be valuable here."
	(#(16r2019 16r201B) includes: asciiCodeOrCodePoint) ifTrue: [
		^$' ].
	(#(16r201C 16r201D 16r201F) includes: asciiCodeOrCodePoint) ifTrue: [
		^$" ].
	^(self unicodeCodePoint: asciiCodeOrCodePoint)
		ifNil: [Character value: 255 ]! !


!PositionableStream methodsFor: '*SVG-Morphic' stamp: 'jmv 4/27/2010 15:01'!
nextNumber
	"Answer a number from the stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $)]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self! !


!String methodsFor: 'converting' stamp: 'jmv 12/1/2009 15:20'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did inString |

	"Hack in some minimal workaround for Unicode stuff"
	inString _ self copyReplaceAll: 'â€™' with: $' asString.
	"Check if we can handle this in #safeValue: in some way..."
	inString = self ifFalse: [ self halt ].
	
	in _ ReadStream on: inString.
	out _ WriteStream on: (String new: self size).
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
						ifTrue: [rest _ in upTo: $;.
								did _ out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $¡].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest first = $# ifTrue: [ out nextPut: (Character value: rest asInteger) ].
								did = out position ifTrue: [
									out nextPut: $&; nextPutAll: rest.
									"self error: 'unknown encoded HTML char'."
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 7/30/2012 17:26'!
crLfNextLine
	"Answer next line (may be empty), or nil if at end.
	Support any line ending convention"

	| answer lineSeparators c |
	self atEnd ifTrue: [^nil].
	lineSeparators _ {Character crCharacter. Character lfCharacter}.
	answer _ self upToAny: lineSeparators.
	c _ self peek.
	c = Character crCharacter ifTrue: [self next].
	c = Character lfCharacter ifTrue: [self next].
	^answer! !

!PositionableStream methodsFor: '*SVG-Morphic' stamp: 'gvc 10/3/2005 10:14'!
upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given coellection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (collection species new: 100).
	[self atEnd or: [aCollection includes: (element := self next)]]
		whileFalse: [newStream nextPut: element].
	(aCollection includes: element)
		ifTrue: [self skip: -1].
	^newStream contents! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/30/2012 17:24'!
upToAny: aCollection 
	"Similar to upTo:, but find any of the objects in aCollection"
	| newStream element |
	newStream _ WriteStream on: (buffer1 species new: 100).
	[self atEnd or: [aCollection includes: (element _ self next)]]
		whileFalse: [newStream nextPut: element].
	(aCollection includes: element)
		ifTrue: [self skip: -1].
	^newStream contents! !


!SocketStream reorganize!
('stream in' next next: next:into: next:into:startingAt: nextAllInBuffer nextAvailable nextAvailable: nextInBuffer: nextInto: nextInto:startingAt: nextLine nextLineCrLf nextLineLf peek peek: peekFor: peekForAll: readInto:startingAt:count: skip: upTo: upTo:limit: upToAll: upToAll:limit: upToEnd)
('testing' atEnd isBinary isConnected isDataAvailable isEmpty isInBufferEmpty isOtherEndConnected shouldTimeout)
('accessing' socket timeout:)
('initialize-release' destroy initialize)
('private' adjustInBuffer: adjustOutBuffer: beSignalingWhile: checkFlush growInBuffer moveInBufferDown resetBuffers resizeInBuffer: streamBuffer:)
('stream out' newLine next:putAll:startingAt: nextPut: nextPutAll: nextPutAllFlush: sendCommand: space)
('configuration' ascii autoFlush autoFlush: binary bufferSize bufferSize: inBufferSize noTimeout outBufferSize shouldSignal shouldSignal: socket: timeout)
('control' close flush receiveData: recentlyRead)
('printing' debug print: printOn:)
('private-socket' receiveAvailableData receiveData receiveDataIfAvailable receiveDataInto:startingAt: sendData:count: signalClosed signalTimeout waitForData)
('*YAXO' pushBack:)
!


!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToEnd)
('testing' atEnd isBinary isEmpty notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: copyMethodChunkFrom: fileIn fileInAnnouncing: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('nonhomogeneous accessing' nextInt32 nextInt32Put: nextLittleEndianNumber: nextLittleEndianNumber:put: nextNumber: nextNumber:put: nextString nextStringOld nextStringPut: nextWord nextWordPut:)
('converting' asBinaryOrTextStream asZLibReadStream)
('data get/put' boolean boolean: int16 int16: int32 int32: string string: uint16 uint16: uint24 uint24: uint32 uint32:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
('*YAXO' pushBack:)
('*SVG-Morphic' nextNumber upToAny:)
!

