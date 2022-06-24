'From Cuis 6.0 [latest update: #5311] on 24 June 2022 at 10:56:09 am'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/24/2022 10:50:57'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^Utf32String fromUtf8Bytes: self asUtf8Bytes.! !


!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/24/2022 10:51:01'!
= aString
	"Answer whether the receiver sorts equally as anUtf32String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Expensive. Assume it is extremely unlikely."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	aString isUtf32String ifFalse: [
		^self asUtf8String = aString ].

	^ codePoints = aString words! !

!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/24/2022 10:51:05'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	self = aString ifTrue: [ ^true ].
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/24/2022 10:51:11'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive ifTrue: [
		^self findString: key startingAt: start ].

	self isAscii ifTrue: [
		key isAscii
			ifTrue: [ "Fast lane"
				^'' findStringIgnoreCase: key in: bytes startingAt: start ]
			ifFalse: [ "Won't be found"
				^0 ]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: key asLowercase startingAt: start! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:30'!
next: n
	"Return a string with the next n characters of the filestream in it.  1/31/96 sw"
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	^ self nextInto: (ByteArray new: n)! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:34'!
next: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
true ifTrue: [
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt. "ojo"
"si se que son bytes quizas preferir
nextBytes: n into: aBytesOrWordsObject startingAt: startIndex
"
].
	collection 
		ifNil: [ 
			newN := n.
			newStartIndex := startIndex ]
		ifNotNil: [
			aBytesOrWordsObject class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := n.
					newStartIndex := startIndex ]
				ifTrue: [
					| available |
					(available := readLimit - position) > 0 
						ifFalse: [ available := 0 ]
						ifTrue: [
							| bufferedCount |
							bufferedCount := n min: available.
							aBytesOrWordsObject
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = n ifTrue: [ ^aBytesOrWordsObject ] ].
					newN := n - available.
					newStartIndex := startIndex + available ] ].
	count := self primRead: fileID into: aBytesOrWordsObject
				startingAt: newStartIndex count: newN.
	count = newN
		ifTrue:[ ^aBytesOrWordsObject ]
		ifFalse:[ ^aBytesOrWordsObject copyFrom: 1 to: newStartIndex + count - 1 ]! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:38'!
next: anInteger putAll: aString startingAt: startIndex
	"Store the next anInteger elements from the given collection."
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	lastWrittenElement _ aString at: anInteger.
	^aString! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:42'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:45'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	pad := Character space numericValue.
	self nextPutAll: (ByteArray new: ((self size - self position) min: 20000) 
							withAll: pad)! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:48'!
readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"

self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	^(self next: count into: byteArray startingAt: startIndex) size - startIndex + 1
! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:51'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version to speed up nextChunk"

	| pos buffer count skipSeparator tryAgain |

self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	skipSeparator _ delimiterIsTerminator ifTrue: [0] ifFalse: [1].
	collection ifNotNil: [
		(position < readLimit and: [
			(pos _ collection indexOf: delimiter startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^ collection copyFrom: position + 1 to: (position _ pos) - skipSeparator ] ].

	pos _ self position.
	buffer _ self next: 2000.
	(count _ buffer indexOf: delimiter) > 0 ifTrue: [
		"Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - skipSeparator].

	self atEnd ifTrue: [
		"Never found it, and hit end of file"
		^ delimiterIsTerminator ifTrue: [self position: pos. nil] ifFalse: [buffer]].

	"Never found it, but there's more..."
	tryAgain _ self upTo: delimiter delimiterIsTerminator: delimiterIsTerminator.
	tryAgain ifNil: [
		self position: pos.
		^ nil ].
	^ buffer, tryAgain.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:54'!
withAttribute: aTextAttribute do: streamBlock
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:52:57'!
withAttributes: attributes do: streamBlock 
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !


!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 6/24/2022 10:52:26'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
	self halt! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:53:02'!
next: anInteger putAll: aString startingAt: startIndex
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.
	lastWrittenElement _ aString at: anInteger.
	"?
	numberOfCodePointsWritten ifNotNil: [
		numberOfCodePointsWritten _ numberOfCodePointsWritten + aString size ]]"! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:53:25'!
skip
	"Skips one object in the receiver's future sequence values."
"
	self next
"
"Should we actually implement this???"
self flag: #unicodeNeedsImprovement.
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:53:38'!
skip: n
	"Skips one object in the receiver's future sequence values."
"
	self next
"
self flag: #unicodeNeedsImprovement.
"Should we actually implement this???"
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:53:53'!
skipBack
	"Skips one object in the receiver's future sequence values."
"
	self next
"
self flag: #unicodeNeedsImprovement.
"Should we actually implement this???"
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:53:57'!
withAttribute: aTextAttribute do: streamBlock
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 10:54:00'!
withAttributes: attributes do: streamBlock 
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !

