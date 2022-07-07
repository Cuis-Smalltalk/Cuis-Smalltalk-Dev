'From Cuis 6.0 [latest update: #5364] on 7 July 2022 at 11:57:27 am'!

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:47:21'!
nextBytes: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
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


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:47:53'!
next: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	^self nextBytes: n into: aBytesOrWordsObject startingAt: startIndex! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:53:22'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

	self nextBytes: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:57:07'!
nextBytes: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
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

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 7/7/2022 11:53:44'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

	self nextBytes: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 7/7/2022 11:49:51'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer := ByteArray new: 50000.
	[fileStream1 atEnd] whileFalse: [
		fileStream2 nextPutAll: (fileStream1 nextBytes: buffer size into: buffer startingAt: 1)].! !

!methodRemoval: UniFileStream #next:into:startingAt: stamp: 'jmv 7/7/2022 11:56:40'!
UniFileStream removeSelector: #next:into:startingAt:!
