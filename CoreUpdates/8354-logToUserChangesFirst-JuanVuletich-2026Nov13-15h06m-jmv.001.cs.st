'From Cuis7.9 [latest update: #8351] on 13 November 2026 at 3:10:17 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 13/Nov/2026, 5:35:16 pm (UTC)'!
classComment: aString stamp: aStamp
	"Store the comment, associated with the class we are organizing."

	| header |

	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm ].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].
	
	self organization classComment: aString stamp: aStamp.

	SystemChangeNotifier uniqueInstance classCommented: self.! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 13/Nov/2026, 5:41:41 pm (UTC)'!
fileOutCommentOn: aFileStream padForPointerScale: doPad
	"Copy the class comment to aFileStream."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		doPad ifTrue: [
			aFileStream padSpacesTo: SourceFiles pointerScaleForWriting ].
		aFileStream nextChunkPut: self classComment asPlainString ].! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 13/Nov/2026, 6:08:20 pm (UTC)'!
writeToChangesFile: sourceStr withPreamble: preambleBlock
	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position  |

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self historyPointer: (CodeHistory historyPointerFromFileIndex: fileIndex andPosition: position).
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 13/Nov/2026, 5:44:48 pm (UTC)'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "empty doits confuse replay"
	
	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		preambleOrNil ifNotNil: [
			stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
		stream nextChunkPut: aString.
		stream flush ].
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].! !

