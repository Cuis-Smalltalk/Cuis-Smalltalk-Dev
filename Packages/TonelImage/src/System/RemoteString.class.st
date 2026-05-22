"
My instances provide an external file reference to a piece of text.  It may be the sourceCode of a method, or the class comments of a class.

The changes file or file-in file usually has a chunk that is just the source string of a method:

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!

"
Class {
	#name : 'RemoteString',
	#superclass : 'Object',
	#instVars : [
		'sourceFileNumber',
		'filePositionHi'
	],
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'instance creation' }
RemoteString class >> newFileNumber: sourceIndex position: anInteger  [
	"Answer a new instance of RemoteString for file indexed by sourceIndex,
	at the position anInteger.  Assumes that the string is already stored
	on the file and the instance will be used to access it."

	^self new fileNumber: sourceIndex position: anInteger
]

{ #category : 'instance creation' }
RemoteString class >> newString: aString onFileNumber: sourceIndex  [
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file and creates the remote reference."

	^self new string: aString onFileNumber: sourceIndex
]

{ #category : 'instance creation' }
RemoteString class >> newString: aString onFileNumber: sourceIndex toFile: aFileStream [
	"Answer a new instance of RemoteString for string, aString, on file indexed by
	sourceIndex.  Puts the string on the file, aFileStream, and creates the remote
	reference.  Assumes that the index corresponds properly to aFileStream."

	^self new string: aString onFileNumber: sourceIndex toFile: aFileStream
]

{ #category : 'accessing' }
RemoteString >> compilerClass [
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler
]

{ #category : 'private' }
RemoteString >> fileNumber: fileNumber position: position  [

	sourceFileNumber := fileNumber.
	filePositionHi := position
]

{ #category : 'accessing' }
RemoteString >> fileStream  [
	"Answer the file stream with position set at the beginning of my string"

	| theFile |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^ nil].
	theFile := SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	^ theFile
]

{ #category : 'testing' }
RemoteString >> isRemote [
	^true
]

{ #category : 'accessing' }
RemoteString >> last [
	^self string ifNotNil: [ :s | s last ]
]

{ #category : 'accessing' }
RemoteString >> position  [
	"Answer the location of the string on a file."

	^ filePositionHi
]

{ #category : 'accessing' }
RemoteString >> setSourcePointer: aSourcePointer [
	sourceFileNumber := SourceFiles fileIndexFromSourcePointer: aSourcePointer.
	filePositionHi := SourceFiles filePositionFromSourcePointer: aSourcePointer
]

{ #category : 'accessing' }
RemoteString >> sourceFileNumber [
	"Answer the index of the file on which the string is stored."
	^sourceFileNumber
]

{ #category : 'accessing' }
RemoteString >> sourcePointer [
	sourceFileNumber ifNil: [^ 0].
	^SourceFiles sourcePointerFromFileIndex: sourceFileNumber andPosition: filePositionHi
]

{ #category : 'accessing' }
RemoteString >> string  [
	"Answer the receiver's string if remote files are enabled."
	| theFile answer |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^nil].
	theFile := SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	answer := theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]
]

{ #category : 'private' }
RemoteString >> string: aString onFileNumber: fileNumber [
	"Store this as my string if source files exist."
	| theFile |
	(SourceFiles at: fileNumber) ifNotNil: [
		theFile := SourceFiles at: fileNumber.
		theFile setToEnd; newLine.
		self string: aString onFileNumber: fileNumber toFile: theFile]
]

{ #category : 'private' }
RemoteString >> string: aStringOrText onFileNumber: fileNumber toFile: aFileStream [
	"Store this as the receiver's text if source files exist."

	| position |
	fileNumber = 0 ifFalse: [
		aFileStream padTo: SourceFiles pointerScaleForWriting put: $  ].
	position := aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aStringOrText asPlainString
]

{ #category : 'accessing' }
RemoteString >> text  [
	"In Cuis, all source code is plain Strings"
	^self string
]
