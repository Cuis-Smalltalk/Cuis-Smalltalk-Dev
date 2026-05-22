"
Standard Output/Error Streams.
"
Class {
	#name : 'StdIOWriteStream',
	#superclass : 'Stream',
	#instVars : [
		'fileID',
		'name'
	],
	#classVars : [
		'StdErr',
		'StdOut'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'cached state access' }
StdIOWriteStream class >> releaseClassCachedState [

	StdOut := nil.
	StdErr := nil
]

{ #category : 'instance creation' }
StdIOWriteStream class >> stderr [
	StdErr ifNil: [
		StdErr := self basicNew.
		StdErr 
			openOnHandle: self stderrHandle 
			name: 'stderr' ].
	^StdErr
]

{ #category : 'accessing' }
StdIOWriteStream class >> stderrHandle [

	^ self stdioHandles at: 3
]

{ #category : 'accessing' }
StdIOWriteStream class >> stdioHandles [
" answer handles: #(stdin stdout stderr) "
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
]

{ #category : 'instance creation' }
StdIOWriteStream class >> stdout [
	StdOut ifNil: [
		StdOut := self basicNew.
		StdOut 
			openOnHandle: self stdoutHandle 
			name: 'stdout' ].
	^StdOut
]

{ #category : 'accessing' }
StdIOWriteStream class >> stdoutHandle [

	^ self stdioHandles at: 2
]

{ #category : 'streaming' }
StdIOWriteStream >> flush [
	"Flush pending changes"
	^self primFlush: fileID
]

{ #category : 'character writing' }
StdIOWriteStream >> newLine [
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter
]

{ #category : 'streaming' }
StdIOWriteStream >> nextPut: aCharacter [
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: (String with: aCharacter).
	^aCharacter
]

{ #category : 'streaming' }
StdIOWriteStream >> nextPutAll: aStringOrUTF8Bytes [
	"Write the given String  or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes := aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: 'Лорем ипсум долор сит амет, дебет салутанди меи еи. Ан хис граеце утамур долорум, лорем принципес молестиае дуо ин. Еи мовет ностро оптион нам, еос епицури омиттантур ут. Ут мунере лаборе интеллегебат усу, еа сед дебет луптатум, видит иллуд иудицо яуо ут.'; flush
"
]

{ #category : 'open / close' }
StdIOWriteStream >> openOnHandle: aFileID name: streamName [
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID := aFileID.
	name := streamName.
]

{ #category : 'primitives' }
StdIOWriteStream >> primFlush: id [
	"Flush pending changes to the disk"

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
]

{ #category : 'primitives' }
StdIOWriteStream >> primWrite: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self error: 'StdIOWriteStream [', name, '] write failed'.
]

{ #category : 'printing' }
StdIOWriteStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
]

{ #category : 'character writing' }
StdIOWriteStream >> space [
	"Append a space character to the receiver."

	self nextPut: Character space
]
