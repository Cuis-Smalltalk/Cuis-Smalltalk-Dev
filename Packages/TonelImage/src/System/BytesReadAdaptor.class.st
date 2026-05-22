Class {
	#name : 'BytesReadAdaptor',
	#superclass : 'UniStreamReadAdaptor',
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'accessing' }
BytesReadAdaptor >> collectionSpecies [
	^ByteArray
]

{ #category : 'reading' }
BytesReadAdaptor >> next [
	^stream nextByte
]

{ #category : 'reading' }
BytesReadAdaptor >> next: n [
	"Read n bytes, or until the end of the file.
	Answer them as a ByteArray."

	^stream nextBytes: n
]

{ #category : 'reading' }
BytesReadAdaptor >> streamUpToEnd [
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Whatever bytes in the stream.
	Leave stream at the end position."

	^stream bytesUpToEnd
]
