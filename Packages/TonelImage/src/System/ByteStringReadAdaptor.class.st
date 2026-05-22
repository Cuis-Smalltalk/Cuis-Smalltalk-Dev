Class {
	#name : 'ByteStringReadAdaptor',
	#superclass : 'UniStreamReadAdaptor',
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'accessing' }
ByteStringReadAdaptor >> collectionSpecies [
	^String
]

{ #category : 'reading' }
ByteStringReadAdaptor >> next [
	"Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters."

	stream nextByte ifNotNil: [ :byte |
		^Character codePoint: byte ].
	^nil
]

{ #category : 'reading' }
ByteStringReadAdaptor >> next: n [
	"Read n bytes, or until the end of the file.
	Answer them as seen from our perspective:
	Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters.
	Answer is a String (i.e. a byte oriented ISO 8859-1 String)"

	^(stream nextBytes: n) asString
]

{ #category : 'reading' }
ByteStringReadAdaptor >> streamUpToEnd [
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as Latin-1 / ISO 8859-1 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Latin-1 / ISO-8859-1 byte Characters.
	Leave stream at the end position."

	^stream bytesUpToEnd asString
]
