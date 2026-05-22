Class {
	#name : 'UniStreamReadAdaptor',
	#superclass : 'Object',
	#instVars : [
		'stream'
	],
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'accessing' }
UniStreamReadAdaptor >> collectionSpecies [
	^self subclassResponsibility
]

{ #category : 'reading' }
UniStreamReadAdaptor >> next [
	self subclassResponsibility
]

{ #category : 'reading' }
UniStreamReadAdaptor >> next: n [
	"Read n elements, or until the end of the file."

	self subclassResponsibility
]

{ #category : 'accessing' }
UniStreamReadAdaptor >> stream: aStream [
	stream := aStream
]

{ #category : 'reading' }
UniStreamReadAdaptor >> streamUpToEnd [
	"Answer the contents of our stream, from current postion to end, seen from our perspective.
	Leave stream at the end position."

	^self subclassResponsibility
]
