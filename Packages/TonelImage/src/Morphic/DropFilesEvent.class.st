Class {
	#name : 'DropFilesEvent',
	#superclass : 'MorphicEvent',
	#instVars : [
		'position',
		'wasHandled',
		'numberOfFiles'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'instance creation' }
DropFilesEvent class >> at: aPosition with: aNumberOfFiles from: aHand [

	^self new initializeAt: aPosition with: aNumberOfFiles from: aHand


]

{ #category : 'dispatching' }
DropFilesEvent >> dispatchIn: aWorldMorph [
	"Drop is done on the innermost target that accepts it."

	| handlerMorph |
	handlerMorph := aWorldMorph topUnlockedMorphAt: position.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			
			handlerMorph allowsFilesDrop ifTrue: [
				self sendEventTo: handlerMorph ].
			
		].
		handlerMorph := handlerMorph owner.
	].
]

{ #category : 'position' }
DropFilesEvent >> eventPosition [

	^position
]

{ #category : 'files' }
DropFilesEvent >> fileNamesDo: aBlock [

	1 to: numberOfFiles do: [ :fileNumber | | string |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :utf8BytesInAStringInstance |
			"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
			contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
			string := utf8BytesInAStringInstance.
			(String isAscii: string) ifFalse: [
				ByteArray adoptInstance: string.
				string := UnicodeString fromUtf8Bytes: string ].
			aBlock value: string ]].
]

{ #category : 'initialization' }
DropFilesEvent >> initializeAt: aPosition with: aNumberOfFiles from: aHand [

	position := aPosition.
	numberOfFiles := aNumberOfFiles.
	source := aHand.
	wasHandled := false.
]

{ #category : 'testing' }
DropFilesEvent >> isDropEvent [

	^true
]

{ #category : 'files' }
DropFilesEvent >> numberOfFiles [

	^numberOfFiles
]

{ #category : 'primitives' }
DropFilesEvent >> primDropRequestFileHandle: dropIndex [
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileHandle' module:'DropPlugin'>
	^nil
]

{ #category : 'primitives' }
DropFilesEvent >> primDropRequestFileName: dropIndex [
	"Primitive. Return the file name for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileName' module:'DropPlugin'>
	^nil
]

{ #category : 'dispatching' }
DropFilesEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processDropFiles: self
]

{ #category : 'testing' }
DropFilesEvent >> wasHandled [

	^wasHandled
]

{ #category : 'testing' }
DropFilesEvent >> wasHandled: aBool [

	wasHandled := aBool
]
