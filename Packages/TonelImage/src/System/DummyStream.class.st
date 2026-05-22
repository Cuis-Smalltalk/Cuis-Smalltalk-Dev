"
The purpose of this class is to absorb all steam messages and do nothing.  This is so ReferenceStream can pretend to write on it while traversing all objects it would normally write.  We need to know what those object are.  8/17/96 tk

"
Class {
	#name : 'DummyStream',
	#superclass : 'Stream',
	#category : 'System-Object Storage',
	#package : 'System',
	#tag : 'Object Storage'
}

{ #category : 'as yet unclassified' }
DummyStream class >> on: aFile [
	"Return a new DummyStream instance, ignoring the argument."

	^ self basicNew

]

{ #category : 'accessing' }
DummyStream >> contents [
	^nil
]

{ #category : 'character writing' }
DummyStream >> newLine [
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."
]

{ #category : 'accessing' }
DummyStream >> nextPut: aByte [
	"do nothing"
]

{ #category : 'accessing' }
DummyStream >> nextPutAll: aByteArray [
	"do nothing"
]

{ #category : 'accessing' }
DummyStream >> originalContents [

	^ ''
]

{ #category : 'as yet unclassified' }
DummyStream >> padToEndIfCantTruncate [
	"Only makes sense for file streams with existing content."
]

{ #category : 'positioning' }
DummyStream >> position [
	"Return any random number.  Here is where the real lying begins.  We are a DummyStream afterall.  8/17/96 tk"

	^ 47 
]

{ #category : 'positioning' }
DummyStream >> position: anOffset [
	"Pretend to position wherever the caller says!"

]

{ #category : 'positioning' }
DummyStream >> skip [
	"Do nothing."
]

{ #category : 'positioning' }
DummyStream >> skip: aNumber [
	"Do nothing."
]

{ #category : 'positioning' }
DummyStream >> skipBack [
	"Do nothing."
]

{ #category : 'character writing' }
DummyStream >> space [

]

{ #category : 'error handling' }
DummyStream >> subclassResponsibility [
	"Do nothing.  Most messages to class Stream are defined as subclassResponsibility.  Just accept them.  8/17/96 tk"

	"No error.  Just go on."
]

{ #category : 'character writing' }
DummyStream >> tab [

]

{ #category : 'file modes' }
DummyStream >> useBytes [
	"A DummyStream can't switch between bytes and characters, but it doesn't matter.
	After all, all contents is discarded"
]

{ #category : 'file modes' }
DummyStream >> useCharacters [

]
