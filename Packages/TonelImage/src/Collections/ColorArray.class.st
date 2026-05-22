Class {
	#name : 'ColorArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'accessing' }
ColorArray >> at: index [
	^(super at: index) asColorOfDepth: 32
]

{ #category : 'accessing' }
ColorArray >> at: index put: aColor [
	^super at: index put: (aColor pixelWordForDepth: 32).
]
