Class {
	#name : 'TonelMethodDefinition',
	#superclass : 'Object',
	#instVars : [
		'className',
		'selector',
		'protocol',
		'classSide',
		'source'
	],
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'accessing' }
TonelMethodDefinition >> className [
	^ className
]

{ #category : 'accessing' }
TonelMethodDefinition >> className: aSymbol [
	className := aSymbol
]

{ #category : 'accessing' }
TonelMethodDefinition >> classSide [
	^ classSide ifNil: [ false ]
]

{ #category : 'accessing' }
TonelMethodDefinition >> classSide: aBoolean [
	classSide := aBoolean
]

{ #category : 'accessing' }
TonelMethodDefinition >> protocol [
	^ protocol
]

{ #category : 'accessing' }
TonelMethodDefinition >> protocol: aSymbol [
	protocol := aSymbol
]

{ #category : 'accessing' }
TonelMethodDefinition >> selector [
	^ selector
]

{ #category : 'accessing' }
TonelMethodDefinition >> selector: aSymbol [
	selector := aSymbol
]

{ #category : 'accessing' }
TonelMethodDefinition >> source [
	^ source
]

{ #category : 'accessing' }
TonelMethodDefinition >> source: aString [
	source := aString
]
