Class {
	#name : 'TonelClassDefinition',
	#superclass : 'Object',
	#instVars : [
		'comment',
		'className',
		'superclassName',
		'instanceVariableNames',
		'classVariableNames',
		'poolDictionaries',
		'classInstanceVariableNames',
		'category',
		'package',
		'tag',
		'methods'
	],
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'accessing' }
TonelClassDefinition >> category [
	^ category
]

{ #category : 'accessing' }
TonelClassDefinition >> category: aSymbol [
	category := aSymbol
]

{ #category : 'accessing' }
TonelClassDefinition >> classInstanceVariableNames [
	^ classInstanceVariableNames ifNil: [ #() ]
]

{ #category : 'accessing' }
TonelClassDefinition >> classInstanceVariableNames: aCollection [
	classInstanceVariableNames := aCollection asArray
]

{ #category : 'accessing' }
TonelClassDefinition >> className [
	^ className
]

{ #category : 'accessing' }
TonelClassDefinition >> className: aSymbol [
	className := aSymbol
]

{ #category : 'accessing' }
TonelClassDefinition >> classVariableNames [
	^ classVariableNames ifNil: [ #() ]
]

{ #category : 'accessing' }
TonelClassDefinition >> classVariableNames: aCollection [
	classVariableNames := aCollection asArray
]

{ #category : 'accessing' }
TonelClassDefinition >> comment [
	^ comment ifNil: [ '' ]
]

{ #category : 'accessing' }
TonelClassDefinition >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
TonelClassDefinition >> instanceVariableNames [
	^ instanceVariableNames ifNil: [ #() ]
]

{ #category : 'accessing' }
TonelClassDefinition >> instanceVariableNames: aCollection [
	instanceVariableNames := aCollection asArray
]

{ #category : 'accessing' }
TonelClassDefinition >> methods [
	^ methods ifNil: [ methods := OrderedCollection new ]
]

{ #category : 'accessing' }
TonelClassDefinition >> package [
	^ package
]

{ #category : 'accessing' }
TonelClassDefinition >> package: aSymbol [
	package := aSymbol
]

{ #category : 'accessing' }
TonelClassDefinition >> poolDictionaries [
	^ poolDictionaries ifNil: [ #() ]
]

{ #category : 'accessing' }
TonelClassDefinition >> poolDictionaries: aCollection [
	poolDictionaries := aCollection asArray
]

{ #category : 'accessing' }
TonelClassDefinition >> superclassName [
	^ superclassName
]

{ #category : 'accessing' }
TonelClassDefinition >> superclassName: aSymbol [
	superclassName := aSymbol
]

{ #category : 'accessing' }
TonelClassDefinition >> tag [
	^ tag
]

{ #category : 'accessing' }
TonelClassDefinition >> tag: aSymbol [
	tag := aSymbol
]
