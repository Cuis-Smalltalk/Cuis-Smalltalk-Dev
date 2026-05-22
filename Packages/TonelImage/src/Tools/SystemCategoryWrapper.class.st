Class {
	#name : 'SystemCategoryWrapper',
	#superclass : 'ListItemWrapper',
	#instVars : [
		'itemName'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'instance creation' }
SystemCategoryWrapper class >> with: anObject name: aString model: aModel [
	^ self new  setItem: anObject name: aString model: aModel
]

{ #category : 'converting' }
SystemCategoryWrapper >> asString [
	^ itemName
]

{ #category : 'testing' }
SystemCategoryWrapper >> canBeDragged [
	^ false
]

{ #category : 'accessing' }
SystemCategoryWrapper >> contents [
	| prefix subcategories |
	(item includes: $-) ifTrue: [^ #()].
	prefix := item, '-'.
	subcategories := OrderedCollection new.
	model systemCategoryList do: [:each|
		(each beginsWith: prefix)
			ifTrue: [subcategories add: (each allButFirst: prefix size)]].
	^ subcategories collect: [:each| self class with: prefix, each name: each model: model]
]

{ #category : 'testing' }
SystemCategoryWrapper >> isAncestorOf: aString [
	^ (item includes: $-) not and: [aString beginsWith: item, '-']
]

{ #category : 'private' }
SystemCategoryWrapper >> setItem: anObject name: aString model: aModel [
	item := anObject.
	model := aModel.
	itemName := aString
]
