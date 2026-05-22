Class {
	#name : 'FileDirectoryWrapper',
	#superclass : 'ListItemWrapper',
	#instVars : [
		'itemName',
		'balloonText',
		'hasContents'
	],
	#category : 'Tools-Explorer',
	#package : 'Tools',
	#tag : 'Explorer'
}

{ #category : 'as yet unclassified' }
FileDirectoryWrapper class >> with: anObject name: aString model: aModel [

	^self new 
		setItem: anObject name: aString model: aModel
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> asString [
	| result |
	result := itemName.
	"
	('_*_' match: result)
		ifTrue: [ result := (result copyFrom: 2 to: result size - 1) ].
	"
	^ result 
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> balloonText [

	^balloonText
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> balloonText: aStringOrNil [

	balloonText := aStringOrNil
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> contents [

	| names |
	names := item directoryNames asArray.
	model showHidden ifFalse:[
		names := names reject:[:each | each beginsWith: '.'] ].
	^(names sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: model ]
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> hasContents [
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local)  (jmv: REMOVED)
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents (jmvL: REMOVED)
	"
	hasContents
		ifNil: [
			hasContents := true. "default"
			item isRoot
				ifFalse: [hasContents := self contents notEmpty ]].
	^ hasContents
]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> setItem: anObject name: aString model: aModel [

	item := anObject.
	model := aModel.
	itemName := aString.
	hasContents := nil.

]

{ #category : 'as yet unclassified' }
FileDirectoryWrapper >> settingSelector [

	^#setSelectedDirectoryTo:
]
