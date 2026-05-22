"
Contributed by Bob Arning as part of the ObjectExplorer package.

"
Class {
	#name : 'ListItemWrapper',
	#superclass : 'Object',
	#instVars : [
		'item',
		'model'
	],
	#category : 'Tools-Explorer',
	#package : 'Tools',
	#tag : 'Explorer'
}

{ #category : 'as yet unclassified' }
ListItemWrapper class >> with: anObject [

	^self new setItem: anObject
]

{ #category : 'as yet unclassified' }
ListItemWrapper class >> with: anObject model: aModel [

	^self new setItem: anObject model: aModel
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> asString [

	^item asString
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> balloonText [

	^nil
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> canBeDragged [

	^true
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> contents [

	^Array new
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> hasContents [

	^self contents notEmpty
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> hasEquivalentIn: aCollection [

	aCollection detect: [ :each | 
		each withoutListWrapper = item withoutListWrapper
	] ifNone: [^false].
	^true
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> isAncestorOf: anObject [
	^ false
]

{ #category : 'accessing' }
ListItemWrapper >> item [
	^item
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> preferredColor [

	^nil
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> printOn: aStream [
	aStream nextPutAll: self asString
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> sendSettingMessageTo: aModel [

	aModel 
		perform: (self settingSelector ifNil: [^self])
		with: self withoutListWrapper

]

{ #category : 'as yet unclassified' }
ListItemWrapper >> setItem: anObject [

	item := anObject
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> setItem: anObject model: aModel [

	item := anObject.
	model := aModel.
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> settingSelector [

	^nil
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> wantsDroppedObject: anotherItem [

	^false
]

{ #category : 'as yet unclassified' }
ListItemWrapper >> withoutListWrapper [

	^item withoutListWrapper
]
