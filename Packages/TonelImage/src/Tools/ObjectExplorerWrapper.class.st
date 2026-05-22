"
Contributed by Bob Arning as part of the ObjectExplorer package.

"
Class {
	#name : 'ObjectExplorerWrapper',
	#superclass : 'ListItemWrapper',
	#instVars : [
		'itemName',
		'parent'
	],
	#category : 'Tools-Explorer',
	#package : 'Tools',
	#tag : 'Explorer'
}

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper class >> with: anObject name: aString model: aModel [

	^self new 
		setItem: anObject name: aString model: aModel
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper class >> with: anObject name: aString model: aModel parent: aParent [

	^self new 
		setItem: anObject name: aString model: aModel parent: aParent

]

{ #category : 'converting' }
ObjectExplorerWrapper >> asString [
	| explorerString |
	explorerString := [ item shortPrintString ]
		on: UnhandledError 
		do: [:ex | ex return: '<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	^(itemName , ': ' , explorerString) withBlanksCondensed
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> canBeDragged [

	^false
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> contents [
	"For objects with specific ways to show contents in explorers."
	(item customizeExplorerContents) ifTrue: [
		^ item explorerContents].

	"For all others, show named vars first, then indexed vars"
	^(item class allInstVarNames asOrderedCollection withIndexCollect: [:each :index |
		self class
			with: (item instVarAt: index)
			name: each
			model: item
			parent: self]) ,
	((1 to: item basicSize) collect: [:index |
		self class
			with: (item basicAt: index)
			name: index printString
			model: item
			parent: self])
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> hasContents [

	^item hasContentsInExplorer
	

]

{ #category : 'converting' }
ObjectExplorerWrapper >> itemName [
	^itemName
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> parent [
	^parent
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> parent: anObject [
	parent := anObject
]

{ #category : 'monitoring' }
ObjectExplorerWrapper >> refresh [
	"Refresh item given an object and a string that is either an index or an instance variable name."
	| index |
	(model class allInstVarNames includes: itemName)
		ifTrue: [
			item := model instVarNamed: itemName ]
		ifFalse: [
			item := nil.
			index := itemName findPositiveInteger.
			(index notNil and: [index between: 1 and: model basicSize]) ifTrue: [
				item := model basicAt: index]]
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> selector [
	parent ifNil: [ ^nil ].
	^(parent withoutListWrapper class allInstVarNames includes: itemName) ifTrue: [ itemName asSymbol ]
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> setItem: anObject name: aString model: aModel [

	item := anObject.
	model := aModel.
	itemName := aString.
]

{ #category : 'as yet unclassified' }
ObjectExplorerWrapper >> setItem: anObject name: aString model: aModel parent: itemParent [
	parent := itemParent.
	self setItem: anObject name: aString model: aModel
]
