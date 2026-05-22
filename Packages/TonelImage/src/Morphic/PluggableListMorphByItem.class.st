Class {
	#name : 'PluggableListMorphByItem',
	#superclass : 'PluggableListMorph',
	#instVars : [
		'itemList'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'model access' }
PluggableListMorphByItem >> getCurrentSelectionIndex [
	"Answer the index of the current selection."
	| item |
	getIndexSelector ifNil: [^ 0].
	item := model perform: getIndexSelector.
	^ list findFirst: [ :x | x = item]

]

{ #category : 'model access' }
PluggableListMorphByItem >> getList [
	"cache the raw items in itemList"
	itemList := getListSelector ifNil: [ #() ] ifNotNil: [ model perform: getListSelector ].
	^super getList
]

{ #category : 'model access' }
PluggableListMorphByItem >> setSelectionIndex: anInteger [
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item := itemList at: anInteger ifAbsent: nil.
		model perform: setIndexSelector with: item.
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		^ true ].
	^false
	
]
