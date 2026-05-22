"
Contributed by Bob Arning as part of the ObjectExplorer package.

"
Class {
	#name : 'AbstractHierarchicalList',
	#superclass : 'ActiveModel',
	#instVars : [
		'currentSelection'
	],
	#category : 'Tools-Explorer',
	#package : 'Tools',
	#tag : 'Explorer'
}

{ #category : 'as yet unclassified' }
AbstractHierarchicalList >> getCurrentSelection [

	^currentSelection
]

{ #category : 'as yet unclassified' }
AbstractHierarchicalList >> noteNewSelection: x [

	currentSelection := x.
	self changed: #getCurrentSelection.
	currentSelection ifNil: [^self].
	currentSelection sendSettingMessageTo: self.

]
