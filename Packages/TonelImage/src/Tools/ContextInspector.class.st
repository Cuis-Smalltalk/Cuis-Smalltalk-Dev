Class {
	#name : 'ContextInspector',
	#superclass : 'Inspector',
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'accessing' }
ContextInspector >> fieldList [
	"Answer the base field list plus an abbreviated list of indices."

	^ self baseFieldList , (object tempNames collect:[:t| '[',t,']'])
]

{ #category : 'accessing' }
ContextInspector >> selection [
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ object instVarAt: self selectionIndexNonMeta].
	basicIndex := self selectionIndexNonMeta - self objectClassInstSize.
	^object debuggerMap namedTempAt: basicIndex in: object

]
