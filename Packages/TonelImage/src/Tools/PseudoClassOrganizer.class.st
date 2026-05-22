Class {
	#name : 'PseudoClassOrganizer',
	#superclass : 'BasicClassOrganizer',
	#category : 'Tools-Code File Browser',
	#package : 'Tools',
	#tag : 'Code File Browser'
}

{ #category : 'comment accessing' }
PseudoClassOrganizer >> classComment [
	"Answer the comment associated with the object that refers to the receiver."
	^classComment ifNil: [ '' ]
]

{ #category : 'comment accessing' }
PseudoClassOrganizer >> classComment: aChangeRecord [
	classComment := aChangeRecord
]

{ #category : 'accessing' }
PseudoClassOrganizer >> initEmpty [
	super initEmpty.
	self classComment: nil
]

{ #category : 'testing' }
PseudoClassOrganizer >> isRemoved: aSelector [
	^(self categoryOfElement: aSelector) = PseudoClass removedCategoryName
]
