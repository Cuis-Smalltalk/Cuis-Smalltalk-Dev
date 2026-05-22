"
A Change Sorter limited to operate on a single ChangeSet.
"
Class {
	#name : 'SingleSetChangeSorter',
	#superclass : 'ChangeSorter',
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'accessing' }
SingleSetChangeSorter >> changeSetList [
	^{ (myChangeSet hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), myChangeSet name }
]

{ #category : 'accessing' }
SingleSetChangeSorter >> currentChangeSetName [
	^ (myChangeSet hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), myChangeSet name
]
