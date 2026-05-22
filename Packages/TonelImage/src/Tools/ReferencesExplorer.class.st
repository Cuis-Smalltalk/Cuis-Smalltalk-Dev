"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferencesExplorer',
	#superclass : 'ObjectExplorer',
	#instVars : [
		'finder'
	],
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'instance creation' }
ReferencesExplorer class >> finder: aReferenceFinder [

	^self new
		finder: aReferenceFinder;
		rootObject: aReferenceFinder target;
		yourself
]

{ #category : 'accessing' }
ReferencesExplorer >> finder [

	finder isNil ifTrue: [self finder: ReferenceFinder new].
	^finder
]

{ #category : 'private - accessing' }
ReferencesExplorer >> finder: aReferenceFinder [

	finder := aReferenceFinder
]

{ #category : 'references' }
ReferencesExplorer >> getList [

	| wrapper |
	wrapper := ReferencesExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper
]

{ #category : 'references' }
ReferencesExplorer >> rescan [

	self rescanSkipping: Array new
]

{ #category : 'references' }
ReferencesExplorer >> rescanSkipping: aCollection [

	self finder scanClosureSkipping: aCollection.
	self changed: #getList
]

{ #category : 'private - accessing' }
ReferencesExplorer >> rootObject: anObject [
	"Assume a preloaded finder if the target matches"

	super rootObject: anObject.
	self finder target == anObject ifFalse:
		[
			self finder target: anObject.
			self rescan
		]
]
