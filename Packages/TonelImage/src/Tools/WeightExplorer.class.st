"
See comments in ClosureScanner.
"
Class {
	#name : 'WeightExplorer',
	#superclass : 'ReferencesExplorer',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'references' }
WeightExplorer >> getList [

	| wrapper |
	wrapper := WeightExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash printString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper
]
