"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferenceFinder',
	#superclass : 'ClosureScanner',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'private - explorer window' }
ReferenceFinder >> explorerClass [

	^ReferencesExplorer
]

{ #category : 'private - explorer window' }
ReferenceFinder >> explorerWindowPrefix [

	^'References to '
]
