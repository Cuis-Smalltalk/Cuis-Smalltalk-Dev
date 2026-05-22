"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferencesExplorerWindow',
	#superclass : 'ObjectExplorerWindow',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'GUI building' }
ReferencesExplorerWindow >> buildMorphicWindow [

	super buildMorphicWindow.
	listMorph autoExpand: true
]
