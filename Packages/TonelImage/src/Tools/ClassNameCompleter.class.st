Class {
	#name : 'ClassNameCompleter',
	#superclass : 'AutoCompleter',
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'testing' }
ClassNameCompleter >> canShowSelectorDocumentation [

	^false
]

{ #category : 'entries' }
ClassNameCompleter >> computeEntries [

	prefix := model actualContents string.
	entries := (Smalltalk classNames select: [ :aClassName | aClassName beginsWith: prefix ]) sort.
	
]

{ #category : 'menu morph' }
ClassNameCompleter >> morphClass [
	^SmalltalkCompleterMorph
]

{ #category : 'testing' }
ClassNameCompleter >> opensWithTab [

	^true
]
