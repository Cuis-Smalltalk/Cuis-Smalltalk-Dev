"
Allows use of the SmalltalkCompleter to find class names
"
Class {
	#name : 'ClassNameRequestMorph',
	#superclass : 'StringRequestMorph',
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
ClassNameRequestMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'user interface support' }
ClassNameRequestMorph >> autoCompleterClassFor: textGetter [
	
	^ClassNameCompleter 
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> classOfThisContext [

	^ MethodContext
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> classOfWorkspaceVarNamed: aName [

	^ nil
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
ClassNameRequestMorph >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]
