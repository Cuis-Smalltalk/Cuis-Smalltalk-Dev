"
Specific for Smalltalk code.
"
Class {
	#name : 'SmalltalkCompleterMorph',
	#superclass : 'AutoCompleterMorph',
	#instVars : [
		'selectorDocumentation'
	],
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'new-morph participation' }
SmalltalkCompleterMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> crPressedOnSelectorDocumentation [

	self hideSelectorDocumentation.
	self activeHand newKeyboardFocus: completer textMorph 
]

{ #category : 'submorphs-add/remove' }
SmalltalkCompleterMorph >> delete [

	selectorDocumentation ifNotNil: [
		selectorDocumentation delete.
		selectorDocumentation := nil ].

	super delete 
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> hideSelectorDocumentation [

	selectorDocumentation ifNotNil: [ selectorDocumentation hide ].
	self stillActive 
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> initializeSelectorDocumentation [

	selectorDocumentation := TextModelMorph withText: ''.
	selectorDocumentation innerTextMorph
		acceptOnCR: false;
		crAction: [ self crPressedOnSelectorDocumentation ].

	selectorDocumentation
		wrapFlag: true;
		borderColor: Color black;
		borderWidth: 1;
		disableEditing;
		openInWorld
	
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> isShowingSelectorDocumentation [

	^selectorDocumentation notNil and: [ selectorDocumentation visible ]
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> methodDocumentationSeparator [

	^ Text
		string: String newLineString, '------------------------------------------------------------------------------------------------', String newLineString
		attribute: TextColor black.
]

{ #category : 'actions' }
SmalltalkCompleterMorph >> resetMenu [

	self hideSelectorDocumentation.
	super resetMenu
]

{ #category : 'accessing' }
SmalltalkCompleterMorph >> selected: aNumber [
	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [
			selected := aNumber.
			self isShowingSelectorDocumentation ifTrue: [ self showSelectorDocumentation ]]
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> selectorDocumentation [

	selectorDocumentation ifNil: [ self initializeSelectorDocumentation ].
	^selectorDocumentation 
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> selectorDocumentationText [

	| selectedEntry selectorsClasses |

	selectedEntry := completer selectedEntry.
	(completer isCategoryEntry: selectedEntry) ifTrue: [ ^'' ].
	selectedEntry := selectedEntry asSymbol.
	selectorsClasses := completer selectorsClasses.

	^ selectorsClasses isEmpty
		ifTrue: [ self selectorDocumentationTextForAllI: (Smalltalk allImplementorsOf: selectedEntry) ]
		ifFalse: [ self selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses ].
	
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> selectorDocumentationTextForAllI: aMethodsCollection [

	| selectorDocumentationText implementors methodDocumentationSeparator |

	selectorDocumentationText := Text new.
	methodDocumentationSeparator := self methodDocumentationSeparator.
	implementors := aMethodsCollection asSortedCollection: [ :leftMethod :rightMethod | leftMethod methodClass classDepth <  rightMethod methodClass classDepth ].

	implementors
		do: [ :implementor | selectorDocumentationText := selectorDocumentationText append: (completer documentationOf: implementor)]
		separatedBy: [ selectorDocumentationText := selectorDocumentationText append: methodDocumentationSeparator ].

	^ selectorDocumentationText
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses [

	| methodsToShow |

	methodsToShow := selectorsClasses
		inject: IdentitySet new
		into: [ :methods :aClass |
			(aClass lookupSelector: selectedEntry) ifNotNil: [ :method | methods add: method ].
			methods ].

	^self selectorDocumentationTextForAllI: methodsToShow 
]

{ #category : 'selector documentation' }
SmalltalkCompleterMorph >> showSelectorDocumentation [

	| docExtent relativeSelected dy ourBounds docPos |
	docExtent := `600@250`.
	relativeSelected := (self selected - self firstVisible) min: self itemsPerPage - 1 max: 0.
	dy := relativeSelected * self itemHeight + 1.
	ourBounds := self displayBounds.
	docPos := ourBounds right + docExtent x < self world morphWidth
		ifTrue: [ ourBounds topRight + (0@dy) ]
		ifFalse: [ ourBounds topLeft + (docExtent x negated @ dy) ].

	self setDefaultColors.

	self selectorDocumentation
		model: (TextModel withText: self selectorDocumentationText);
		position: docPos extent: docExtent;
		wrapFlag: false;
		show;
		fitInOwner.
]

{ #category : 'stepping' }
SmalltalkCompleterMorph >> step [

	self isShowingSelectorDocumentation ifTrue: [ ^self ].
	super step.
]
