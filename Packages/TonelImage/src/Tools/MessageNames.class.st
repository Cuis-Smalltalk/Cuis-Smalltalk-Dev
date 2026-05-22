"
Search for message names.  There are several special characters that alter how searchString is interpreted:
$; - separate several search criteria (like 'editorClassFor:;contentsSelection')
$* - matches a string pattern rather than just a simple string match (i.e. 'set*text')
$# - matches a single character (for example, 'ini###lize'
"
Class {
	#name : 'MessageNames',
	#superclass : 'MethodSet',
	#instVars : [
		'searchString',
		'selectorList',
		'selectedSelector'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'ui' }
MessageNames class >> open [
"Bring a MessageNames window to the front"
	^ MessageNamesWindow 
		open: MessageNames new
		label: 'Message Names'
]

{ #category : 'user interface support' }
MessageNames >> autoCompleterClassFor: textGetter [
	^ (textGetter = #acceptedContents) ifTrue:
		[
			super autoCompleterClassFor: textGetter ].

]

{ #category : 'search' }
MessageNames >> computeSelectorListFromSearchString [
	"Compute selector list from search string"
	| sorted |
	sorted := SortedCollection sortBlock: [ :x :y |
		x asLowercase <= y asLowercase ].
	(searchString findBetweenSubStrs: ';') do: [ :selPat | | raw n m |
		selPat notEmpty ifTrue: [
			(selPat first == $" and: [
				selPat last == $" and: [ selPat size > 2 ]])
				ifTrue: [
					Symbol
						hasInterned:
							(selPat
								copyFrom: 2
								to: selPat size - 1)
						ifTrue: [ :sym |
							sorted add: sym ]]
				ifFalse: [
					raw := ((n := selPat occurrencesOf: $*) > 0 | ((m := selPat occurrencesOf: $#) > 0) and: [ selPat size > (n + m) ])
						ifTrue: [ Symbol selectorsMatching: selPat ]
						ifFalse: [ Symbol selectorsContaining: selPat ].
					sorted addAll: raw ]]].
	selectorList := sorted asArray  select: [ :sym |
		(Smalltalk isThereAnImplementorOf: sym) or: [ Smalltalk isThereAReferenceTo: sym ]].
	^ selectorList.
]

{ #category : 'initialization' }
MessageNames >> contentsSelection [
	"Return the interval of text in the search pane to select when I set the pane's contents"

	^ #all 		"all of it"
]

{ #category : 'user interface support' }
MessageNames >> editorClassFor: textGetter [
	textGetter = #searchString ifTrue: [
		^SmalltalkEditor ].
	^super editorClassFor: textGetter
]

{ #category : 'initialization' }
MessageNames >> initialize [
	super initialize.
	searchString := ''
]

{ #category : 'initialization' }
MessageNames >> labelString [
	^ 'Message names containing "', searchString asLowercase, '"'
]

{ #category : 'selector list' }
MessageNames >> messageList [
	"Answer the receiver's message list, computing it if necessary. The way 
	to force a recomputation is to set the messageList to nil"
	messageList
		ifNil: [
			messageList := selectedSelector
				ifNil: [#()]
				ifNotNil: [
					Smalltalk allImplementorsOf: selectedSelector].
			self initializeMessageList: messageList.
			self messageListIndex: (messageList notEmpty
				ifTrue: [1]
				ifFalse: [0])
			].
	^ messageList
]

{ #category : 'search' }
MessageNames >> searchString [
	"Answer the current searchString"

	^ searchString
]

{ #category : 'search' }
MessageNames >> searchString: aString [
	"Take what the user typed and find all selectors containing it"

	searchString := aString asPlainString.
	self changed: #relabel.
	selectorList := nil.
	self changed: #selectorList.
	self changed: #messageList.
	^ true
]

{ #category : 'class list' }
MessageNames >> selectedClassOrMetaClass [
	"Answer the currently selected class (or metaclass).
	
	If no method is selected, try to interpret the selected message name
	as a class"
	
	^ super selectedClassOrMetaClass ifNil: [
		self selectedMessageName ifNotNil: [ :className | | mayBeClass |
			(mayBeClass := Smalltalk
				at: className ifAbsent: [^ nil ]) isBehavior
					ifTrue: [ mayBeClass ]
					ifFalse: [ mayBeClass class ] ] ].
]

{ #category : 'selector list' }
MessageNames >> selectedMessageName [
	"Answer the name of the currently selected message."
	^selectedSelector
]

{ #category : 'selector list' }
MessageNames >> selectorList [
	"Answer the selectorList"

	selectorList ifNil: [
		self computeSelectorListFromSearchString.
		selectedSelector := selectorList isEmpty ifFalse: [ selectorList first ].
		messageList := nil ].
	^ selectorList
]

{ #category : 'selector list' }
MessageNames >> selectorListIndex [
	"Answer the selectorListIndex"

	selectedSelector ifNil: [ ^ 0 ].
	^self selectorList indexOf: selectedSelector
]

{ #category : 'selector list' }
MessageNames >> selectorListIndex: anInteger  [
	"Set the selectorListIndex as specified, and propagate consequences"

	selectedSelector := anInteger = 0 ifFalse: [ self selectorList at: anInteger ].
	anInteger = 0
		ifTrue: [^ self].
	messageList := nil.
	self changed: #selectorListIndex.
	self changed: #messageList
]
