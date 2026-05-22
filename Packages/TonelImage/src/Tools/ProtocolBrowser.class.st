"
An instance of ProtocolBrowser shows the methods a class understands--inherited or implemented at this level--as a "flattened" list.
"
Class {
	#name : 'ProtocolBrowser',
	#superclass : 'MethodSet',
	#instVars : [
		'baseClass',
		'selectiveClassList',
		'selectiveClassListIndex',
		'selectedName',
		'exclude',
		'categoriesList',
		'categoriesListIndex'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'private' }
ProtocolBrowser >> categoriesList [
	
	^ categoriesList
]

{ #category : 'private' }
ProtocolBrowser >> categoriesListIndex [
	"Answer the value of categoriesListIndex"

	^ categoriesListIndex ifNil: [0] ifNotNil: [categoriesListIndex]
]

{ #category : 'private' }
ProtocolBrowser >> categoriesListIndex: anIndex [
	"Set the value of categoriesListIndex"

	categoriesListIndex := anIndex isZero ifTrue:  [nil] ifFalse: [anIndex].
	self changed: #categoriesListIndex. "update my selection"
	self changed: #messageList
]

{ #category : 'private' }
ProtocolBrowser >> hierarchyForClass: aClass [
	"Set the class hierarchy for the list pane"
	| tab |

	selectiveClassList := OrderedCollection new.
	tab := ''.
	aClass withAllSuperclasses reverse do: [:ea | 
		selectiveClassList add: tab , ea name.
		tab := tab , '  '].

]

{ #category : 'private' }
ProtocolBrowser >> initListFrom: selectorCollection highlighting: aClass  [
	"Make up the messageList with items from aClass in boldface."
	| defClass item |

	messageList := OrderedCollection new.
	selectorCollection do: [ :selector |  
		defClass := aClass whichClassIncludesSelector: selector.
		item := selector, '     (' , defClass name , ')'.
		defClass == aClass ifTrue: [item := item asText allBold].
		messageList add: (
			MethodReference new
				setClass: defClass 
				methodSymbol: selector 
				stringVersion: item)].
	self hierarchyForClass: (baseClass := aClass)
]

{ #category : 'initialization' }
ProtocolBrowser >> initialize [
	super initialize.
	exclude := OrderedCollection new
]

{ #category : 'accessing' }
ProtocolBrowser >> labelString [
	"Answer the string for the window title"
	
	^ 'Protocol for: ', baseClass name, ' up to: ', (selectedName ifNil: [ ProtoObject name asPlainString ])
]

{ #category : 'private' }
ProtocolBrowser >> messageList [

	^ self selectedCategory ifNil: [ super messageList]
		ifNotNil: [:category |
			messageList select: [:methodRef | methodRef category == category]]
]

{ #category : 'private' }
ProtocolBrowser >> on: aClass [
	"Initialize the protocol for the class, aClass."
	"Optionally, the upper part of the protocol is excluded."
	| selectors |
	
	selectors := Set new.
	aClass withAllSuperclasses do: [ :each |
		(exclude includes: each name) ifFalse: [selectors addAll: each selectors]].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass.
	self updateCategoriesList.
]

{ #category : 'private' }
ProtocolBrowser >> protocolFor: anIndex [
	"Change the listed protocol"

	exclude := OrderedCollection new.
	anIndex > 0
		ifTrue: [
			selectedName := (selectiveClassList at: anIndex) withBlanksTrimmed.
			(1 to: anIndex - 1) do: [:ix |
				exclude addLast: (selectiveClassList at: ix) withBlanksTrimmed]]
		ifFalse: [
			selectedName := nil.
			].
	self on: baseClass.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged
]

{ #category : 'private' }
ProtocolBrowser >> selectedCategory [

	^ categoriesListIndex ifNotNil: [categoriesList at: categoriesListIndex]
]

{ #category : 'accessing' }
ProtocolBrowser >> selectedMessageName: aSelector [
	"Make the given selector be the selected message name"

	| anIndex |
	anIndex := self messageList indexOf: aSelector.
	anIndex > 0 ifTrue:
		[self messageListIndex: anIndex]
]

{ #category : 'private' }
ProtocolBrowser >> selectiveClassList [
	"Answer the value of selectiveClassList"

	^ selectiveClassList
]

{ #category : 'private' }
ProtocolBrowser >> selectiveClassListIndex [
	"Answer the value of selectiveClassListIndex"

	selectiveClassListIndex ifNil: [selectiveClassListIndex := 0].
	^ selectiveClassListIndex
]

{ #category : 'private' }
ProtocolBrowser >> selectiveClassListIndex: anObject [
	"Set the value of selectiveClassListIndex"

	| hadSelectedCategoryFor |
	selectiveClassListIndex := anObject.
	hadSelectedCategoryFor := categoriesListIndex notNil ifTrue: [selectedMessage].
	categoriesListIndex := nil.
	self protocolFor: selectiveClassListIndex.
	hadSelectedCategoryFor notNil ifTrue: [
		self categoriesListIndex: (categoriesList indexOf: selectedMessage category) ].

]

{ #category : 'message category functions' }
ProtocolBrowser >> showHomeCategory [
	"Show the home category of the selected method."

	self categoryOfCurrentMethod ifNotNil: [ :category | | catIndex selector |
		selector := self selectedMessageName.
		catIndex := self categoriesList indexOf: category.
		self categoriesListIndex: catIndex.
		self selectedMessageName: selector ].

]

{ #category : 'private' }
ProtocolBrowser >> updateCategoriesList [

	"Update the list of categories"
	
	| categories |
	
	categories := Set new.
	messageList do: [:methodRef | categories add: methodRef category].
	
	categoriesList := categories asSortedCollection.
	
	self changed: #categoriesList
]
