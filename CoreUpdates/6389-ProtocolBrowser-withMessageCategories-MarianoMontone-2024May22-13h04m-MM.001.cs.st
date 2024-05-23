'From Cuis7.1 [latest update: #6391] on 22 May 2024 at 1:08:08 pm'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MethodSet subclass: #ProtocolBrowser
	instanceVariableNames: 'baseClass selectiveClassList selectiveClassListIndex selectedName exclude categoriesList categoriesListIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!ProtocolBrowserWindow commentStamp: 'MM 5/22/2024 13:05:42' prior: 0!
A view of the messages available to a class from itself upward through the class hierarchy. The viewed protocol can be pruned by selecting a superclass in the class hierarchy pane.

ProtocolBrowserWindow openFullProtocolForClass: Number.!


!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 21:18:44'!
categoriesList
	
	^ categoriesList! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 22:06:52'!
categoriesListIndex
	"Answer the value of categoriesListIndex"

	^ categoriesListIndex ifNil: [0] ifNotNil: [categoriesListIndex]! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/21/2024 10:38:38'!
categoriesListIndex: anIndex
	"Set the value of categoriesListIndex"

	categoriesListIndex := anIndex isZero ifTrue:  [nil] ifFalse: [anIndex].
	self changed: #categoriesListIndex. "update my selection"
	self changed: #messageList! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 22:04:08'!
messageList

	^ self selectedCategory ifNil: [ super messageList]
		ifNotNil: [:category |
			messageList select: [:methodRef | methodRef category == category]]! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 21:59:06'!
selectedCategory

	^ categoriesListIndex ifNotNil: [categoriesList at: categoriesListIndex]! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 21:55:31'!
updateCategoriesList

	"Update the list of categories"
	
	| categories |
	
	categories := Set new.
	messageList do: [:methodRef | categories add: methodRef category].
	
	categoriesList := categories asSortedCollection.
	
	self changed: #categoriesList! !

!ProtocolBrowser methodsFor: 'as yet unclassified' stamp: 'MM 5/21/2024 10:33:44'!
selectedMessageName: aSelector
	"Make the given selector be the selected message name"

	| anIndex |
	anIndex := self messageList indexOf: aSelector.
	anIndex > 0 ifTrue:
		[self messageListIndex: anIndex]! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'MM 5/21/2024 09:44:57'!
buildCategoriesList
	"Define the message categories list pane"

	^ PluggableListMorph
		withModel: model
		listGetter: #categoriesList
		indexGetter: #categoriesListIndex
		indexSetter: #categoriesListIndex:! !

!ProtocolBrowserWindow methodsFor: 'as yet unclassified' stamp: 'MM 5/21/2024 10:34:49'!
browseCategoryOfCurrentMethod

	model categoryOfCurrentMethod ifNotNil: [:category| |catIndex selectedMessage|
		selectedMessage := model selectedMessageName.
		catIndex := model categoriesList indexOf: category.
		model categoriesListIndex: catIndex.
		model selectedMessageName: selectedMessage]! !


!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 21:45:15'!
on: aClass
	"Initialize the protocol for the class, aClass."
	"Optionally, the upper part of the protocol is excluded."
	| selectors |
	
	selectors := Set new.
	aClass withAllSuperclasses do: [ :each |
		(exclude includes: each name) ifFalse: [selectors addAll: each selectors]].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass.
	self updateCategoriesList.! !

!ProtocolBrowser methodsFor: 'private' stamp: 'MM 5/20/2024 22:24:41'!
selectiveClassListIndex: anObject
	"Set the value of selectiveClassListIndex"

	selectiveClassListIndex := anObject.
	categoriesListIndex := nil.
	self protocolFor: selectiveClassListIndex
! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'MM 5/20/2024 21:22:51'!
buildMorphicWindow
	"Answer a morphic window that can display the receiver with a class hierarchy"
	| topRow |

	topRow := LayoutMorph newRow.
	topRow 
		addMorph: self buildSelectiveClassList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildCategoriesList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildMorphicMessageList proportionalWidth: 0.5.
	self layoutMorph
		addMorph: topRow proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection! !

!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MethodSet subclass: #ProtocolBrowser
	instanceVariableNames: 'baseClass selectiveClassList selectiveClassListIndex selectedName exclude categoriesList categoriesListIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
