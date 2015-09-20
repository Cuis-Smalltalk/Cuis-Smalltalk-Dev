'From Cuis 4.2 of 25 July 2013 [latest update: #2504] on 19 September 2015 at 9:03:16.965699 pm'!
'Description Browse class comments for classes with names such as "Pluggable," "Morphic," "Text," or "Morph" which appear in a hierarchical list.'!
!provides: 'ClassCommentBrowser' 1 9!
!classDefinition: #CommentGuide category: #ClassCommentBrowser!
AbstractHierarchicalList subclass: #CommentGuide
	instanceVariableNames: 'window selection rootNames root'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuide class' category: #ClassCommentBrowser!
CommentGuide class
	instanceVariableNames: 'topics'!

!classDefinition: #CommentGuideWrapper category: #ClassCommentBrowser!
ObjectExplorerWrapper subclass: #CommentGuideWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuideWrapper class' category: #ClassCommentBrowser!
CommentGuideWrapper class
	instanceVariableNames: ''!

!classDefinition: #CommentGuideWindow category: #ClassCommentBrowser!
SystemWindow subclass: #CommentGuideWindow
	instanceVariableNames: 'listMorph textMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuideWindow class' category: #ClassCommentBrowser!
CommentGuideWindow class
	instanceVariableNames: ''!


!CommentGuide commentStamp: 'jmv 9/19/2015 21:02' prior: 0!
Class comments browser for Cuis

To open, evaluate:
	CommentGuide open.

Class names are organized into "categories" according to whether a certain string can be found in the class name.

The categories appear in a hierarchical list whose elements can be expanded into class names. When a class name is selected, its class comment appears in an adjacent pane.

A pop-up menu enables browsing the class, the class' hierarchy, or its protocol. Searches of the comments of classes in a category can be done. Searches over the names of all classes can be done.

This class provides the model for browsing class comments based on class names which contain certain strings, such as:

	Pluggable
	Morphic
	Debug
	Text
	Morph!

!CommentGuideWrapper commentStamp: 'dhn 9/12/2015 12:35' prior: 0!
Provide the wrapper for a list item in CommentGuideWindow.

Removes the undesired cruft after the item name.!

!CommentGuideWindow commentStamp: '<historical>' prior: 0!
Show a window for browsing class comments based on class names which contain certain strings, such as:

	Pluggable
	Morphic
	Debug
	Text
	Morph
	
The window has a text pane with a list pane beside it. 

CommentGuide open.	
!

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/10/2015 20:57'!
commentOf: aString
	"Answer the class comment of the class named aString"

	| str browser className kw1 kw2 |
	(str _ String new writeStream)
		nextPutAll: aString;
		nextPutAll: ' Class Comment:'; newLine; newLine.
	browser _ Browser new.
	kw1 _ 'setClass:' asSymbol.
	kw2 _ 'selector:' asSymbol.
	className _ Smalltalk at: aString asSymbol.
	browser 
		perform: (kw1, kw2) asSymbol 
		withArguments: (Array 	with: className with: nil).
	str nextPutAll: browser classCommentText asString.
	^ str contents
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/17/2015 21:00'!
getCurrentSelection
	"Send the class comment to the text pane"
	| str |
	selection _ super getCurrentSelection.
	selection ifNotNil: [
		selection withoutListWrapper class == String
			ifTrue: [str _ self commentOf: selection item]
			ifFalse: [
				str _ String new.
				root _ selection].
		window textMorph model actualContents: str].
	^ selection
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/12/2015 11:41'!
getList
	"Answer the roots of the hierarchical list"
	| arr col k |
	col _ self class rootCategories copy.
	col addLast: self class theRest.
	arr _ Array new: col size.
	k _ 0.
	col do: [:ea |
		k _ k + 1.
		arr at: k put: (
			CommentGuideWrapper
				with: (rootNames at: ea) asSortedCollection
				name: ea
				model: self)].
	^ arr
! !

!CommentGuide methodsFor: 'initialization' stamp: 'dhn 9/12/2015 10:56'!
initialize
	"Establish the database for the class comment browser"
	
	super initialize.
	
	rootNames _ Dictionary new.
	self class rootCategories do: [:n | rootNames at: n put: OrderedCollection new].
	rootNames at: self class theRest put: OrderedCollection new.
	
	Smalltalk allClasses do: [:c |
		| in hit |
		in _ c asString.
		hit _ false.
		
		rootNames keysAndValuesDo: [:key :val |	"try to find a root for 'in' "
			(in findString: key asString) > 0 
				ifTrue: [
					val add: in.
					hit _ true]].
			
		hit ifFalse: [(rootNames at: self class theRest) add: in]]	"the 'out' collection"
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/15/2015 18:51'!
root
	"Answer the value of root"

	^ root! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/16/2015 11:09'!
rootNames
	"Answer the value of rootNames"

	^ rootNames! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/17/2015 21:01'!
selectedRoot
	"Answer the class names containing root as a substring"
	^ rootNames at: root itemName! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/9/2015 20:42'!
selection
	"Answer the value of selection"

	^ selection! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/9/2015 18:24'!
window: anObject
	"Set the value of window"

	window _ anObject! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/11/2015 21:05'!
how
"
	CommentGuide open.
"! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/13/2015 13:37'!
open

	(CommentGuideWindow
		open: self new
		label: 'Class Comment Browser')
		setWindowColor:  Theme current browser! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/17/2015 12:11'!
rootCategories
	"Answer the names of root categories for the hierarchical list"
	
	^ OrderedCollection new
		add: 'Pluggable';
		add: 'Menu';
		add: 'Window';
		add: 'Morphic';
		add: 'Morph';
		add: 'Model';
		add: 'Block';
		add: 'Browse';
		add: 'Cat';
		add: 'Change';
		add: 'Character';
		add: 'Class';
		add: 'Closure';
		add: 'Code';
		add: 'Col';
		add: 'Collection';
		add: 'Comm';
		add: 'Comp';
		add: 'Compile';
		add: 'Cont';
		add: 'Debug';
		add: 'Del';
		add: 'Dir';
		add: 'Editor';
		add: 'Event';
		add: 'Exception';
		add: 'Feat';
		add: 'File';
		add: 'Ident';
		add: 'Inte';
		add: 'Inst';
		add: 'Key';
		add: 'List';
		add: 'Message';
		add: 'Method';
		add: 'Mouse';
		add: 'Not';
		add: 'Notif';
		add: 'Object';
		add: 'Pack';
		add: 'Parse';
		add: 'Progress';
		add: 'Proto';
		add: 'Read';
		add: 'Reference';
		add: 'Short';
		add: 'Standard';
		add: 'Stream';
		add: 'System';
		add: 'Test';
		add: 'Text';
		add: 'Time';
		add: 'Trans';
		add: 'Variable';
		add: 'Weak';
		add: 'Weight';
		yourself! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/12/2015 11:41'!
theRest
	"Answer the category for 'everything else' "
	^ 'Other Classes'! !

!CommentGuideWrapper methodsFor: 'access' stamp: 'dhn 9/12/2015 11:31'!
asString
	"Answer the string for the hierarchical list category"
	^ itemName! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 9/9/2015 21:20'!
browseClass
	"Open a browser on the selected class"
	model selection item class == String
		ifTrue: [
			BrowserWindow 
				fullOnClass: (Smalltalk at: model selection item asSymbol) 
				selector: nil]! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 9/9/2015 21:23'!
browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class"

	| aPBrowser class label |
	model selection item class == String
		ifTrue: [
			class _ Smalltalk at: model selection item asSymbol.
			aPBrowser _ ProtocolBrowser new on: class.
			label _ 'Entire protocol of: ', class name.
			MessageSetWindow open: aPBrowser label: label] ! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 9/9/2015 21:22'!
browseHierarchy
	"Create and schedule a class list browser on the selected class' hierarchy."

	model selection item class == String
		ifTrue: [
			HierarchyBrowserWindow
				onClass: (Smalltalk at: model selection item asSymbol)
				selector: nil]! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 9/12/2015 12:39'!
buildMorphicWindow
	"Answer a window for the class comment browser"
	| row |
	listMorph _ HierarchicalListMorph
		model: model
		listGetter: #getList
		indexGetter: #getCurrentSelection
		indexSetter: #noteNewSelection:
		mainView: self
		menuGetter: #commentGuideMenu
		keystrokeAction: nil.
	listMorph name: #Hierarchy.
	textMorph _ TextModelMorph textProvider: model.
	textMorph 
		name: 'Class Comment';
		askBeforeDiscardingEdits: false.
	row _ LayoutMorph newRow.
	row
		addMorph: listMorph fixedWidth: 250;
		addAdjusterAndMorph: textMorph proportionalWidth: 1.0.
	self layoutMorph
		addMorph: row.
	model window: self! !

!CommentGuideWindow methodsFor: 'menu' stamp: 'dhn 9/16/2015 11:27'!
commentGuideMenu
	"Show the popup menu"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: 'search class names'	"names, if no selection"
				target: self
				selector: #searchClassNames]
		ifNotNil: [
			aMenu addList: #(
				('search comments'		searchComment)	"comments, if selection"
				('browse full'				browseClass)
				('browse hierarchy'		browseHierarchy)
				('browse protocol'		browseFullProtocol)).

				].
	^ aMenu! !

!CommentGuideWindow methodsFor: 'menu' stamp: 'dhn 9/16/2015 19:34'!
searchClassNames
	"Search class names for a string"
	| arg str hits menu |
	arg _ FillInTheBlankMorph request: 'Search Class Names for:'.
	arg 
		ifNotEmpty: [
			hits _ OrderedCollection new.
			(menu _ MenuMorph entitled: 'Class Names With "', arg, '"')
				defaultTarget: self;
				addStayUpIcons.
			Smalltalk allClasses do: [:ea |
				str _ ea asString.
				(str findString: arg startingAt: 1 caseSensitive: false) > 0
					ifTrue: [
						hits addLast: ea asString.
			]].
		hits 
			ifEmpty: [listMorph flash]
			ifNotEmpty: [
				hits do: [:h | menu add: h selector: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)]]! !

!CommentGuideWindow methodsFor: 'menu' stamp: 'dhn 9/17/2015 21:03'!
searchComment
	"Search for a string in class comments"
	| arg str ix hits col nam menu |
	arg _ ''.
	(model selection item isKindOf: String)
		ifTrue: [	"searching from the leaf"
			col _ model selectedRoot.
			nam _ model root itemName]
		ifFalse: [	"searching from the root"
			col _ model selection item.
			nam _ model selection itemName].
		arg _ FillInTheBlankMorph request: 'Search Root Named "', nam, '"'.
		hits _ OrderedCollection new.
		(menu _ MenuMorph entitled: 'Comments in Root with "', arg, '"')
			defaultTarget: self;
			addStayUpIcons.
			
		col do: [:ea |
			str _ model commentOf: ea.
			ix _ str indexOf: Character newLineCharacter ifAbsent: [1].
			(str findString: arg startingAt: ix caseSensitive: false) > 0
				ifTrue: [hits addLast: ea]].
		hits 
			ifEmpty: [listMorph flash]
			ifNotEmpty: [
				hits do: [:h | menu add: h selector: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)].
	^ nil! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 9/10/2015 21:11'!
showFind: aName
	"Cause the class comment for aName to display"
	| str |
	str _ model commentOf: aName.
	textMorph model actualContents: str! !

!CommentGuideWindow methodsFor: 'accessing' stamp: 'dhn 8/19/2015 19:47'!
textMorph
	"Answer the value of textMorph"

	^ textMorph! !
