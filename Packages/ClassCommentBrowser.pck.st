'From Cuis 4.2 of 25 July 2013 [latest update: #2505] on 7 October 2015 at 1:40:51.203402 pm'!
'Description Browse class comments for classes with names such as "Pluggable," "Morphic," "Text," or "Morph" which appear in a hierarchical list.'!
!provides: 'ClassCommentBrowser' 1 17!
!classDefinition: #CommentGuide category: #ClassCommentBrowser!
AbstractHierarchicalList subclass: #CommentGuide
	instanceVariableNames: 'window selection rootNames root browser'
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

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 10/6/2015 20:43'!
commentOf: aString
	"Answer the class comment of the class named aString"

	| className |
	
	window setLabel: 'Class Comment for ', aString.
	
	className _ Smalltalk at: aString asSymbol.
	browser setClass: className selector: nil.
	^ browser classCommentText
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 10/7/2015 13:29'!
editorClassFor: textGetter

	^ SmalltalkEditor
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 10/6/2015 20:48'!
getCurrentSelection
	"Send the class comment to the text pane"
	| str |
	selection _ super getCurrentSelection.
	selection ifNotNil: [
		selection withoutListWrapper class == String
			ifTrue: [str _ self commentOf: selection item]
			ifFalse: [
				str _ String new.
				window setLabel: 'Class Comment Browser'.
				root _ selection].
		window textMorph model actualContents: str].
	^ selection
! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/24/2015 09:08'!
getList
	"Answer the roots of the hierarchical list"
	| arr k |
	arr _ Array new: rootNames size.
	k _ 0.
	rootNames keys asSortedCollection do: [:ea |
		k _ k + 1.
		arr at: k put: (
			CommentGuideWrapper
				with: (rootNames at: ea) asSortedCollection
				name: ea
				model: self)].
	^ arr
! !

!CommentGuide methodsFor: 'initialization' stamp: 'dhn 10/6/2015 20:40'!
initialize
	"Establish the database for the class comment browser"
	
	super initialize.
	
	self rootNames: self setList.
	browser _ Browser new
! !

!CommentGuide methodsFor: 'parsing' stamp: 'dhn 9/21/2015 16:50'!
parseClassNames
	"Answer a set of initial-caps terms and acronyms which appear in class names"
	| set terms |
	set _ Set new.
	Smalltalk classNames do: [:c |
		terms _ self parseName: c asString.
		terms do: [:ea | set add: ea]].
	^ set asSortedCollection! !

!CommentGuide methodsFor: 'parsing' stamp: 'dhn 9/21/2015 18:55'!
parseName: aString
	"Answer the initial-caps components and acronyms of aString"
	| tokens acronym str |
	tokens _ aString findTokens: self class caps keep: self class caps.
	str _ OrderedCollection new.
	acronym _ ''.
	tokens do: [:ea |
		ea first isUppercase 
			ifTrue: [acronym _ acronym, ea first asString]
			ifFalse: [
				(acronym size > 1) 
					ifTrue: [
						str add: (acronym copyFrom: 1 to: acronym size - 1).
						acronym _ acronym last asString].
				str add: acronym, ea.
				acronym _ '']].
	^ str! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/15/2015 18:51'!
root
	"Answer the value of root"

	^ root! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/22/2015 17:04'!
rootNames: anObject
	"Set the value of rootNames"

	rootNames _ anObject! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/17/2015 21:01'!
selectedRoot
	"Answer the class names containing root as a substring"
	^ rootNames at: root itemName! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/9/2015 20:42'!
selection
	"Answer the value of selection"

	^ selection! !

!CommentGuide methodsFor: 'initialization' stamp: 'dhn 9/22/2015 17:09'!
setList
	"Answer the list of roots and their children for the hierarchical list"
	| dx |
	dx _ Dictionary new.
	self parseClassNames do: [:n | dx at: n put: OrderedCollection new].
	
	Smalltalk classNames do: [:c |
		| in |
		in _ c asString.
		dx keysAndValuesDo: [:key :val |	"try to find a root for 'in' "
			(in findString: key asString) > 0 ifTrue: [val add: in]]].
	^ dx! !

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/9/2015 18:24'!
window: anObject
	"Set the value of window"

	window _ anObject! !

!CommentGuide class methodsFor: 'parsing' stamp: 'dhn 9/18/2015 18:26'!
caps
	"Answer the alphabet, capitalized"
	^ $A to: $Z! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/11/2015 21:05'!
how
"
	CommentGuide open.
"! !

!CommentGuide class methodsFor: 'instance creation' stamp: 'dhn 9/23/2015 18:29'!
open
"
	CommentGuide open.
"
	(CommentGuideWindow
		open: self new
		label: 'Class Comment Browser')
		setWindowColor:  Theme current browser! !

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

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 10/2/2015 15:29'!
browseVersions
	"Open a browser on the versions of the selected class' comment"
	model selection item class == String
		ifTrue: [
			VersionsBrowserWindow 
				browseCommentOf: (Smalltalk at: model selection item asSymbol)]! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 9/21/2015 15:42'!
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
	updatablePanes _ #(listMorph).
	model window: self! !

!CommentGuideWindow methodsFor: 'menu' stamp: 'dhn 10/2/2015 15:31'!
commentGuideMenu
	"Show the popup menu"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu addList: #(
				('search all class names'		searchClassNames)
				('search all class comments'	searchAllComments))]
		ifNotNil: [
			aMenu addList: #(
				('search comments'		searchComment)	"comments, if selection"
				('comment versions'		browseVersions)
				('browse full'				browseClass)
				('browse hierarchy'		browseHierarchy)
				('browse protocol'		browseFullProtocol)).

				].
	^ aMenu! !

!CommentGuideWindow methodsFor: 'initialization' stamp: 'dhn 9/24/2015 08:47'!
initialize

	super initialize.

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #refreshListAfterDelay to: self;
		when: #classCommented send: #refreshListAfterDelay to: self;
		when: #classDefinitionChanged send: #refreshListAfterDelay to: self;
		when: #classRecategorized send: #refreshListAfterDelay to: self;
		when: #classRemoved send: #refreshListAfterDelay to: self;
		when: #classRenamed send: #refreshListAfterDelay to: self
! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 9/24/2015 09:35'!
refreshList
	"Re-build the hierarchical list"
	model 
		noteNewSelection: nil;
		rootNames: model setList.
	listMorph update: #getList! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 9/24/2015 08:45'!
refreshListAfterDelay
	"The pause that refreshes"

	self addAlarm: #refreshList after: 20 "milliseconds" ! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/21/2015 19:31'!
searchAllComments
	"Search for a string in class comments of all classes"
	| arg hits menu |
	arg _ ''.
		arg _ FillInTheBlankMorph request: 'Search All Class Comments for:'.
		(menu _ MenuMorph entitled: 'Comments of All Classes with "', arg, '"')
			defaultTarget: self;
			addStayUpIcons.
			
		hits _ self searchIn: Smalltalk classNames for: arg.
		hits 
			ifEmpty: [listMorph flash]
			ifNotEmpty: [
				hits do: [:h | menu add: h selector: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)].
	^ nil! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/21/2015 16:50'!
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
			Smalltalk classNames do: [:ea |
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

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/21/2015 19:27'!
searchComment
	"Search for a string in class comments of root siblings"
	| arg hits col nam menu |
	arg _ ''.
	(model selection item isKindOf: String)
		ifTrue: [	"searching from the leaf"
			col _ model selectedRoot.
			nam _ model root itemName]
		ifFalse: [	"searching from the root"
			col _ model selection item.
			nam _ model selection itemName].
		arg _ FillInTheBlankMorph request: 'Search Root Named "', nam, '"'.
		(menu _ MenuMorph entitled: 'Comments in Root with "', arg, '"')
			defaultTarget: self;
			addStayUpIcons.
			
		hits _ self searchIn: col for: arg.
		hits 
			ifEmpty: [listMorph flash]
			ifNotEmpty: [
				hits do: [:h | menu add: h selector: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)].
	^ nil! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/21/2015 19:15'!
searchIn: aCollection for: aString
	"Answer the names of classes in aCollection whose comments include aString"
	| str ix hits |
	hits _ OrderedCollection new.
	aCollection do: [:ea |
		str _ model commentOf: ea.
		ix _ str indexOf: Character newLineCharacter ifAbsent: [1].
		(str findString: aString startingAt: ix caseSensitive: false) > 0
			ifTrue: [hits addLast: ea]].
	^ hits! !

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

!CommentGuideWindow class methodsFor: 'instance creation' stamp: 'dhn 9/24/2015 08:48'!
how
"
	CommentGuide open.
"! !
