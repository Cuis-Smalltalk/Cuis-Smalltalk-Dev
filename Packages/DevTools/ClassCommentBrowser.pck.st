'From Cuis 5.0 of 7 November 2016 [latest update: #3727] on 11 May 2019 at 2:53:07 pm'!
'Description List classes whose names include terms such as "Pluggable," "Morphic," "Text," or "Morph", search their class comments or open browsers on them.'!
!provides: 'ClassCommentBrowser' 1 46!
SystemOrganization addCategory: #ClassCommentBrowser!


!classDefinition: #CommentGuide category: #ClassCommentBrowser!
AbstractHierarchicalList subclass: #CommentGuide
	instanceVariableNames: 'window rootNames root browser subList index key'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuide class' category: #ClassCommentBrowser!
CommentGuide class
	instanceVariableNames: 'topics'!

!classDefinition: #NotCommented category: #ClassCommentBrowser!
ActiveModel subclass: #NotCommented
	instanceVariableNames: 'classList bumList bumListIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'NotCommented class' category: #ClassCommentBrowser!
NotCommented class
	instanceVariableNames: ''!

!classDefinition: #CommentGuideListMorph category: #ClassCommentBrowser!
HierarchicalListMorph subclass: #CommentGuideListMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuideListMorph class' category: #ClassCommentBrowser!
CommentGuideListMorph class
	instanceVariableNames: ''!

!classDefinition: #CommentGuideWindow category: #ClassCommentBrowser!
SystemWindow subclass: #CommentGuideWindow
	instanceVariableNames: 'listMorph textMorph nwin hwin'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuideWindow class' category: #ClassCommentBrowser!
CommentGuideWindow class
	instanceVariableNames: ''!

!classDefinition: #NoCommentsWindow category: #ClassCommentBrowser!
SystemWindow subclass: #NoCommentsWindow
	instanceVariableNames: 'aList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'NoCommentsWindow class' category: #ClassCommentBrowser!
NoCommentsWindow class
	instanceVariableNames: ''!

!classDefinition: #CommentGuideWrapper category: #ClassCommentBrowser!
ObjectExplorerWrapper subclass: #CommentGuideWrapper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClassCommentBrowser'!
!classDefinition: 'CommentGuideWrapper class' category: #ClassCommentBrowser!
CommentGuideWrapper class
	instanceVariableNames: ''!


!CommentGuide commentStamp: 'dhn 5/11/2019 14:52:06' prior: 0!
Class comments browser for Cuis

To open, evaluate:
	CommentGuide open.

Class names are organized into "categories" according to whether a certain string can be found in the class name. The categories consist of all terms and acronyms which appear in Cuis class names. Terms are strings with initial caps. Acronyms are strings of all-caps.

This class provides the model for browsing class comments based on class names which contain categories such as:

	Pluggable
	Morphic
	ST80
	93
	Debug
	Text
	Morph
	
The view for this model is CommentGuideWindow.!

!NotCommented commentStamp: '<historical>' prior: 0!
The model for view of classes without comments.!

!CommentGuideListMorph commentStamp: 'dhn 6/15/2016 17:30' prior: 0!
A list of terms and acronyms which occur in class names. Selection can be made by typed key, arrows, or clicks.

This class enables access to the scroller, which is needed for selection by typed key.!

!CommentGuideWindow commentStamp: 'dhn 5/11/2019 14:50:00' prior: 0!
Show a window for browsing class comments based on class names which contain terms such as:

	Pluggable
	Morphic
	ST80
	93
	Debug
	Text
	Morph

Use the menu 'World > Help... > Class Comment Browser' or: 

CommentGuide open.	
	
The window has a text pane with a list pane beside it. 

The categories appear in a hierarchical list whose elements can be expanded into class names. When a class name is selected, its class comment appears in an adjacent pane.

Class additions and deletions are automatically reflected in the list.

Within the class names of a category, a pop-up menu enables:

- browsing the class
- browsing the hierarchy of the class
- browsing the protocol of the class
- browsing versions of the class comment
- searching the comments of all classes in a category
 
In the outer list of categories, a pop-up menu enables:

- searches over the names of all classes
- searches over all class comments
!

!NoCommentsWindow commentStamp: '<historical>' prior: 0!
A view for listing classes without comments.!

!CommentGuideWrapper commentStamp: 'dhn 9/12/2015 12:35' prior: 0!
Provide the wrapper for a list item in CommentGuideWindow.

Removes the undesired cruft after the item name.!

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

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 11/5/2015 14:58'!
getCurrentSelection
	"Send the class comment to the text pane"
	| str |

	super getCurrentSelection.
	currentSelection ifNotNil: [
		currentSelection withoutListWrapper class == String
			ifTrue: [str _ self commentOf: currentSelection item]
			ifFalse: [
				str _ ''.
				window setLabel: 'Class Comment Browser'.
				root _ currentSelection].
		window textMorph model actualContents: str].
	^ currentSelection
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

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 11/5/2015 15:22'!
indexOf: aChar
	"Set the current selection to the root which starts with aChar"
	| sel ch |

	key = aChar
		ifTrue: [
			index _ index + 1.
			index > subList size
				ifTrue: [index _ 1]]
		ifFalse: [
			index _ 1.
			key _ aChar.
			ch _ key asString capitalized.
			subList _ self getList select: [:ea | ch  = ea itemName first asString].
			window setLabel: 'Class Comment Browser'.
			window textMorph model actualContents: ''].
	subList ifEmpty: [window listMorph flash.	^ 0].
	sel _ subList at: index.
	currentSelection _ sel.
	currentSelection ifNil: [^self].
	currentSelection sendSettingMessageTo: self
! !

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

!CommentGuide methodsFor: 'accessing' stamp: 'dhn 9/9/2015 18:24'!
window: anObject
	"Set the value of window"

	window _ anObject! !

!CommentGuide methodsFor: 'initialization' stamp: 'dhn 10/6/2015 20:40'!
initialize
	"Establish the database for the class comment browser"
	
	super initialize.
	
	self rootNames: self setList.
	browser _ Browser new
! !

!CommentGuide methodsFor: 'initialization' stamp: 'dhn 11/25/2015 10:23'!
setList
	"Answer the dictionary of roots and their children for the hierarchical list"
	| dx |
	dx _ Dictionary new.
	self parseClassNames do: [:n | dx at: n put: OrderedCollection new].
	
	Smalltalk classNames do: [:ea |
		| in |
		in _ ea asString.
		(self parseName: in) do: [:term | (dx at: term) add: in]].
	^ dx! !

!CommentGuide methodsFor: 'parsing' stamp: 'dhn 9/21/2015 16:50'!
parseClassNames
	"Answer a set of initial-caps terms and acronyms which appear in class names"
	| set terms |
	set _ Set new.
	Smalltalk classNames do: [:c |
		terms _ self parseName: c asString.
		terms do: [:ea | set add: ea]].
	^ set asSortedCollection! !

!CommentGuide methodsFor: 'parsing' stamp: 'dhn 1/28/2016 17:24'!
parseName: aString
	"Answer the initial-caps components and acronyms of aString"
	| tokens acronym str |
	
	tokens _ aString 
		findTokens: self class tokenChars 
		keep: self class tokenChars.
	str _ OrderedCollection new.
	acronym _ ''.
	tokens do: [:ea |
		(ea first isUppercase or: ea first isDigit)
			ifTrue: [acronym _ acronym, ea first asString]
			ifFalse: [
				(acronym size > 1) 
					ifTrue: [
						str add: (acronym copyFrom: 1 to: acronym size - 1).
						acronym _ acronym last asString].
				str add: acronym, ea.
				acronym _ '']].
	acronym isEmpty ifFalse: [str add: acronym].
	^ str! !

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

!CommentGuide class methodsFor: 'parsing' stamp: 'dhn 1/28/2016 17:12'!
tokenChars
	"Answer the alphabet, capitalized, plus the digits"
	^ ($A to: $Z), ($0 to: $9)! !

!NotCommented methodsFor: 'operating' stamp: 'dhn 5/10/2019 21:08:59'!
bumListIndex
	"Answer the value of bumListIndex"
	
	bumListIndex ifNil: [bumListIndex _ 0].
	^ bumListIndex! !

!NotCommented methodsFor: 'operating' stamp: 'dhn 5/10/2019 21:11:02'!
bumListIndex: anIndex
	"Set the value of bumListIndex"
	
	bumListIndex _ anIndex.
	self changed: #bumListIndex! !

!NotCommented methodsFor: 'accessing' stamp: 'dhn 5/10/2019 21:34:40'!
bumList
	"Answer the list of classes"
	
	^ bumList! !

!NotCommented methodsFor: 'accessing' stamp: 'dhn 5/10/2019 21:35:36'!
bumList: aCollection
	"Set the list of classes"
	
	bumList _ aCollection! !

!NotCommented methodsFor: 'accessing' stamp: 'dhn 5/9/2019 21:12:37'!
classList
	"Answer the value of classList"

	classList ifNil: [classList _ OrderedCollection new].
	^ classList! !

!NotCommented methodsFor: 'accessing' stamp: 'dhn 5/9/2019 21:07:40'!
classList: anObject
	"Set the value of classList"

	classList _ anObject! !

!NotCommented methodsFor: 'accessing' stamp: 'dhn 5/9/2019 21:43:01'!
getList

	^ self bumList! !

!NotCommented methodsFor: 'initialization' stamp: 'dhn 5/10/2019 13:01:53'!
initialize

	bumList _ OrderedCollection new.
	bumListIndex _ 0! !

!CommentGuideListMorph methodsFor: 'events-processing' stamp: 'dhn 6/15/2016 16:56'!
keySelection
	"Scroll the hiearchical list based on a keystroke"
	| i |
	
	model getCurrentSelection ifNil: [^self selectionIndex: 0].
	i _ scroller submorphs findFirst: [:m | m contents == model getCurrentSelection itemName].
	^ self selectionIndex: i! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 11/5/2015 14:53'!
browseClass
	"Open a browser on the selected class"
	model getCurrentSelection item class == String
		ifTrue: [
			BrowserWindow 
				fullOnClass: (Smalltalk at: model getCurrentSelection item asSymbol) 
				selector: nil]! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 1/11/2018 16:48:17'!
browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class"

	| aPBrowser class label |
	model getCurrentSelection item class == String
		ifTrue: [
			class _ Smalltalk at: model getCurrentSelection item asSymbol.
			aPBrowser _ ProtocolBrowser new on: class.
			label _ 'Entire protocol of: ', class name.
			ProtocolBrowserWindow open: aPBrowser label: label] ! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 11/5/2015 14:54'!
browseHierarchy
	"Create and schedule a class list browser on the selected class' hierarchy."

	model getCurrentSelection item class == String
		ifTrue: [
			HierarchyBrowserWindow
				onClass: (Smalltalk at: model getCurrentSelection item asSymbol)
				selector: nil]! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 11/5/2015 14:54'!
browseVersions
	"Open a browser on the versions of the selected class' comment"
	model getCurrentSelection item class == String
		ifTrue: [
			VersionsBrowserWindow 
				browseCommentOf: (Smalltalk at: model getCurrentSelection item asSymbol)]! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 5/11/2019 14:06:50'!
help
	"Information on how to use Class Coment Browser"
	
	^ 'Two different pop-up menus appear, depending on whether there is a selection in the list or not.

If no selection:
	Search all class comments - which contain a string.
	Search all class names - using a fragment or a full name.
	Help... - show this page.
	
If selection (expanded or not):
	Search comments - of a class or all classes in the selection.
	Comment versions - open a version window for the class comment
	Browse full - open a browser for the named class
	Browse hierarchy - open a hierarchy browser for the named class
	Browse protocol - open a protocol browser for the named class
	Help... - show this page.

Typing a character causes the list to scroll to the next entry which begins with that character. The same character can be repeated to cycle through that part of the list.

This browser lists all the terms, acronyms, and numbers which appear in ''Smalltalk classNames''. It finds the complete class names containing these.
'
	
	! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 5/11/2019 12:44:12'!
openHelp
	"Show help screen"
	
	hwin _ (TextModel new contents: self help; openLabel: 'Class Comment Guide Help')
		morphExtent: 600@500! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 5/19/2016 11:19'!
scrollToClass: aString
	"Scroll to an entry for the class named aString"
	| cat m1 m2 str |
	
	"ensure that the first category which contains aString is expanded"
	cat _ (model parseName: aString) first.
	m1 _ listMorph findDeepSubmorphThat: [:ea | 
		(ea respondsTo: #contents) and: [ea contents includesSubString: cat]] ifAbsent: [nil].
	m1 ifNotNil: [m1 isExpanded ifFalse: [listMorph toggleExpandedState: m1]].
	
	"select the class entry and scroll it into view"
	str _  aString asString printString.
	m2 _ listMorph findDeepSubmorphThat: [:ea | 
		(ea respondsTo: #contents) and: [ea contents includesSubString: str]] ifAbsent: [nil].
	m2 ifNotNil: [
		listMorph 
			setSelectedMorph: m2;
			scrollSelectionIntoView]! !

!CommentGuideWindow methodsFor: 'browsing' stamp: 'dhn 5/9/2019 17:07:07'!
showFind: aName
	"Cause the class comment for aName to display; answer the comment"
	| cmt |

	cmt _ model commentOf: aName.
	textMorph model actualContents: cmt.
	self scrollToClass: aName.
	^ cmt! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 6/16/2016 13:08'!
buildMorphicWindow
	"Answer a window for the class comment browser"
	| row |
	listMorph _ CommentGuideListMorph
		model: model
		listGetter: #getList
		indexGetter: #getCurrentSelection
		indexSetter: #noteNewSelection:
		mainView: self
		menuGetter: #commentGuideMenu
		keystrokeAction: #respondToKey:.
	listMorph name: #Hierarchy.
	self when: #getList send: #update: to: listMorph withArguments: #(#getList).
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

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 6/16/2016 13:17'!
refreshList
	"Re-build the hierarchical list"
	model 
		noteNewSelection: nil;
		rootNames: model setList.
	self triggerEvent: #getList 	"needed when a class is added or deleted"! !

!CommentGuideWindow methodsFor: 'instance creation' stamp: 'dhn 9/24/2015 08:45'!
refreshListAfterDelay
	"The pause that refreshes"

	self addAlarm: #refreshList after: 20 "milliseconds" ! !

!CommentGuideWindow methodsFor: 'menu' stamp: 'dhn 5/11/2019 12:10:21'!
commentGuideMenu
	"Show the popup menu"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: 'Search all class comments'	action: #searchAllComments;
				add: 'Search all class names'		action: #searchClassNames;
				add: 'Classes without comments'	action: #noComment;
				add: 'Help...'							action: #openHelp]
		ifNotNil: [
			aMenu
				add: 'Search comments'		action: #searchComment;
				add: 'Comment versions'		action: #browseVersions;
				add: 'Browse full'			action: #browseClass;
				add: 'Browse hierarchy'		action: #browseHierarchy;
				add: 'Browse protocol'		action: #browseFullProtocol;
				add: 'Help...'					action: #openHelp].
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

!CommentGuideWindow methodsFor: 'accessing' stamp: 'dhn 10/27/2015 20:49'!
listMorph
	"Answer the value of listMorph"

	^ listMorph! !

!CommentGuideWindow methodsFor: 'accessing' stamp: 'dhn 8/19/2015 19:47'!
textMorph
	"Answer the value of textMorph"

	^ textMorph! !

!CommentGuideWindow methodsFor: 'events' stamp: 'dhn 6/15/2016 16:56'!
respondToKey: aChar
	"Take action when a key is struck. The key value is aChar"

	model indexOf: aChar.
	listMorph keySelection! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 5/11/2019 13:40:32'!
noComment
	"Search for classes with no comments"
	| hits mod |
	
	hits _ Smalltalk allClasses select: [:eaClass | 
		eaClass organization classComment size<10].
	hits
		ifEmpty: [listMorph flash]
		ifNotEmpty: [
			mod _ NotCommented new.
			mod bumList: hits.
			nwin _ (NoCommentsWindow open: mod label: 'Classes with no Comments') morphExtent: 400@600]! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 5/9/2019 16:53:44'!
searchAllComments
	"Search for a string in class comments of all classes"
	| arg hits caption menu |
	
	arg _ ''.
	arg _ FillInTheBlankMorph request: 'Search All Class Comments for:'.
	caption _ 'Classes with Comments containing "', arg, '"'.
			
	hits _ self searchIn: Smalltalk classNames for: arg.
	hits 
		ifEmpty: [listMorph flash]
		ifNotEmpty: [
			(menu _ MenuMorph entitled: caption)
				defaultTarget: self;
				addStayUpIcons.
				hits do: [:h | menu add: h target: self action: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)].
					
	^ nil! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/18/2018 17:55:56'!
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
				hits do: [:h | menu add: h target: self action: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)]]! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 9/18/2018 18:16:02'!
searchComment
	"Search for a string in class comments of root siblings"
	| arg hits col nam menu |
	arg _ ''.
	(model getCurrentSelection item isKindOf: String)
		ifTrue: [	"searching from the leaf"
			col _ model selectedRoot.
			nam _ model root itemName]
		ifFalse: [	"searching from the root"
			col _ model getCurrentSelection item.	"gather every leaf"
			nam _ model getCurrentSelection itemName].
		arg _ FillInTheBlankMorph request: 'Search Root Named "', nam, '"'.
		(menu _ MenuMorph entitled: 'Comments in Root with "', arg, '"')
			defaultTarget: self;
			addStayUpIcons.

		hits _ self searchIn: col for: arg.
		hits 
			ifEmpty: [listMorph flash]
			ifNotEmpty: [
				hits do: [:h | menu add: h target: self action: #showFind: argument: h].
				menu 
					openInWorld;
					morphPosition: self morphPosition + (-70@50)].
	^ nil! !

!CommentGuideWindow methodsFor: 'searching' stamp: 'dhn 11/5/2015 16:13'!
searchIn: aCollection for: aString
	"Answer the names of classes in aCollection whose comments include aString"
	| str hits |
	hits _ OrderedCollection new.
	aCollection do: [:ea |
		str _ model commentOf: ea.
		(str findString: aString startingAt: 1 caseSensitive: false) > 0
			ifTrue: [hits addLast: ea]].
	^ hits! !

!CommentGuideWindow methodsFor: 'open/close' stamp: 'dhn 5/11/2019 12:45:22'!
closeBoxHit
	"Close the window of classes with no comments and help window, if any"
	
	nwin ifNotNil: [nwin delete].
	hwin ifNotNil: [hwin delete].
	super closeBoxHit! !

!NoCommentsWindow methodsFor: 'initialization' stamp: 'dhn 5/10/2019 21:31:05'!
buildMorphicWindow
	"Build the window for the list of classes without comments"
	
	aList _ (PluggableListMorph
				model: model
				listGetter: #bumList  "answers the list content"
				indexGetter: #bumListIndex  "answers the selection made in the list"
				indexSetter: #bumListIndex:  "sets the list index"
				mainView: self
				menuGetter: nil
				keystrokeAction: nil)
					name: 'The List'.
	layoutMorph addMorphUseAll: aList
! !

!CommentGuideWrapper methodsFor: 'access' stamp: 'dhn 9/12/2015 11:31'!
asString
	"Answer the string for the hierarchical list category"
	^ itemName! !
