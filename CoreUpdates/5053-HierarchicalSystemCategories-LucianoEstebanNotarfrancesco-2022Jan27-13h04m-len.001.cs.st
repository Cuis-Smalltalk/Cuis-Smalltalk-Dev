'From Cuis 6.0 [latest update: #5049] on 27 January 2022 at 1:38:52 pm'!
!classDefinition: #SystemCategoryWrapper category: #'Tools-Browser'!
ListItemWrapper subclass: #SystemCategoryWrapper
	instanceVariableNames: 'itemName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!Browser methodsFor: 'system category list' stamp: 'len 1/25/2022 08:19:55'!
selectedSystemCategory
	"Answer the name of the selected system category or nil."

	^selectedSystemCategory! !

!Browser methodsFor: 'system category list' stamp: 'len 1/25/2022 08:48:29'!
setSelectedSystemCategory: aString
	"Set the selected system category and update all other selections to be deselected."
	selectedSystemCategory _ aString.
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection: (aString isNil ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #selectedSystemCategory.
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged! !

!Browser methodsFor: 'system category list' stamp: 'len 1/27/2022 13:19:44'!
setSelectedSystemCategoryTreeItem: aSystemCategoryWrapper
	aSystemCategoryWrapper ifNotNil: [self setSelectedSystemCategory: aSystemCategoryWrapper withoutListWrapper]! !

!Browser methodsFor: 'system category list' stamp: 'len 1/27/2022 13:12:19'!
systemCategoryRoots
	"Answer the roots of the forest of system categories."
	| subcategories roots name |
	subcategories _ Dictionary new.
	self systemCategoryList do: [:each|
		name _ (each findTokens: '-') first.
		(subcategories at: name ifAbsentPut: [OrderedCollection new]) add: each].
	roots _ OrderedCollection new.
	self systemCategoryList do: [:each|
		name _ (each findTokens: '-') first.
		(roots includes: name) ifFalse:
			[(subcategories at: name) size > 1 ifTrue: [roots add: name] ifFalse: [roots add: (subcategories at: name) first]]].
	^ roots collect: [:each| SystemCategoryWrapper with: each name: each model: self]! !


!HierarchicalListMorph methodsFor: 'accessing' stamp: 'len 6/7/2020 05:36:17'!
roots
	"Answer the root items. These are instances of IndentingListItemMorph and submorphs of the scroller."
	| list |
	list _ self getList collect: [:each| each withoutListWrapper].
	^ scroller submorphs select: [:each| list includes: each withoutListWrapper]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 1/27/2022 13:13:34'!
buildMorphicSystemCategoryList
	^(HierarchicalListMorph
		model: model
		listGetter: #systemCategoryRoots
		indexGetter: #selectedSystemCategory
		indexSetter: #setSelectedSystemCategoryTreeItem:
		mainView: self
		menuGetter: #systemCategoryMenu
		keystrokeAction: #systemCatListKey:from:)
			autoDeselect: true;
			autoExpand: true;
			yourself! !


!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'len 6/7/2020 05:02:26'!
isAncestorOf: anObject
	^ false! !

!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'len 6/6/2020 16:29:09'!
printOn: aStream
	aStream nextPutAll: self asString! !


!SystemCategoryWrapper methodsFor: 'accessing' stamp: 'len 1/27/2022 13:33:52'!
contents
	| prefix subcategories |
	(item includes: $-) ifTrue: [^ #()].
	prefix _ item, '-'.
	subcategories _ OrderedCollection new.
	model systemCategoryList do: [:each|
		(each beginsWith: prefix)
			ifTrue: [subcategories add: (each copyFrom: prefix size + 1 to: each size)]].
	^ subcategories collect: [:each| self class with: prefix, each name: each model: model]! !

!SystemCategoryWrapper methodsFor: 'testing' stamp: 'len 6/6/2020 12:48:40'!
canBeDragged
	^ false! !

!SystemCategoryWrapper methodsFor: 'testing' stamp: 'len 1/26/2022 07:00:37'!
isAncestorOf: aString
	^ (item includes: $-) not and: [aString beginsWith: item, '-']! !

!SystemCategoryWrapper methodsFor: 'private' stamp: 'len 6/6/2020 12:46:34'!
setItem: anObject name: aString model: aModel
	item _ anObject.
	model _ aModel.
	itemName _ aString! !

!SystemCategoryWrapper methodsFor: 'converting' stamp: 'len 6/6/2020 16:28:40'!
asString
	^ itemName! !


!SystemCategoryWrapper class methodsFor: 'instance creation' stamp: 'len 6/6/2020 12:46:06'!
with: anObject name: aString model: aModel
	^ self new  setItem: anObject name: aString model: aModel! !


!Browser methodsFor: 'annotation' stamp: 'len 1/25/2022 08:20:16'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategory ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !

!Browser methodsFor: 'class functions' stamp: 'len 1/27/2022 13:12:42'!
defineClass: defString notifying: aRequestor  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryRoots.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !

!Browser methodsFor: 'class list' stamp: 'len 1/27/2022 13:28:49'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	| answer prefix |
	selectedSystemCategory ifNil: [^ #()].
	answer _ systemOrganizer listAtCategoryNamed: selectedSystemCategory.
	prefix _ selectedSystemCategory, '-'.
	systemOrganizer categories do: [:each| (each beginsWith: prefix) ifTrue: [answer _ answer, (systemOrganizer listAtCategoryNamed: each)]].
	^ answer! !

!Browser methodsFor: 'class list' stamp: 'len 1/25/2022 08:44:48'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ self class recentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className _ (SelectionMenu selections: recentList) startUpMenu.
	className ifNil: [^ self].
	class _ Smalltalk at: className.
	self setSelectedSystemCategory: class category.
	self selectClass: class! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:26'!
addSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| newName |
	newName _ self newSystemCategoryNameIfNone: [ ^self ].	
	systemOrganizer
		addCategory: newName
		before: selectedSystemCategory.
	self setSelectedSystemCategory: newName.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:30'!
alphabetizeSystemCategories
	systemOrganizer sortCategories.
	self setSelectedSystemCategory: nil.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:36'!
changeSystemCategories: aString 
	"Update the class categories by parsing the argument aString."

	systemOrganizer changeFromString: aString.
	self changed: #systemCategoryRoots.
	^ true! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/25/2022 08:41:44'!
editSystemCategories
	"Retrieve the description of the class categories of the system organizer."

	self setSelectedSystemCategory: nil.
	self editSelection: #editSystemCategories.
	self changed: #editSystemCategories.
	self acceptedContentsChanged! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:48'!
moveAllToOtherSystemCategory
	"If a class category is selected, prompt user for category to move to,
	create a Confirmer so the user can verify that all the classes in current category
 	should be moved to the selected category."
	| newSystemCategory |
	selectedSystemCategory ifNil: [ ^ self ].
	newSystemCategory _ Smalltalk systemCategoryFromUserWithPrompt: 'Move classes to System Category...'.
	(newSystemCategory notNil and: [
		self classList size > 0 and: [ self confirm: 'Are you sure you want to
move classes from ' , selectedSystemCategory , ' 
to ' , newSystemCategory , '?' ]]) ifTrue: [
		"Safer this way (#classList will be a collection of strings with spaces and who knows what in the future.  So let's just get the classes we need directly)"
		(SystemOrganization classesAt: selectedSystemCategory) do: [ :eaClass |
			eaClass category: newSystemCategory ].
		self changed: #systemCategoryRoots ].! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:53'!
moveSystemCategoryBottom
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryBottom: selectedSystemCategory.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:12:58'!
moveSystemCategoryDown
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryDown: selectedSystemCategory.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:13:03'!
moveSystemCategoryTop
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryTop: selectedSystemCategory.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:13:07'!
moveSystemCategoryUp
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryUp: selectedSystemCategory.
	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:13:13'!
removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList size = 0
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self setSelectedSystemCategory: nil.
			self changed: #systemCategoryRoots ]! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:13:21'!
renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	| oldName newName |
	selectedSystemCategory ifNil: [ ^ self].  "no selection"
	oldName _ selectedSystemCategory.
	newName _ self
		request: 'New category name?'
		initialAnswer: oldName
		verifying: [:aString| aString notEmpty and: [aString ~= oldName]]
		do: [:aString|
			newName _ aString asSymbol.
			systemOrganizer
				renameCategory: oldName
				toBe: newName.
			self setSelectedSystemCategory: newName.
			self changed: #systemCategoryRoots]! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/25/2022 08:46:41'!
systemCategoryBrowser
	"Create a new system category browser with initial textual 
	contents set to aString."

	| newBrowser |
	selectedSystemCategory
		ifNotNil: [
			newBrowser _ Browser new.
			newBrowser setSelectedSystemCategory: self selectedSystemCategory.
			newBrowser setClass: self selectedClassOrMetaClass selector: self selectedMessageName.
			^newBrowser].
	^nil! !

!Browser methodsFor: 'system category functions' stamp: 'len 1/27/2022 13:13:26'!
updateSystemCategories
	"The class categories were changed in another browser. The receiver must 
	reorganize its lists based on these changes."

	self changed: #systemCategoryRoots! !

!Browser methodsFor: 'initialization' stamp: 'len 1/25/2022 08:45:06'!
setClass: aBehavior selector: aSymbol
	"Set the state of a new, uninitialized Browser."

	| isMeta aClass messageCatIndex |
	aBehavior ifNil: [^ self].
	aBehavior isMeta
		ifTrue: [
			isMeta _ true.
			aClass _ aBehavior soleInstance]
		ifFalse: [
			isMeta _ false.
			aClass _ aBehavior].
	self setSelectedSystemCategory: aClass category.
	self selectClass: aClass.
	self metaClassIndicated: isMeta.
	aSymbol ifNil: [^ self].
	messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: (messageCatIndex > 0
		ifTrue: [messageCatIndex + 1]
		ifFalse: [0]).
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex: (
		(aBehavior organization listAtCategoryNumber: messageCatIndex)
			indexOf: aSymbol).! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'len 1/27/2022 13:17:22'!
classListIndex: newIndex
	"Cause system organization to reflect appropriate category"

	| newClassName index |

	(classList isInBounds: newIndex) ifTrue: [
		newClassName _ (classList at: newIndex) withoutLeadingBlanks.
		selectedSystemCategory _ systemOrganizer categoryOfElement: newClassName].
	index _ super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ index! !


!Categorizer methodsFor: 'accessing' stamp: 'len 1/27/2022 13:37:32'!
listAtCategoryNamed: categoryName
	"Answer the array of elements associated with the name, categoryName."

	^ self listAtCategoryNumber: (categoryArray indexOf: categoryName ifAbsent: [^ #()])! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/2/2021 14:45:19'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my left edge
		- and my text left edge"

	| aMorphCenter hLineY hLineLeft rect right |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	hasToggle ifFalse: [
		rect _ anIndentingListItemMorph toggleRectangle.
		aMorphCenter _ anIndentingListItemMorph externalize: rect center.
		right _ (anIndentingListItemMorph externalize: rect rightCenter) x.
		hLineY _ aMorphCenter y.
		hLineLeft _ aMorphCenter x - 1.
		aCanvas
			line: hLineLeft @ hLineY
			to: right @ hLineY
			width: 1
			color: lineColor ]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'len 1/23/2022 11:48:29'!
drawLinesFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor 
	anIndentingListItemMorph indentLevel > 0 ifTrue: [
		| hasToggle |
		hasToggle _ anIndentingListItemMorph hasToggle.
	
		"Draw line from toggle to text"
		self
			drawLineToggleToTextFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor
			hasToggle: hasToggle.

		"Draw the line from toggle to the nextSibling's toggle"
		anIndentingListItemMorph nextSibling ifNotNil: [
			self
				drawLinesToNextSiblingFor: anIndentingListItemMorph
				on: aCanvas
				lineColor: lineColor
				hasToggle: hasToggle ]].

	"If it have children and am expanded, draw a line to its first child"
	(anIndentingListItemMorph firstChild notNil and: [
			anIndentingListItemMorph isExpanded ]) ifTrue: [
		self
			drawLinesToFirstChildFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 09:30'!
drawLinesOn: aCanvas 
	| lColor bottomY topY tx clipRect |
	lColor _ Theme current line.
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	topY _ (tx internalizePosition: clipRect topLeft) y min: (tx internalizePosition: clipRect topRight) y.
	bottomY _ (tx internalizePosition: clipRect bottomLeft) y max: (tx internalizePosition: clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/20/2012 21:49'!
drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter firstChild |
	"child in the drawn tree. it is acually our submorph"
	firstChild _ anIndentingListItemMorph firstChild.
	childCenter _ firstChild externalize: firstChild toggleRectangle center.
	vLineX _ childCenter x - 1.
	vLineTop _ (anIndentingListItemMorph
		externalize: anIndentingListItemMorph morphExtent) y.
	firstChild hasToggle
		ifTrue: [ vLineBottom _ childCenter y - 7 ]
		ifFalse: [ vLineBottom _ childCenter y ].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: 1
		color: lineColor! !


!HierarchicalListMorph methodsFor: 'commands' stamp: 'len 6/7/2020 05:36:50'!
expandAllAsPer: aBlock

	scroller submorphs isEmpty ifTrue: [^self].
	self roots do: [:each| each beFullyExpandedAsPer: aBlock].
	scroller adjustExtent.
	self setScrollDeltas! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'len 6/7/2020 05:31:21'!
privateVisualSelection: item
	"Called internally to set a new selection.
	Does not update model"
	| index |
	item isNil ifTrue: [^ self privateVisualSelectionIndex: 0].
	index _ self indexForItem: item.
	index > 0 ifFalse:
		[self expandAllAsPer: [:each| each complexContents isAncestorOf: item withoutListWrapper].
		index _ self indexForItem: item].
	self privateVisualSelectionIndex: index! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 1/27/2022 13:08:33'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCategoryList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
"	systemCatList rightSibling: classList."
	classList leftSibling: systemCatList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	classList makeItemsDraggable.
"	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
"		
	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!BrowserWindow methodsFor: 'menu commands' stamp: 'len 1/25/2022 08:21:41'!
openSystemCategoryBrowser
	"Create and schedule a new system category browser with initial textual
	contents set to aString."

	model systemCategoryBrowser ifNotNil: [ :newBrowser |
		newBrowser selectedSystemCategory ifNotNil: [ :category |
			BrowserWindow
				openNoSysCat: newBrowser
				label: 'Classes in category ', category ]]! !

!BrowserWindow methodsFor: 'updating' stamp: 'len 1/25/2022 08:20:58'!
classAdded: addedClass

	self model selectedSystemCategory = addedClass category 
		ifTrue: [ self model changed: #classList ]! !

!BrowserWindow methodsFor: 'updating' stamp: 'len 1/25/2022 08:21:31'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| currentSelectedClass |
	
	self canDiscardEdits ifTrue: [
		self model selectedSystemCategory = aCategoryName ifTrue: [
			currentSelectedClass := self model selectedClass.
			currentSelectedClass ifNil: [ 
				self model originalSelectedClassName = oldClassName ifTrue: [ 
					currentSelectedClass := aClass ]].
			
		 	self model changed: #classList.
			self model selectClass: currentSelectedClass ]]! !

!BrowserWindow methodsFor: 'commands' stamp: 'len 1/25/2022 08:45:23'!
findClass

	| scopedClassNames |
	
	scopedClassNames _ model potentialClassNames asOrderedCollection.
	
	self class 
		findClassFrom: scopedClassNames 
		ifFound: [:foundClass |
			model setSelectedSystemCategory: foundClass category.
			model selectClass: foundClass ]! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'len 1/25/2022 09:07:08'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.
	sysCatList rightSibling: clsList.
	clsList leftSibling: sysCatList rightSibling: msgCatList.
	msgCatList leftSibling: clsList rightSibling: msgList.
	msgList leftSibling: msgCatList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !

!CodeFileBrowserWindow methodsFor: 'commands' stamp: 'len 1/25/2022 09:08:23'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	self request: 'Class name?' do: [:aString|
		aString isEmpty ifFalse:
			[pattern _ aString asLowercase.
			classNames _ Set new.
			classNames addAll: model caseCodeSource classDictionary keys.
			classNames _ classNames asArray select: 
				[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
			classNames isEmpty ifFalse:
				[index _ classNames size = 1
					ifTrue: [1]
					ifFalse: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
				index = 0 ifFalse:
					[foundCodeFile _ nil.
					foundClass _ nil.
					(model caseCodeSource classDictionary includesKey: (classNames at: index))
						ifTrue:
							[foundClass _ model caseCodeSource classDictionary at: (classNames at: index).
							foundCodeFile _ model caseCodeSource].
					foundClass ifNotNil:
						[model setSelectedSystemCategory: foundCodeFile name asSymbol.
						model classListIndex: (model classList indexOf: foundClass name)]]]]]! !

!methodRemoval: BrowserWindow #buildMorphicSystemCatList stamp: 'len 1/27/2022 13:07:19'!
BrowserWindow removeSelector: #buildMorphicSystemCatList!
!methodRemoval: BrowserWindow #buildMorphicSystemCategoryTree stamp: 'len 1/27/2022 13:08:37'!
BrowserWindow removeSelector: #buildMorphicSystemCategoryTree!
!methodRemoval: Browser #systemCategoryTree stamp: 'len 1/27/2022 13:13:57'!
Browser removeSelector: #systemCategoryTree!
!methodRemoval: Browser #systemCategoryListIndex: stamp: 'len 1/27/2022 13:07:33'!
Browser removeSelector: #systemCategoryListIndex:!
!methodRemoval: Browser #selectedSystemCategoryName stamp: 'len 1/27/2022 13:06:59'!
Browser removeSelector: #selectedSystemCategoryName!
!methodRemoval: Browser #systemCategoryListIndex stamp: 'len 1/27/2022 13:07:35'!
Browser removeSelector: #systemCategoryListIndex!
!methodRemoval: Browser #categorizeUnderNewCategoryClass: stamp: 'len 1/27/2022 13:06:19'!
Browser removeSelector: #categorizeUnderNewCategoryClass:!
!methodRemoval: Browser #categorizeUnderCategoryAt:class: stamp: 'len 1/27/2022 13:06:42'!
Browser removeSelector: #categorizeUnderCategoryAt:class:!
!methodRemoval: Browser #selectCategoryForClass: stamp: 'len 1/27/2022 13:06:48'!
Browser removeSelector: #selectCategoryForClass:!

SystemOrganization
        renameCategory: #'Theme-Core' toBe: #'Graphics-Themes';
        renameCategory: #'System-TextAttributes' toBe: #'System-Text-Attributes';
        renameCategory: #'System-FileMan-Core' toBe: #'System-File System';
        renameCategory: #'Exceptions Kernel' toBe: #'Exceptions-Kernel';
        renameCategory: #'Exceptions Extensions' toBe: #'Exceptions-Extensions'!

