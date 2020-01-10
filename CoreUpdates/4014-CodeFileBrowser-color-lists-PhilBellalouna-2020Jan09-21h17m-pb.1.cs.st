'From Cuis 5.0 [latest update: #4012] on 10 January 2020 at 12:21:32 am'!

!CodeFileBrowser methodsFor: 'message list' stamp: 'pb 1/10/2020 00:20:12'!
messageList
	"Colorize messages as needed"
	^ super messageList collect: [ :eaListItem | | foundCat useAttr |
		foundCat _ classOrganizer categoryOfElement: eaListItem.
		"
		Transcript
			show: foundCat class name;
			finishEntry.
			"
		useAttr _ foundCat = PseudoClass removedCategoryName
			ifTrue: [ TextColor red ]
			ifFalse: [ | baseSrc |
				baseSrc _ self pvtBaseSelectedMessageSourceCodeFor: eaListItem.
				baseSrc
					ifNil: [ TextColor green ]
					ifNotNil: [ | caseSrc |
						caseSrc _ self pvtCaseSelectedMessageSourceCodeFor: eaListItem.
						baseSrc = caseSrc ifFalse: [ TextColor blue ]]].
		useAttr
			ifNil: [ eaListItem ]
			ifNotNil: [ :attr |
				Text
					string: eaListItem
					attribute: attr ]].! !

!CodeFileBrowser methodsFor: 'private' stamp: 'pb 1/9/2020 23:39:45'!
pvtBaseSelectedMessageSourceCodeFor: selector
	^ self pvtBaseClassOrMetaclass ifNotNil: [ :theClass | | useClass |
		self metaClassIndicated
			ifTrue: [ useClass _ theClass class ]
			ifFalse: [ useClass _ theClass ].
		(useClass includesSelector: selector) ifTrue: [ useClass sourceCodeAt: selector ]].! !

!CodeFileBrowser methodsFor: 'private' stamp: 'pb 1/9/2020 23:40:02'!
pvtCaseSelectedMessageSourceCodeFor: selector
	| class  |
	class _ self selectedClassOrMetaClass.
	^ class sourceCodeAt: selector.! !


!PseudoClass class methodsFor: 'categories' stamp: 'pb 1/9/2020 22:19:49'!
removedCategoryName
	^ `Text string: '*** removed methods ***'  attribute: TextColor red`! !


!Categorizer methodsFor: 'accessing' stamp: 'pb 1/9/2020 21:21:38'!
addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	| index newCategory |
	newCategory _ catString .
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^self].	"heading already exists, so done"
	index _ categoryArray indexOf: nextCategory
		ifAbsent: [categoryArray size + 1].
	categoryArray _ categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops _ categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).
	"remove empty default category"
	(newCategory ~= Default
			and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]! !

!Categorizer methodsFor: 'accessing' stamp: 'pb 1/9/2020 21:21:47'!
classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	((heading = NullCategory) or: [heading == nil])
		ifTrue: [realHeading _ Default]
		ifFalse: [realHeading _ heading ].
	(catName _ self categoryOfElement: element) = realHeading
		ifTrue: [^ self].  "done if already under that category"

	catName ifNotNil: [
		(aBoolean and: [realHeading = Default])
				ifTrue: [^ self].	  "return if non-Default category already assigned in memory"
		self removeElement: element].	"remove if in another category"

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex _ categoryArray indexOf: realHeading.
	elemIndex _ 
		catIndex > 1
			ifTrue: [categoryStops at: catIndex - 1]
			ifFalse: [0].
	[(elemIndex _ elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray _ elementArray copyReplaceFrom: elemIndex to: elemIndex-1
						with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: 
		[:i | categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: Default) size = 0 ifTrue: [self removeCategory: Default]! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'pb 1/9/2020 22:41:38'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout msgLayout clsList msgList |
	model systemCategoryListIndex: 1.
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
	clsLayout := self buildMorphicClassColumn.
	msgLayout := self buildMorphicMessageList.
	clsList := clsLayout findDeepSubmorphThat: [:tstMorph| tstMorph class = PluggableListMorph] ifAbsent: [nil].
	msgList := msgLayout findDeepSubmorphThat: [:tstMorph| tstMorph class = PluggableListMorph] ifAbsent: [nil].
	sysCatList rightSibling: clsList.
	clsList leftSibling: sysCatList rightSibling: msgCatList.
	msgCatList leftSibling: clsList rightSibling: msgList.
	msgList leftSibling: msgCatList .
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgLayout proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'pb 1/9/2020 22:20:09'!
fileInMethods: aCollection
	"FileIn all methods with selectors taken from aCollection"
	| theClass |
	self exists ifFalse:[^self classNotDefined].
	theClass := self realClass.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			theClass 
				compile: (self sourceCodeAt: sel) 
				classified: cat
				withStamp: (self stampAt: sel)
				notifying: nil.
		].
	].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'pb 1/9/2020 22:20:04'!
fileOutMethods: aCollection on: aStream
	"FileOut all methods with selectors taken from aCollection"
	| categories |
	categories := Dictionary new.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			(categories includesKey: cat) 
				ifFalse:[ categories at: cat put: Set new ].
			(categories at: cat) add: sel].
	].
	categories associationsDo:[:assoc|
		assoc value do: [ :sel |
			aStream newLine.
			(self sourceCode at: sel) fileOutOn: aStream.
		].
	].! !

!PseudoClass methodsFor: 'methods' stamp: 'pb 1/9/2020 22:19:59'!
removeSelector: aSelector
	| catName |
	catName := self class removedCategoryName.
	self organization addCategory: catName before: self organization categories first.
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.! !


!SequenceDifference methodsFor: 'printing' stamp: 'pb 1/9/2020 23:51:15'!
attributesFor: condition 
	condition == #unchanged
		ifTrue: [ 
			^ {TextEmphasis normal} ].
	condition == #removed
		ifTrue: [ 
			^ {TextEmphasis struckThrough. TextColor red} ].
	condition == #inserted
		ifTrue: [ 
			^ {TextColor green} ]! !

!methodRemoval: PseudoClass #removedCategoryName stamp: 'pb 1/9/2020 22:19:49'!
PseudoClass removeSelector: #removedCategoryName!

!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText baseCodeSource baseCodeSource: caseCodeSource caseCodeSource: contents:notifying: contentsSymbol)
('class list' classList renameClass selectedClass)
('diffs' methodDiffFor:class:selector:meta: modifiedClassDefinition)
('edit pane' selectedBytecodes selectedMessage)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('menus' contentsSymbolQuints)
('message list' messageList)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('shout styling' shouldStyle:with:)
('private' pvtBaseClassOrMetaclass pvtBaseSelectedMessageSourceCodeFor: pvtCaseSelectedMessageSourceCodeFor:)
!

