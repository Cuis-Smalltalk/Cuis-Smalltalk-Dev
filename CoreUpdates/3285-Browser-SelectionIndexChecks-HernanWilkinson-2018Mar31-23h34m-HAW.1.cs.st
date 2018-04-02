'From Cuis 5.0 of 7 November 2016 [latest update: #3283] on 31 March 2018 at 11:37:37 pm'!

!Browser methodsFor: 'class list' stamp: 'HAW 3/31/2018 23:34:42'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList |

	classList _ self classList.
	canSelectClass _ classList isInBounds: anInteger.
	selectedClassName _ canSelectClass ifTrue: [ classList at: anInteger ].
	self setClassOrganizer.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	
	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent _ self class recentClasses.
		className _ self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].
	
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !

!Browser methodsFor: 'message category list' stamp: 'HAW 3/31/2018 23:35:18'!
messageCategoryListIndex: anInteger
	"Set the selected message category to be the one indexed by anInteger."

	| index messageCategoryList |
	
	messageCategoryList _ self messageCategoryList.
	index _ messageCategoryList ifInBounds: anInteger ifNot: 0.

	selectedMessageCategory _ index = 0 ifFalse: [messageCategoryList at: index ].
	selectedMessage _ nil.
	self changed: #messageCategorySelectionChanged.
	self changed: #messageCategoryListIndex. "update my selection"
	self changed: #messageList.
	self editSelection: (index > 0
		ifTrue: [#newMessage]
		ifFalse: [self classListIndex > 0
			ifTrue: [#editClass]
			ifFalse: [#newClass]]).
	self acceptedContentsChanged.! !

!Browser methodsFor: 'message list' stamp: 'HAW 3/31/2018 23:35:30'!
messageListIndex: anInteger
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList _ self messageList.
	index _ messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage _ index = 0 ifFalse: [ messageList at: index ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self acceptedContentsChanged! !

!Browser methodsFor: 'system category list' stamp: 'HAW 3/31/2018 23:35:42'!
systemCategoryListIndex: anInteger
	"Set the selected system category index to be anInteger. Update all other 
	selections to be deselected."
	
	| index systemCategoryList |
	
	systemCategoryList _ self systemCategoryList.
	index _ systemCategoryList ifInBounds: anInteger ifNot: 0.
	
	selectedSystemCategory _ index = 0 ifFalse: [ systemCategoryList at: index ].
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection: ( index = 0 ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #systemCategorySelectionChanged.
	self changed: #systemCategoryListIndex.	"update my selection"
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'HAW 3/31/2018 23:36:09'!
classListIndex: newIndex

	"Cause system organization to reflect appropriate category"

	| newClassName ind i |

	(classList isInBounds: newIndex) ifTrue: [
		newClassName _ (classList at: newIndex) copyWithout: $ .
		i _ systemOrganizer numberOfCategoryOfElement: newClassName.
		selectedSystemCategory _ i = 0 ifFalse: [ self systemCategoryList at: i]].
	ind _ super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ ind! !


!MessageSet methodsFor: 'message list' stamp: 'HAW 3/31/2018 23:36:45'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."

	| list |
	
	list _ self messageList.
	selectedMessage _ (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged! !


!TimeProfileBrowser methodsFor: 'message list' stamp: 'HAW 3/31/2018 23:37:17'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."
	
	selectedMessage _ (talliesList isInBounds: anInteger) ifTrue: [ talliesList at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged! !


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit removeClass renameClass)
('class list' classList classListIndex classListIndex: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
!


!MessageSet reorganize!
('message list' addMethodReference: growable messageList messageListIndex: selectedMessageName sortByDate)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage removeMessageFromBrowser)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText selectedMessage)
('private' autoSelectString autoSelectString: contents:notifying: initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' labelString reformulateListNoting:)
('shout styling' shouldStyle:with:)
!


!TimeProfileBrowser reorganize!
('private' initializeMessageList:talliesList: runBlock:)
('message list' messageListIndex messageListIndex:)
!

