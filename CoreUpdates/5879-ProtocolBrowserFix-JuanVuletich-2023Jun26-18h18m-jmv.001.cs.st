'From Cuis 6.0 [latest update: #5878] on 26 June 2023 at 6:24:37 pm'!

!MessageSet methodsFor: 'initialization' stamp: 'jmv 6/26/2023 18:21:20'!
initialize

	super initialize.
	shouldAutoHighlight := false.! !


!MessageSet methodsFor: 'shout styling' stamp: 'jmv 6/26/2023 18:23:27'!
autoHighlightString: aString allOccurrences: aBoolean
	"Set the string to be automatically highlighted."

	shouldAutoHighlight := true.
	autoHighlightString := aString.
	autoHighlightAllOccurrences := aBoolean.! !

!methodRemoval: TimeProfileBrowser #initialize stamp: 'jmv 6/26/2023 18:19:48'!
TimeProfileBrowser removeSelector: #initialize!

!MessageSet reorganize!
('message list' addMethodReference: addMethodReference:ifIncluded: growable messageList messageListIndex: removeMessageFromBrowser removeMessageFromBrowserKeepingLabel selectedMessage selectedMessageName sortByClassHierarchy sortByClassName sortByDate sortBySelector sortReverse)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedStringOrText)
('private' initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' contents:notifying: labelString reformulateListNoting:)
('shout styling' autoHighlightAllOccurrences autoHighlightString autoHighlightString:allOccurrences: justDidAutoHighlight messageSendsRangesOf: shouldAutoHighlight shouldStyle:with:)
('initialization' initialize)
!

