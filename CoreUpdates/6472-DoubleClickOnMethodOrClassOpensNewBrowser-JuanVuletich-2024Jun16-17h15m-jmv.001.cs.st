'From Cuis7.1 [latest update: #6471] on 16 June 2024 at 5:20:03 pm'!

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/14/2024 15:17:41'!
buildMorphicClassList

	^(PluggableListMorph
		withModel: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:)
			menuNshortcutsKey: #classListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/14/2024 15:17:47'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 6/14/2024 15:17:50'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !


!HierarchyBrowserWindow reorganize!
('as yet unclassified')
!


!MethodSetWindow class reorganize!
('opening' findInSourceCode open:label: openMessageList:label: openMessageList:label:autoHighlight:allOccurrences: openMessageListUnsorted:label: openMessageListUnsorted:label:autoHighlight:allOccurrences:)
!


!ProtocolBrowserWindow class reorganize!
('as yet unclassified' openFullProtocolForClass:)
!

