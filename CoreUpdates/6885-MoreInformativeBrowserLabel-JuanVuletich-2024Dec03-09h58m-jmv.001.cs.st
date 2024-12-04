'From Cuis7.1 [latest update: #6884] on 3 December 2024 at 9:59:19 am'!

!Browser methodsFor: 'message category list' stamp: 'jmv 12/3/2024 09:51:36'!
messageCategoryListIndex: anInteger
	"Set the selected message category to be the one indexed by anInteger."

	| index messageCategoryList |
	
	messageCategoryList := self messageCategoryList.
	index := messageCategoryList ifInBounds: anInteger ifNot: 0.

	selectedMessageCategory := index = 0 ifFalse: [messageCategoryList at: index ].
	selectedMessage := nil.
	self changed: #messageCategorySelectionChanged.
	self changed: #messageCategoryListIndex. "update my selection"
	self changed: #messageList.
	self editSelection: (index > 0
		ifTrue: [#newMessage]
		ifFalse: [self classListIndex > 0
			ifTrue: [#editClass]
			ifFalse: [#newClass]]).
	self changed: #relabel.
	self acceptedContentsChanged.! !

!Browser methodsFor: 'message list' stamp: 'jmv 12/3/2024 09:42:10'!
messageListIndex: anInteger
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList := self messageList.
	index := messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage := index = 0 ifFalse: [ (messageList at: index) string ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self changed: #relabel.
	self acceptedContentsChanged! !

!Browser methodsFor: 'initialization' stamp: 'jmv 12/3/2024 09:50:51'!
labelString
	^self selectedClassOrMetaClass
		ifNil: [ self defaultBrowserTitle ]
		ifNotNil: [ :c |
			self selectedMessageName
				ifNil: [
					self selectedMessageCategoryName
						ifNil: [ self defaultBrowserTitle, ': ', c printString ]
						ifNotNil: [ :cat | self defaultBrowserTitle, ': ', c printString, ' - ', cat ]]
				ifNotNil: [ :m |
					self defaultBrowserTitle, ': ', c printString, ' >> ', m printString ]].

! !

