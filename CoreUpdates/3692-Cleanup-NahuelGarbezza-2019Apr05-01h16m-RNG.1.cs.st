'From Cuis 5.0 of 7 November 2016 [latest update: #3690] on 5 April 2019 at 1:21:24 am'!
"Change Set:		3691-CuisCore-AuthorName-2019Apr05-01h16m
Date:			5 April 2019
Author:			Nahuel Garbezza

replace calls to messageListIndex: 0 by reformulateList which does that"!


!Browser methodsFor: 'message functions' stamp: 'RNG 4/5/2019 01:19:29'!
removeMessage
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName _ self selectedMessageName.
	confirmation _ Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: self selectedMessageName.
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	"In case organization not cached"
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !

!Browser methodsFor: 'initialization' stamp: 'RNG 4/5/2019 01:19:09'!
methodCategoryChanged
	self changed: #messageCategoryList.
	self changed: #messageList.
	self triggerEvent: #annotationChanged.
	self reformulateList! !


!CodeFileBrowser methodsFor: 'removing' stamp: 'RNG 4/5/2019 01:19:43'!
removeMessage
	| messageName |
	selectedMessage ifNil: [ ^self ].
	messageName _ self selectedMessageName.
	(self selectedClass confirmRemovalOf: messageName)
		ifFalse: [^ false].
	self selectedClassOrMetaClass removeMethod: self selectedMessageName.
	self reformulateList.
	self setClassOrganizer.
	"In case organization not cached"
	self changed: #messageList! !

!CodeFileBrowser methodsFor: 'removing' stamp: 'RNG 4/5/2019 01:20:08'!
removeUnmodifiedMethods
	| theClass cat |
	theClass := self selectedClassOrMetaClass.
	theClass ifNil: [ ^self].
	cat := self selectedMessageCategoryName.
	cat ifNil: [ ^self].
	theClass removeUnmodifiedMethods: (theClass organization listAtCategoryNamed: cat).
	self reformulateList.
	self changed: #messageList.! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'RNG 4/5/2019 01:18:54'!
pushUpSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushUpMethod for: model selectedClassOrMetaClass>>selectedSelector) apply.
		model reformulateList.
		model changed: #messageList.
		model setClassOrganizer ].! !

