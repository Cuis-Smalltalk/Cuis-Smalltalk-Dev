'From Cuis 4.2 of 25 July 2013 [latest update: #2097] on 26 September 2014 at 11:24:39.298966 am'!

!PluggableButtonMorph methodsFor: 'updating' stamp: 'jmv 9/25/2014 15:10'!
update: aSymbol

	super update: aSymbol.
	aSymbol = getStateSelector ifTrue: [
		self redrawNeeded ]! !


!Browser methodsFor: 'class functions' stamp: 'jmv 9/25/2014 15:21'!
plusButtonHit
	"Cycle among definition, comment, and hierachy"

	editSelection == #editComment
		ifTrue: [
			self hierarchy.
			^ self].
	editSelection == #hierarchy
		ifTrue: [
			self editSelection: #editClass.
			selectedClassName ifNil: [ ^self ].
			self changed: #editComment.
			self acceptedContentsChanged.
			^ self].
	self editComment.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.! !

!Browser methodsFor: 'metaclass' stamp: 'jmv 9/25/2014 15:18'!
metaClassIndicated: trueOrFalse 
	"Indicate whether browsing instance or class messages."

	metaClassIndicated _ trueOrFalse.
	self setClassOrganizer.
	selectedSystemCategory ifNotNil: [
		self editSelection: (selectedClassName
			ifNil: [metaClassIndicated
				ifTrue: [#none]
				ifFalse: [#newClass]]
			ifNotNil: [#editClass])].
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self changed: #classSelectionChanged.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged! !

!Browser methodsFor: 'system category list' stamp: 'jmv 9/25/2014 15:12'!
systemCategoryListIndex: anInteger 
	"Set the selected system category index to be anInteger. Update all other 
	selections to be deselected."

	selectedSystemCategory _ anInteger = 0 ifFalse: [ self systemCategoryList at: anInteger].
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection: ( anInteger = 0 ifTrue: [#none] ifFalse: [#newClass]).
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

