'From Cuis 4.2 of 25 July 2013 [latest update: #2250] on 17 April 2015 at 2:06:45.950607 pm'!

!Browser class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 14:02'!
recentClasses
	RecentClasses ifNil: [
		RecentClasses := OrderedCollection new ].
	^RecentClasses! !

!Browser class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 14:02'!
releaseClassCachedState
	"
	Browser releaseClassCachedState
	"
	RecentClasses _ nil! !


!Browser methodsFor: 'class list' stamp: 'jmv 4/17/2015 14:04'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent |

	selectedClassName _ anInteger = 0 ifFalse: [ self classList at: anInteger ].
	self setClassOrganizer.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self classCommentIndicated
		ifFalse: [
			self editSelection: (anInteger = 0
				ifTrue: [
					metaClassIndicated | selectedSystemCategory isNil
						ifTrue: [#none]
						ifFalse: [#newClass]]
				ifFalse: [#editClass])].
	self selectedClass
		ifNotNil: [
			recent _ self class recentClasses.
			className _ self selectedClass name.
			(recent includes: className)
				ifTrue: [recent remove: className].
			recent addFirst: className.
			recent size > 16
				ifTrue: [recent removeLast]].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !

!Browser methodsFor: 'class list' stamp: 'jmv 4/17/2015 14:04'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ self recentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUpMenu.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self selectClass: class! !

!methodRemoval: BitBltCanvas class #initialize!
BitBltCanvas class removeSelector: #initialize!
BitBltCanvas initialize!
!methodRemoval: Browser class #initialize!
Browser class removeSelector: #initialize!
Browser initialize!

!Browser class reorganize!
('instance creation' new)
('class initialization')
('cached state access' recentClasses releaseClassCachedState)
!

