'From Cuis 4.2 of 25 July 2013 [latest update: #1885] on 18 December 2013 at 12:16:06.01441 am'!

!Browser methodsFor: 'class functions' stamp: 'sqr 12/16/2013 23:29'!
renameClass
	| oldName newName obs renamedClass |
	selectedClassName ifNil: [ ^self ].
	oldName _ self selectedClass name.
	newName _ self request: 'Please type new class name' initialAnswer: oldName.
	newName = '' ifTrue: [^ self].  " Cancel returns '' "
	newName _ newName asSymbol.
	newName = oldName ifTrue: [^ self].
	(Smalltalk includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	renamedClass _ self selectedClass.
	renamedClass rename: newName.
	self changed: #classList.
	self selectClass: renamedClass.
	obs _ Smalltalk allCallsOn: (Smalltalk associationAt: newName).
	obs isEmpty ifFalse: [
		Smalltalk 
			browseMessageList: obs
			name: 'Obsolete References to ' , oldName
			autoSelect: oldName
	].
! !

!Browser methodsFor: 'class list' stamp: 'sqr 12/16/2013 23:26'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ RecentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUp.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self selectClass: class! !

!Browser methodsFor: 'initialization' stamp: 'sqr 12/16/2013 23:23'!
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
	self selectCategoryForClass: aClass.
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

