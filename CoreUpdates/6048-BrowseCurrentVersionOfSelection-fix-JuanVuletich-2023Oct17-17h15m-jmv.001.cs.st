'From Cuis 6.0 [latest update: #6047] on 17 October 2023 at 5:16:55 pm'!

!ChangeList methodsFor: 'accessing' stamp: 'jmv 10/17/2023 16:47:09'!
currentCompiledMethod
	"Actually, the CompiledMethod installed in the system that correspond to current selection"
	| selector |
	selector := self selectedMessageName.
	(selector isNil or: [ MessageSet isPseudoSelector: selector ]) ifTrue: [
		^nil ].
	^self selectedClassOrMetaClass compiledMethodAt: selector! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 10/17/2023 16:49:58'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"

	| currentVersions |
	currentVersions := model currentVersionsOfSelections.
	currentVersions isEmpty ifTrue: [
		^ self inform: 'no selected methods have in-memory counterparts' ].
	MessageSetWindow
		openMessageListUnsorted: currentVersions
		label: 'Current versions of selected methods in ', model file localName.! !

