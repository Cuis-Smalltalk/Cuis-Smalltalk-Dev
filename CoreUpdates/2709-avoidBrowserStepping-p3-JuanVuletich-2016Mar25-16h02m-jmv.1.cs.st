'From Cuis 4.2 of 25 July 2013 [latest update: #2708] on 25 March 2016 at 4:02:33.600134 pm'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/25/2016 15:58'!
updateIfNeeded
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList ].

	newList _ self changeSetDirtyFlags.
	(priorDirtyFlags == nil or: [priorDirtyFlags ~= newList])
		ifTrue: [
			priorDirtyFlags _ newList.
			self changed: #changeSetDirtyFlags ].
	self acceptedContentsChanged! !

!methodRemoval: ChangeSorterWindow #updateListsAndCode!
ChangeSorterWindow removeSelector: #updateListsAndCode!

!ChangeSorter reorganize!
('access' changeSet currentCanHavePreambleAndPostscript currentHasPostscript currentHasPreamble label labelString methodInfoFromRemoval: myChangeSet myChangeSet: showChangeSet: showChangeSetNamed:)
('changeSet menu' addPreamble changeSetDirtyFlags changeSetList currentCngSet editPostscript fileOutAndKeep fileOutAndRemove mainButtonName methodConflicts remove removePostscript removePreamble removePrompting: rename trimHistory update updateIfNeeded viewAffectedClassCategories)
('class list' classList currentClassName currentClassName: forgetClass selectedClass selectedClassOrMetaClass)
('message list' currentSelector currentSelector: forget messageList removeMessage selectedMessageName)
('code pane' acceptedStringOrText contents:notifying: toggleDiffing)
('annotation' addPriorVersionsCountForSelector:ofClass:to:)
('shout styling' shoutAboutToStyle:)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu building' changeSetMenu classListMenu messageMenu)
('menu commands' browseMethodConflicts browseVersions)
('keyboard shortcuts' changeSetListKey:from: classListKey:from: messageListKey:from:)
!

