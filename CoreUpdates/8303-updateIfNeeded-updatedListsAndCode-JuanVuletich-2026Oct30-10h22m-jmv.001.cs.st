'From Cuis7.9 [latest update: #8302] on 30 October 2026 at 10:24:21 am'!

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 30/Oct/2026, 1:06:40 pm (UTC)'!
updateListsAndCode
	self acceptedContentsChanged.! !


!Browser methodsFor: 'self-updating' stamp: 'jmv 30/Oct/2026, 1:10:21 pm (UTC)'!
updateListsAndCode
	super updateListsAndCode.
	self changed: #systemCategoryRoots.! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:10:13 pm (UTC)'!
updateListsAndCode
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		self showChangeSet: ChangeSet changeSetForBaseSystem.
		^self ].

	newList := self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList := newList.
			self changed: #changeSetList ].

	self acceptedContentsChanged.! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 30/Oct/2026, 1:06:54 pm (UTC)'!
updateListsAndCode
	| aMethod |
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 30/Oct/2026, 1:23:13 pm (UTC)'!
update
	"Recompute all of my lists."

	self updateListsAndCode.! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 30/Oct/2026, 1:23:18 pm (UTC)'!
updateListsAndCodeNow
	"All code windows receive this message on any code change in the system"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updateListsAndCode ].! !

!methodRemoval: MethodVersionsBrowser #updateIfNeeded stamp: 'jmv 30/Oct/2026, 1:23:32 pm (UTC)'!
MethodVersionsBrowser removeSelector: #updateIfNeeded!
!methodRemoval: ChangeSorter #updateIfNeeded stamp: 'jmv 30/Oct/2026, 1:23:31 pm (UTC)'!
ChangeSorter removeSelector: #updateIfNeeded!
!methodRemoval: Browser #updateIfNeeded stamp: 'jmv 30/Oct/2026, 1:23:27 pm (UTC)'!
Browser removeSelector: #updateIfNeeded!
!methodRemoval: CodeProvider #updateIfNeeded stamp: 'jmv 30/Oct/2026, 1:23:31 pm (UTC)'!
CodeProvider removeSelector: #updateIfNeeded!
