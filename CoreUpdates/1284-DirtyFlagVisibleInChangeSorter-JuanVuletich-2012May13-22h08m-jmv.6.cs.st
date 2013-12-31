'From Cuis 4.0 of 21 April 2012 [latest update: #1282] on 13 May 2012 at 10:38:14 pm'!
!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'myChangeSet currentClassName currentSelector priorChangeSetList priorDirtyFlags '
	classVariableNames: 'AllChangeSets '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 5/13/2012 22:28'!
changeSetDirtyFlags

	^ (ChangeSorter allChangeSets collect: [ :each |
		(each isForBaseSystem and: [ each hasUnsavedChanges ])
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]) reversed! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 5/13/2012 22:23'!
initialExtent
	^720@480! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 5/13/2012 22:31'!
fileOutAndKeep
	"File out the current change set."

	myChangeSet fileOut.
	self update! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 5/13/2012 22:37'!
updateIfNecessary
	"Recompute all of my panes."

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
			self changed: #changeSetDirtyFlags ]! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 5/13/2012 22:27'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes |
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: Color white.
	dirtyFlags _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: 16;
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: Color white.
	changeSetList _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Change Set name') fixedHeight: 16;
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: Color white.
	classList _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Classes') fixedHeight: 16;
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.5;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: Color white.
	messageList _ LayoutMorph newColumn
		color: Color veryLightGray;
		addMorph: (Morph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: 'Methods') fixedHeight: 16;
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.5.

	self setLabel: model labelString! !

!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'myChangeSet currentClassName currentSelector priorChangeSetList priorDirtyFlags'
	classVariableNames: 'AllChangeSets'
	poolDictionaries: ''
	category: 'Tools-Changes'!
