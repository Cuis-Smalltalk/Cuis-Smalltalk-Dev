'From Cuis 5.0 [latest update: #4488] on 18 December 2020 at 11:16:52 am'!

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'KLG 12/18/2020 11:13:07'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: Preferences standardListFont emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o _ anObject withoutListWrapper.
	icon _ o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	icon isSymbol ifTrue: [ icon _ Theme current perform: icon ].
	indentLevel _ newLevel.
! !

