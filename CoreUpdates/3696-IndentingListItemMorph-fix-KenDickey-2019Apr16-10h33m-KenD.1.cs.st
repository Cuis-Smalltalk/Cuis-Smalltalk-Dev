'From Cuis 5.0 of 7 November 2016 [latest update: #3695] on 16 April 2019 at 10:34:27 am'!

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'KenD 4/14/2019 16:46:00'!
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
	indentLevel _ newLevel.
! !

