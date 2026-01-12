'From Cuis7.7 [latest update: #7794] on 12 January 2026 at 11:57:40 am'!

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'JEC 12/Jan/2026 11:55:07'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container := hostList.
	complexContents := anObject.
	self initWithContents: anObject asString font: nil emphasis: nil.
	indentLevel := 0.
	isExpanded := false.
 	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o := anObject withoutListWrapper.
	icon := o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	icon isSymbol ifTrue: [ icon := Theme current perform: icon ].
	indentLevel := newLevel.
! !

