'From Cuis 4.2 of 25 July 2013 [latest update: #2365] on 12 June 2015 at 8:49:49.374026 am'!
"Change Set:		2366-CuisCore-AuthorName-2015Jun12-08h29m
Date:			12 June 2015
Author:			Juan Vuletich

<your descriptive text goes here>"
HandMorph allInstancesDo: [ :h | h grabMorphDataDictionary ]!


!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'jmv 6/11/2015 10:55'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ acceptDroppedMorphs! !

!WorkspaceWindow methodsFor: 'access' stamp: 'jmv 6/11/2015 10:56'!
toggleAcceptDroppedMorphs

	acceptDroppedMorphs _ acceptDroppedMorphs not! !

!WorkspaceWindow methodsFor: 'initialization' stamp: 'jmv 6/11/2015 10:55'!
initialize

	super initialize.
	acceptDroppedMorphs _ false! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 6/12/2015 08:47'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| morphPos |
	self releaseMouseFocus. "Break focus"
	morphPos := aMorph morphPositionInWorld.
	grabMorphData
		at: aMorph 
		put: { formerOwner. morphPos. }.
	self addMorphBack: aMorph position: morphPos - self morphPositionInWorld.
	aMorph justGrabbedFrom: formerOwner.
! !

!HandMorph methodsFor: 'private' stamp: 'jmv 6/12/2015 08:47'!
forgetGrabMorphDataFor: aMorph

	grabMorphData 
		removeKey: aMorph 
		ifAbsent: [ "no error" ]! !

!HandMorph methodsFor: 'private' stamp: 'jmv 6/12/2015 08:47'!
grabMorphDataFor: aMorph

	^ grabMorphData at: aMorph ifAbsent: [ { nil. nil. } ]! !

!HandMorph methodsFor: 'private' stamp: 'jmv 6/12/2015 08:47'!
rememberGrabMorphDataFor: aMorph

	grabMorphData
		at: aMorph 
		put: { aMorph owner. aMorph morphPositionInWorld. }! !

!methodRemoval: DropEvent #setPosition:contents:hand:!
DropEvent removeSelector: #setPosition:contents:hand:!
!methodRemoval: HandMorph #grabMorph:positionInWorld:!
HandMorph removeSelector: #grabMorph:positionInWorld:!
!methodRemoval: HandMorph #grabMorphDataDictionary!
HandMorph removeSelector: #grabMorphDataDictionary!
!methodRemoval: WorkspaceWindow #acceptDroppedMorphs!
WorkspaceWindow removeSelector: #acceptDroppedMorphs!
!methodRemoval: WorkspaceWindow #acceptDroppedMorphs:!
WorkspaceWindow removeSelector: #acceptDroppedMorphs:!
