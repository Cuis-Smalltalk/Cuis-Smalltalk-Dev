'From Cuis7.5 [latest update: #7531] on 9 September 2025 at 11:28:58 am'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/9/2025 09:03:57'!
noticeMouseOver: aMorph event: aMouseOverEvent
	"Remember that the mouse is currently over some morph"
	(leftMorphs includes: aMorph) 
		ifTrue: [ leftMorphs remove: aMorph ]
		ifFalse: [ enteredMorphs nextPut: aMorph ].
	overMorphs nextPut: aMorph.! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 9/9/2025 09:03:04'!
noticeMouseOver: aMorph event: aMouseOverEvent
	
	self mouseOverHandler noticeMouseOver: aMorph event: aMouseOverEvent.! !

