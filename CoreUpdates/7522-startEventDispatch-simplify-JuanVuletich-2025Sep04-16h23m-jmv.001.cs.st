'From Cuis7.5 [latest update: #7520] on 4 September 2025 at 4:23:17 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 16:06:00'!
startGenericEventDispatch: aMorphicEvent

	aMorphicEvent dispatchIn: owner.! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 16:06:06'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startGenericEventDispatch: self.! !

!methodRemoval: HandMorph #startDropEventDispatch: stamp: 'jmv 9/4/2025 16:23:15'!
HandMorph removeSelector: #startDropEventDispatch:!
!methodRemoval: HandMorph #startDropFilesEventDispatch: stamp: 'jmv 9/4/2025 16:23:15'!
HandMorph removeSelector: #startDropFilesEventDispatch:!
!methodRemoval: HandMorph #startWindowEventDispatch: stamp: 'jmv 9/4/2025 16:23:15'!
HandMorph removeSelector: #startWindowEventDispatch:!
!methodRemoval: WindowEvent #startDispatchFromHand: stamp: 'jmv 9/4/2025 16:23:15'!
WindowEvent removeSelector: #startDispatchFromHand:!
!methodRemoval: DropFilesEvent #startDispatchFromHand: stamp: 'jmv 9/4/2025 16:23:15'!
DropFilesEvent removeSelector: #startDispatchFromHand:!
!methodRemoval: DropEvent #startDispatchFromHand: stamp: 'jmv 9/4/2025 16:23:15'!
DropEvent removeSelector: #startDispatchFromHand:!
