'From Cuis7.1 [latest update: #6712] on 12 September 2024 at 11:32:08 am'!

!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 9/12/2024 11:31:55'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w := self new.
	w color: (Color r: 0.167 g: 0.344 b: 0.629).
	w position: `0@0` extent: Display extent.
	w activeHand privateOwner: w.
	^w! !

