'From Cuis 6.0 [latest update: #6062] on 30 October 2023 at 12:09:27 pm'!

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 10/30/2023 10:26:51'!
tearDownDesktop
	self hideTaskbar.
	submorphs 
		do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]].! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 10/30/2023 10:35:17'!
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
	w borderWidth: 0.
	w morphPosition: `0@0` extent: Display extent.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !

