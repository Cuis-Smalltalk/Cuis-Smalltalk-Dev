'From Cuis 4.2 of 25 July 2013 [latest update: #2762] on 10 May 2016 at 10:15:11.666515 am'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 5/9/2016 20:41'!
justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	newOwnerMorph activateWindow! !

!Morph methodsFor: 'initialization' stamp: 'jmv 5/9/2016 20:41'!
openInWorld: aWorld
	"Add this morph to the requested World."
	(location = MorphicTranslation new)
		ifTrue: [ aWorld addMorph: self position: 50@50 ]
		ifFalse: [ aWorld addMorph: self ]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 5/9/2016 20:42'!
replaceSubmorph: oldMorph by: newMorph
	| index |
	oldMorph stopStepping.
	index _ submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	self privateAddMorph: newMorph atIndex: index! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 5/9/2016 20:40'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 5/9/2016 20:42'!
acceptDroppingMorph: aMorph event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	self isWorldMorph 
		ifTrue: [	
			"Add the given morph to this world and start stepping it if it wants to be."
			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Smalltalk beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m is: #HaloMorph) ifTrue: [ m delete ]]! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 5/9/2016 20:38'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ RealEstateAgent initialFrameFor: self world: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	WorldState addDeferredUIMessage: [ self activate ]! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 5/9/2016 20:39'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphPosition: aWorld morphExtent - extent // 2 extent: w@h.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
	aWorld addMorph: self! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 5/9/2016 20:33'!
newMorphOfClass: morphClass event: evt
	"Attach a new morph of the given class to the invoking hand."

	| m |
	m _ morphClass initializedInstance.
	evt hand attachMorph: m! !

!methodRemoval: PasteUpMorph #addAllMorphs:!
PasteUpMorph removeSelector: #addAllMorphs:!
!methodRemoval: PasteUpMorph #startSteppingSubmorphsOf:!
PasteUpMorph removeSelector: #startSteppingSubmorphsOf:!
