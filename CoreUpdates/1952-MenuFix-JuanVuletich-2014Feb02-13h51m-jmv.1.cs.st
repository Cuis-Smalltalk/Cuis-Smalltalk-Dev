'From Cuis 4.2 of 25 July 2013 [latest update: #1951] on 2 February 2014 at 1:52:28.846829 pm'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 2/2/2014 01:11'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.
	self world startSteppingSubmorphsOf: self.

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

