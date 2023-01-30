'From Cuis 6.0 [latest update: #5657] on 30 January 2023 at 9:37:16 am'!

!Morph methodsFor: 'user interface' stamp: 'jmv 1/30/2023 09:36:17'!
showAndComeToFront
	"Make me visible if not, set me on top of all other sibling morphs."
	self
		visibleBeforePreview: true;
		morphBehindBeforePreview: nil.
	self
		show;
		comeToFront.! !

!Morph methodsFor: 'previewing' stamp: 'jmv 1/30/2023 09:31:28'!
visibleBeforePreview: aBoolean

	self setProperty: #visibleBeforePreview toValue: aBoolean! !

