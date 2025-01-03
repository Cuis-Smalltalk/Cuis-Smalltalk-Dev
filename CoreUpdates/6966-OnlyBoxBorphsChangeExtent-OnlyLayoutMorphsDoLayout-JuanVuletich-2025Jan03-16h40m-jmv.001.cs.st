'From Cuis7.3 [latest update: #6963] on 3 January 2025 at 4:53:24 pm'!

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:50:57'!
extentChanged: oldExtent
	"Our extent changed. oldExtent is provided in case it is useful."! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:49:14'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	self layoutNeeded: true.! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 1/3/2025 16:42:25'!
layoutSubmorphsIfNeeded
	"Recompute the layout if necessary."

	self isLayoutNeeded ifTrue: [
		self layoutSubmorphs ].! !


!Morph methodsFor: 'layout' stamp: 'jmv 1/3/2025 16:42:37'!
layoutSubmorphsIfNeeded
	"Recompute the layout if necessary.
	NOP by default"! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:51:11'!
extentChanged: oldExtent
	"Our extent changed."

	super extentChanged: oldExtent.
	icon ifNotNil: [
		magnifiedIcon := nil ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:51:49'!
extentChanged: oldExtent
	"Our extent changed."

	super extentChanged: oldExtent.
	self buildMagnifiedBackgroundImage.! !


!LayoutSpec methodsFor: 'private' stamp: 'jmv 1/3/2025 16:46:43'!
updateContainingLayout
	| owningLayout |
	morph ifNotNil: [
		owningLayout := morph owner.
		(owningLayout is: #LayoutMorph) ifTrue:
			owningLayout layoutSubmorphs ].! !

!methodRemoval: Morph #extentChanged: stamp: 'jmv 1/3/2025 16:52:44'!
Morph removeSelector: #extentChanged:!
!methodRemoval: Morph #layoutSubmorphs stamp: 'jmv 1/3/2025 16:47:13'!
Morph removeSelector: #layoutSubmorphs!
