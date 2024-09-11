'From Cuis7.1 [latest update: #6687] on 10 September 2024 at 2:38:55 pm'!

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:37:32'!
adjustExtentFor: aBlock
	"For morphs with variable extent, ensure extent is at least minimumExtent.
	See implementors. See senders."! !


!BoxMorph methodsFor: 'private' stamp: 'jmv 9/10/2024 14:37:58'!
adjustExtentFor: aBlock
	"For morphs with variable extent, ensure extent is at least minimumExtent.
	This may mean do the same with owner.
	See implementors. See senders."

	(layoutSpec notNil and: [ owner notNil ]) ifTrue: [
		^ owner adjustExtentFor: [ self basicAdjustExtentFor: aBlock ]].
	self basicAdjustExtentFor: aBlock! !

!BoxMorph methodsFor: 'private' stamp: 'jmv 9/10/2024 14:38:22'!
basicAdjustExtentFor: aBlock
	"Ensure extent is at least minimumExtent.
	This may mean do the same with owner.
	See implementors. See senders."

	| forceMinimumExtent |
	forceMinimumExtent := extent = self minimumExtent.
	aBlock value.
	self morphExtent: (forceMinimumExtent ifTrue: [`0@0`] ifFalse: [extent]).! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2024 11:06:40'!
borderWidth: anInteger

	super borderWidth: anInteger.
	self layoutSubmorphs.! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:24:55'!
adjustExtent
	"Some morphs may need to adjust their extent after some geometry change
	either in itself, some submorpy, or an owner.
	These include morphs intended to be included in PluggableScrollPanes as scrolled content.
	See implementors. NOP by default."! !


!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 11:53:19'!
morphExtent: newExtent
	"In our own coordinates!!"

	| oldExtent |
	oldExtent := extent.
	(self privateExtent: newExtent) ifTrue: [
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:25:42'!
privateExtent: aPoint
	"May or may not honor argument: #minimumExtente will be taken into account,
	as well as other possible restrictions by subclasses. See implementors.
	Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent := aPoint max: self minimumExtent.
	^ extent = newExtent
		ifFalse: [ extent := newExtent ]; not! !


!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2024 11:23:08'!
borderWidth: anInteger

	borderWidth = anInteger ifFalse: [
		self adjustExtentFor: [ borderWidth := anInteger max: 0 ]].! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2024 11:22:26'!
padding: numberOrPoint

	| p |
	p := numberOrPoint asPoint max: `0@0`.
	padding = p ifFalse: [
		self adjustExtentFor: [ padding := p ]].! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2024 11:05:55'!
padding: numberOrPoint

	super padding: numberOrPoint.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/10/2024 11:21:33'!
separation: aNumberOrPoint

	separation = aNumberOrPoint ifFalse: [
		self adjustExtentFor: [ separation := aNumberOrPoint ]].
	self layoutSubmorphs ! !


!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 11:23:33'!
borderWidth: aNumber

	super borderWidth: aNumber.
	self reflowAndMaybeResize.! !

!methodRemoval: BoxMorph #ensureMinimumExtent stamp: 'jmv 9/10/2024 14:35:56'!
BoxMorph removeSelector: #ensureMinimumExtent!
