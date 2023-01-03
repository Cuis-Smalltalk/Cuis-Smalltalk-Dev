'From Cuis 6.0 [latest update: #5616] on 3 January 2023 at 3:29:32 pm'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 1/3/2023 12:51:21'!
isHighlighted
	^super isHighlighted or: [self previewing]! !


!Morph methodsFor: 'user interface' stamp: 'jmv 1/3/2023 13:06:55'!
showAndComeToFront
	"Make me visible if not, set me on top of all other sibling morphs."
	self morphBehindBeforePreview: nil.
	self show; comeToFront.! !

!Morph methodsFor: 'previewing' stamp: 'jmv 1/3/2023 13:12:37'!
beginPreview

	owner ifNil: [ ^self ].
	self visibleBeforePreview: self visible.
	self morphBehindBeforePreview: (self owner submorphBehind: self).
	self previewing: true.
	self show; comeToFront.! !

!Morph methodsFor: 'previewing' stamp: 'jmv 1/3/2023 13:08:54'!
endPreview

	self previewing ifTrue: [
		owner notNil ifTrue: [
			self visible: self visibleBeforePreview.
			self morphBehindBeforePreview ifNotNil: [ :m |
				owner addMorph: self inFrontOf: m ].
			self morphBehindBeforePreview: nil ].
		self previewing: false ]! !

!Morph methodsFor: 'previewing' stamp: 'jmv 1/3/2023 13:08:50'!
previewing: aBoolean

	self setProperty: #previewing toValue: aBoolean.
	self redrawNeeded.! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/3/2023 12:59:15'!
addButtonFor: aMorph

	| button taskbarButtonIcon |
	aMorph == self ifFalse: [
		button _ HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #showAndComeToFront
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		(aMorph is: #SystemWindow) ifTrue: [
			button
				secondaryActionSelector: #offerWindowMenu ].
		taskbarButtonIcon _ aMorph
			valueOfProperty: #taskbarButtonIcon
			ifAbsentPut: [aMorph imageForm: self defaultHeight*5//4 @ self defaultHeight depth: 32].
		button
			color: self color;
			icon: taskbarButtonIcon;
			setBalloonText: #label.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 1/3/2023 13:11:11'!
drawHighlight: aMorph

	aMorph displayBounds ifNotNil: [ :r |
		self
			privateFrameAndFillGlobalRect: r
			fillColor: `Color pink alpha: 0.1`
			borderWidth: 2
			color: `Color black`
			].! !

!methodRemoval: Morph #endPreviewAndToggleCollapseOrShow stamp: 'jmv 1/3/2023 15:28:23'!
Morph removeSelector: #endPreviewAndToggleCollapseOrShow!
!methodRemoval: Morph #toggleCollapseOrShow stamp: 'jmv 1/3/2023 15:28:34'!
Morph removeSelector: #toggleCollapseOrShow!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
HoverableButtonMorph allInstancesDo: [ :b | b actionSelector = #endPreviewAndToggleCollapseOrShow ifTrue: [ b action: #showAndComeToFront ]]!

