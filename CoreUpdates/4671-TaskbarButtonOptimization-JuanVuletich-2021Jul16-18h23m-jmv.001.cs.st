'From Cuis 5.0 [latest update: #4670] on 16 July 2021 at 6:24:47 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'jmv 7/16/2021 17:42:29'!
valueOfProperty: aSymbol ifAbsentPut: aBlock
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock to obtain the value to be
	stored and answered."

	^ self valueOfProperty: aSymbol ifAbsent: [
		self setProperty: aSymbol toValue: aBlock value ]! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 7/16/2021 16:57:24'!
defaultHeight

	^ (Preferences windowTitleFont lineSpacing * 2 * self scale) asInteger! !

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 7/16/2021 17:44:19'!
addButtonFor: aMorph

	| button taskbarButtonIcon |
	aMorph == self ifFalse: [
		button _ HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #endPreviewAndToggleCollapseOrShow
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

