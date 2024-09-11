'From Cuis7.1 [latest update: #6702] on 11 September 2024 at 4:27:13 pm'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 9/11/2024 15:50:20'!
addButtonFor: aMorph

	| button taskbarButtonIcon |
	aMorph == self ifFalse: [
		button := HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #showAndComeToFront
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		(aMorph is: #SystemWindow) ifTrue: [
			button
				secondaryActionSelector: #offerWindowMenu ].
		taskbarButtonIcon := aMorph
			valueOfProperty: #taskbarButtonIcon
			ifAbsentPut: [aMorph imageForm: self defaultHeight*5//4 @ self defaultHeight depth: 32].
		button
			color: color;
			icon: taskbarButtonIcon;
			setBalloonText: #label.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

