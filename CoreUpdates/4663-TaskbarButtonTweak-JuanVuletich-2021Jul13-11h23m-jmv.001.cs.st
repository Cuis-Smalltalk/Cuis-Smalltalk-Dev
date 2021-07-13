'From Cuis 5.0 [latest update: #4662] on 13 July 2021 at 11:29:20 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/13/2021 10:20:27'!
icon
	^ (self imageForm: 16@16 depth: 32)
		ifNil: [ Theme current morphsIcon ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 7/13/2021 11:25:11'!
imageForm: extent depth: depth

	^(self imageForm: depth) magnifyTo: extent! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 7/13/2021 11:28:28'!
addButtonFor: aMorph

	| button |
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
		button
			color: self color;
			icon: (aMorph imageForm: self defaultHeight*5//4 @ self defaultHeight depth: 32);
			setBalloonText: #label.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

