'From Cuis 5.0 of 7 November 2016 [latest update: #3039] on 18 January 2017 at 10:26:44 pm'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/18/2017 22:25:29'!
addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ PluggableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #showAndComeToFront.
		button
			color: self color;
			icon: (aMorph imageForm: 32);
			setBalloonText: aMorph label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

