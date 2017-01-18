'From Cuis 5.0 of 7 November 2016 [latest update: #3037] on 18 January 2017 at 10:36:09 am'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 1/18/2017 10:34:55'!
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
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 1/18/2017 10:35:05'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.
	Preferences taskbarIncludesAllWindows ifTrue: [
		aMorph submorphsDo: [ :m |
			self addButtonFor: m ]].
	self notifyDisplayResize! !

