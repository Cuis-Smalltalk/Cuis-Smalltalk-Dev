'From Cuis 5.0 of 7 November 2016 [latest update: #3351] on 21 June 2018 at 2:06:07 pm'!

!PluggableMorph methodsFor: 'access' stamp: 'jmv 6/20/2018 22:42:04'!
balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	| balloonText |
	balloonText _ super balloonText.
	balloonText isSymbol ifTrue: [ ^model perform: balloonText ].
	^ balloonText! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/20/2018 22:38:58'!
setBalloonText: stringTextOrSymbol
	"Set receiver's balloon help text. Pass nil to remove the help."

	 stringTextOrSymbol
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [
			self setProperty: #balloonText
			    	 toValue: ((stringTextOrSymbol  is: Text)
					ifTrue: [ stringTextOrSymbol asString ]
					ifFalse: [ stringTextOrSymbol ])]! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 6/20/2018 22:33:30'!
addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ PluggableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #toggleCollapseOrShow.
		button
			color: self color;
			icon: (aMorph imageForm: 32);
			setBalloonText: #label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

