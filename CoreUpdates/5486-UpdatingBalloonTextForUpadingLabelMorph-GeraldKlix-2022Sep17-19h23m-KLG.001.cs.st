'From Cuis 6.0 [latest update: #5487] on 19 September 2022 at 10:43:21 am'!

!UpdatingLabelMorph methodsFor: 'halos and balloon help' stamp: 'KLG 9/19/2022 10:23:12'!
balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	"Updating label morphs should have dynamic balloon text."

	^ super balloonText value! !


!UpdatingLabelMorph class methodsFor: 'new-morph participation' stamp: 'KLG 9/19/2022 10:24:40'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (Preferences at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value;
		setBalloonText: [ Date today asString ].

	^ newInst! !

worldself runningWorld ifNotNil: [ :world | 
	world
		hideTaskbar;
		showTaskbar ]!

