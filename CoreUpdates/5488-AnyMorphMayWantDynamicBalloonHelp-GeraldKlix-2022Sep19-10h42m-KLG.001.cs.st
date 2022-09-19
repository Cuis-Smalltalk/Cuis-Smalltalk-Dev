'From Cuis 6.0 [latest update: #5487] on 19 September 2022 at 10:43:23 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'KLG 9/19/2022 10:42:35'!
balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer.
	Allow for dynamic balloon text."

	^ (self valueOfProperty: #balloonText ifAbsent: [ nil ]) value.! !

!methodRemoval: UpdatingLabelMorph #balloonText stamp: 'KLG 9/19/2022 10:42:50'!
UpdatingLabelMorph removeSelector: #balloonText!
