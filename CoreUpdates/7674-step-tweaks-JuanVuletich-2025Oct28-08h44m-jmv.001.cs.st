'From Cuis7.5 [latest update: #7673] on 28 October 2025 at 8:47:34 am'!

!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 10/28/2025 08:40:14'!
step

	super step.
	self updateContents.! !


!UpdatingLabelMorph methodsFor: 'stepping' stamp: 'jmv 10/28/2025 08:40:31'!
step

	self contents: (target perform: getSelector) asString.! !


!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 10/28/2025 08:41:36'!
step

	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !


!SmalltalkCompleterMorph methodsFor: 'stepping' stamp: 'jmv 10/28/2025 08:41:45'!
step

	self isShowingSelectorDocumentation ifTrue: [ ^self ].
	super step.! !

!methodRemoval: SmalltalkCompleterMorph #stepAt: stamp: 'jmv 10/28/2025 08:44:51'!
SmalltalkCompleterMorph removeSelector: #stepAt:!
!methodRemoval: AutoCompleterMorph #stepAt: stamp: 'jmv 10/28/2025 08:44:51'!
AutoCompleterMorph removeSelector: #stepAt:!
!methodRemoval: UpdatingLabelMorph #stepAt: stamp: 'jmv 10/28/2025 08:44:51'!
UpdatingLabelMorph removeSelector: #stepAt:!
!methodRemoval: UpdatingMenuItemMorph #stepAt: stamp: 'jmv 10/28/2025 08:44:51'!
UpdatingMenuItemMorph removeSelector: #stepAt:!
