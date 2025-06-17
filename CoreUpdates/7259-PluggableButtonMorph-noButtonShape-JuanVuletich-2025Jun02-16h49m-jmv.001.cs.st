'From Cuis7.5 [latest update: #7258] on 2 June 2025 at 4:51:09 pm'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 6/2/2025 16:50:18'!
noButtonShape
	"Draw only the icon and or label, with no button shape"
	roundButtonStyle := #none.
	self redrawNeeded.! !

!methodRemoval: PluggableButtonMorph #iconOnly: stamp: 'jmv 6/2/2025 16:50:38'!
PluggableButtonMorph removeSelector: #iconOnly:!
