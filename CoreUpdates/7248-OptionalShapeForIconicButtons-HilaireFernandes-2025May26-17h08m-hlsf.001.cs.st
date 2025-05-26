'From Cuis7.3 [latest update: #7247] on 26 May 2025 at 5:10:28 pm'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 5/24/2025 11:20:37'!
borderWidth: anInteger
	super borderWidth: anInteger.
	magnifiedIcon := nil.
	self redrawNeeded ! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 5/26/2025 17:01:00'!
iconOnly: aForm
	"Draw only the icon, with no button shape or mouse over indication"
	icon := aForm.
	magnifiedIcon := nil.
	roundButtonStyle := #none.
	self redrawNeeded.! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 5/24/2025 11:20:22'!
padding: numberOrPoint
	super padding: numberOrPoint .
	magnifiedIcon := nil.
	self redrawNeeded ! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/26/2025 17:06:41'!
drawOn: aCanvas

	roundButtonStyle == #none ifFalse: [
		(roundButtonStyle ifNil: [Theme current roundButtons])
			ifTrue: [ self drawRoundLookOn: aCanvas ]
			ifFalse: [ self draw3DLookOn: aCanvas ]].

	aCanvas drawButtonIconFromCurrentMorph.! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'hlsf 5/24/2025 11:18:10'!
magnifiedIcon
	| factor magnifiedExtent w h interiorExtent |

	magnifiedIcon ifNotNil: [ ^magnifiedIcon ].
	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		interiorExtent := self interiorExtent.
		magnifiedIcon := icon.
		w := icon width.
		h := icon height.
		w*h = 0 ifFalse: [
			factor := 1.0 * interiorExtent x / w min: 1.0 * interiorExtent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent := (icon extent * factor) rounded.
				magnifiedIcon := icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

!methodRemoval: PluggableButtonMorph #isRoundButton stamp: 'hlsf 5/26/2025 17:09:38'!
PluggableButtonMorph removeSelector: #isRoundButton!
