'From Cuis 4.2 of 25 July 2013 [latest update: #2061] on 30 July 2014 at 7:42:20.926113 pm'!
!classDefinition: #PluggableButtonMorph category: #'Morphic-Views for Models'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon getStateSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!FormCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/30/2014 19:41'!
drawButtonIconFromCurrentMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	currentMorph magnifiedIcon ifNotNil: [ :theIcon |
		self
			image: theIcon
			multipliedBy: currentMorph iconColor
			at: (currentMorph morphExtent - theIcon extent //2).
		^true ].
	^false! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/8/2014 14:33'!
iconName: aSymbol
	iconName _ aSymbol! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 7/8/2014 14:48'!
iconName
	^iconName! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/30/2014 19:35'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [
			aCanvas drawButtonIconFromCurrentMorph ifFalse: [
				self drawRoundGradientLookOn: aCanvas ]]
		ifFalse: [
			self draw3DLookOn: aCanvas.
			aCanvas drawButtonIconFromCurrentMorph ]! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 7/30/2014 19:41'!
magnifiedIcon
	| factor magnifiedExtent |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		self isRoundButton
			ifFalse: [ ^ magnifiedIcon ].

		factor _ extent x / icon width min: extent y / icon height.
		(factor < 1 or: [ factor > 1.7 ]) ifTrue: [
			magnifiedExtent _ (icon extent * factor) rounded.
			magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
	^magnifiedIcon! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 7/8/2014 14:52'!
updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	self iconName: #drawDownIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 7/8/2014 14:52'!
updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	self iconName: #drawLeftIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 7/8/2014 14:52'!
updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ FormCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	self iconName: #drawRightIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded! !

!PluggableButtonMorph methodsFor: 'scrollbar button' stamp: 'jmv 7/8/2014 14:53'!
updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ FormCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	self iconName: #drawUpIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/8/2014 14:53'!
createCloseBox
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		icon: Theme current closeIcon;
		iconName: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/8/2014 14:53'!
createCollapseBox
	^(PluggableButtonMorph model: self action: #collapse)
		icon: Theme current collapseIcon;
		iconName: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/8/2014 14:53'!
createExpandBox
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		icon: Theme current expandIcon;
		iconName: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: self boxExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/8/2014 14:53'!
createMenuBox
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		icon: Theme current windowMenuIcon;
		iconName: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: self boxExtent! !

!methodRemoval: PluggableButtonMorph #drawInconOn:!
PluggableButtonMorph removeSelector: #drawInconOn:!
!classDefinition: #PluggableButtonMorph category: #'Morphic-Views for Models'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon getStateSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
