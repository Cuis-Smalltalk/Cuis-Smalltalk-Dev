'From Cuis 5.0 [latest update: #4680] on 19 July 2021 at 5:08:58 pm'!

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/19/2021 11:33:16'!
pvtDrawButtonFaceForm: aForm
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	| color |
	color _ currentMorph isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			currentMorph mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].
	self
		image: aForm
		multipliedBy: color
		at: (currentMorph morphExtent - aForm extent //2).! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/19/2021 17:06:53'!
icon: aForm
	icon _ aForm.
	magnifiedIcon _ nil! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 7/19/2021 17:06:35'!
magnifiedIcon
	| factor magnifiedExtent w h |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		w _ icon width.
		h _ icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * extent x / w min: 1.0 * extent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (icon extent * factor) rounded.
				magnifiedIcon _ icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 11/15/2020 11:34:23'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	magnifiedIcon _ nil! !


!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/19/2021 17:07:46'!
drawButtonIcon: aPluggableButtonMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	aPluggableButtonMorph iconDrawSelector ifNotNil: [ :selector |
		self perform: selector.
		^true ].

	aPluggableButtonMorph magnifiedIcon ifNotNil: [ :theIcon |
		self pvtDrawButtonFaceForm: theIcon.
		^true ].

	^false! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:25'!
drawCloseIcon
	| icon |
	icon _ self class windowButtonIcon: #closeIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:29'!
drawCollapseIcon
	| icon |
	icon _ self class windowButtonIcon: #collapseIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:32'!
drawDownIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:36'!
drawExpandIcon
	| icon |
	icon _ self class windowButtonIcon: #expandIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:40'!
drawLeftIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:43'!
drawMenuIcon
	| icon |
	icon _ self class windowButtonIcon: #windowMenuIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:47'!
drawPushPinIcon
	| icon |
	icon _ self class windowButtonIcon: #pushPinIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:50'!
drawRightIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/19/2021 11:33:53'!
drawUpIcon
	| icon |
	icon _ BitBltCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.! !

!methodRemoval: BitBltCanvas #drawButtonIcon: stamp: 'jmv 7/19/2021 17:05:19'!
BitBltCanvas removeSelector: #drawButtonIcon:!
!methodRemoval: BitBltCanvas #drawButtonForm: stamp: 'jmv 7/19/2021 17:05:48'!
BitBltCanvas removeSelector: #drawButtonForm:!
!methodRemoval: BitBltCanvas #pvtDrawButtonIcon: stamp: 'jmv 7/19/2021 16:59:29'!
BitBltCanvas removeSelector: #pvtDrawButtonIcon:!
!methodRemoval: MorphicCanvas #drawButtonForm: stamp: 'jmv 7/19/2021 17:05:49'!
MorphicCanvas removeSelector: #drawButtonForm:!
!methodRemoval: PluggableButtonMorph #morphContainsPoint: stamp: 'jmv 7/19/2021 17:03:06'!
PluggableButtonMorph removeSelector: #morphContainsPoint:!
!methodRemoval: PluggableButtonMorph #scaledForm stamp: 'jmv 7/19/2021 17:08:04'!
PluggableButtonMorph removeSelector: #scaledForm!
!methodRemoval: PluggableButtonMorph #form: stamp: 'jmv 7/19/2021 17:08:00'!
PluggableButtonMorph removeSelector: #form:!
