'From Cuis 5.0 [latest update: #3856] on 5 September 2019 at 11:18:24 am'!

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 10:58:07'!
createCloseButton
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		icon: Theme current closeIcon;
		iconName: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 10:58:11'!
createCollapseButton
	^(PluggableButtonMorph model: self action: #collapse)
		icon: Theme current collapseIcon;
		iconName: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 10:58:40'!
createExpandButton
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		icon: Theme current expandIcon;
		iconName: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 10:58:54'!
createMenuButton
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		icon: Theme current windowMenuIcon;
		iconName: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: self titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 08:17:04'!
titleBarButtonsExtent
	"answer the extent to use for close & other title bar buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^`0@0`].
	e _ Preferences windowTitleFont pointSize.
	^e@e! !


!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 9/5/2019 08:24:08'!
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


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/5/2019 08:20:12'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ Preferences windowTitleFont.
	x0 _  f lineSpacing * 4 + 14.
	y0 _ 2+3.
	y0 _ f lineSpacing - f ascent // 2.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/5/2019 08:36:52'!
rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonPos buttonExtent  buttonDelta|
	buttonExtent := self titleBarButtonsExtent.
	buttonPos _ self labelHeight + borderWidth - buttonExtent // 2 * (1@1).
	buttonDelta _ buttonExtent x *14//10.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos _ buttonPos + (buttonDelta@0).
		].
	]! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/5/2019 08:31:34'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft _ self labelHeight + borderWidth - self titleBarButtonsExtent // 2 * (1@1).
	spacing _ self titleBarButtonsExtent x *14//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft! !

!SystemWindow methodsFor: 'label' stamp: 'jmv 9/5/2019 08:20:31'!
labelHeight
	"Answer the height for the window label."

	^ Preferences windowTitleFont lineSpacing+1! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/5/2019 11:14:23'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Preferences windowTitleFont pointSize.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton icon: Theme current closeIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton icon: Theme current pushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: pinButton fixedWidth: buttonHW).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!methodRemoval: SystemWindow #boxExtent!
SystemWindow removeSelector: #boxExtent!
!methodRemoval: SystemWindow #createCloseBox!
SystemWindow removeSelector: #createCloseBox!
!methodRemoval: SystemWindow #createCollapseBox!
SystemWindow removeSelector: #createCollapseBox!
!methodRemoval: SystemWindow #createExpandBox!
SystemWindow removeSelector: #createExpandBox!
!methodRemoval: SystemWindow #createMenuBox!
SystemWindow removeSelector: #createMenuBox!
