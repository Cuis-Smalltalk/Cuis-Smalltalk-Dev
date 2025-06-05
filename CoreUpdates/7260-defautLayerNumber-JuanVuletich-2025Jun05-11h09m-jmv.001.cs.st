'From Cuis7.5 [latest update: #7259] on 5 June 2025 at 11:11:13 am'!

!Morph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:46:20'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	Please make them natural numbers.
	See implementors."

	^100! !


!HaloMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:46:26'!
defaultLayerNumber
	^ 20! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:46:45'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be on top."

	^10! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 11:00:48'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^10! !


!StringRequestMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:46:48'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be above regular morphs."

	^30! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:58:40'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20! !


!AutoCompleterMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:47:03'!
defaultLayerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20! !


!Morph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:48:36'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	See #defaultLayerNumber."

	^self
		valueOfProperty: #layerNumber
		ifAbsent: [self defaultLayerNumber]! !

!Morph methodsFor: 'private' stamp: 'jmv 6/5/2025 10:42:19'!
privateFixLayerOrder
	"Ensure that morphs with smaller #layerNumber are above morphs with larger #layerNumber.
	Keep current sorting inside each set with same #layerNumber."

	| layerNumbers sortedSubmorphs newIndex |
	self honorsLayerOrder ifFalse: [ ^self ].
	submorphs size < 2 ifTrue: [ ^self ].
	layerNumbers := Set new.
	submorphs do: [ :m | layerNumbers add: m layerNumber ].
	layerNumbers := layerNumbers asArray sort.
	sortedSubmorphs := Array new: submorphs size.
	newIndex := 1.
	layerNumbers do: [ :layerNumber |
		submorphs withIndexDo: [ :m :i |
			m layerNumber = layerNumber ifTrue: [
				sortedSubmorphs at: newIndex put: m.
				newIndex = i ifFalse: [ m invalidateBounds ].
				newIndex := newIndex + 1 ]]].
	submorphs := sortedSubmorphs.! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 6/5/2025 11:00:51'!
initialize
	super initialize.
	self separation: 0.
	labelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	subLabelMorph := LabelMorph contents: '' font: FontFamily defaultFamilyAndPointSize.
	progress := ProgressBarMorph new.
	progress morphExtent: 200 @ FontFamily defaultLineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: FontFamily defaultLineSpacing.! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 6/5/2025 10:59:39'!
stayUp: aBoolean

	stayUp := aBoolean.
	aBoolean ifTrue: [ self removeStayUpBox ].
	self taskbar ifNotNil: [ :tb | tb refreshTaskbarFor: self ].! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 6/5/2025 10:35:38'!
addWindowControlTo: aMenu

	aMenu
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 		icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
										action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 					icon: #collapseIcon;
		add: 'expand / contract' 			action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize.
		
	^aMenu! !

!methodRemoval: AutoCompleterMorph #layerNumber stamp: 'jmv 6/5/2025 11:10:22'!
AutoCompleterMorph removeSelector: #layerNumber!
!methodRemoval: MenuMorph #layerNumber stamp: 'jmv 6/5/2025 11:10:22'!
MenuMorph removeSelector: #layerNumber!
!methodRemoval: StringRequestMorph #layerNumber stamp: 'jmv 6/5/2025 11:10:22'!
StringRequestMorph removeSelector: #layerNumber!
!methodRemoval: HoverHelpMorph #layerNumber stamp: 'jmv 6/5/2025 11:10:22'!
HoverHelpMorph removeSelector: #layerNumber!
!methodRemoval: HaloMorph #layerNumber stamp: 'jmv 6/5/2025 11:10:22'!
HaloMorph removeSelector: #layerNumber!
