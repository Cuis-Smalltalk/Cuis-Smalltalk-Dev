'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 23 July 2017 at 4:39:28 pm'!

!Form methodsFor: 'scaling, rotation' stamp: 'pb 7/23/2017 15:54:09'!
scalePreservingAspectRatioTo: extent
	"Answer a Form created as a scaling of the receiver."
	extent x * extent y = 0
		ifTrue: [ ^ self ]
		ifFalse: [ | factor magnifiedExtent |
			factor _ 1.0 * extent x / self width min: 1.0 * extent y / self height.
					magnifiedExtent _ self extent * factor.
					^ self magnifyTo: magnifiedExtent ].! !


!PluggableButtonMorph methodsFor: 'private' stamp: 'pb 7/23/2017 16:38:09'!
magnifiedIcon
	icon ifNil: [ ^ nil ].
	magnifiedIcon ifNil: [ | w h factor |
		magnifiedIcon := icon.
		w _ icon width.
		h _ icon height.
		w * h = 0 ifFalse: [
			factor _ 1.0 * self morphExtent x / w min: 1.0 * self morphExtent y / h.
			(factor < 1 or: [
				factor > 1.7 and: [ self isRoundButton ]]) ifTrue: [ magnifiedIcon _ icon scalePreservingAspectRatioTo: self morphExtent ]]].
	^ magnifiedIcon.! !

!PluggableButtonMorph methodsFor: 'as yet unclassified' stamp: 'pb 7/23/2017 15:08:40'!
morphExtent
	"Use extent if it has already been manually set, otherwise try to set it by computing from the label text and font, otherwise try using the icon extent, or finally fall back to the default value."
	^ extent ifNil: [
		extent _ (self fontToUse notNil and: [ label notNil ])
			ifTrue: [ "Add a bit of padding"
				(self fontToUse widthOfString: label) + 10 @ (self fontToUse height + 10) ]
			ifFalse: [
				icon
					ifNil: [ `20@15` ]
					ifNotNil: [ icon extent ]]].! !


!TaskbarMorph methodsFor: 'services' stamp: 'pb 7/23/2017 15:54:49'!
addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ PluggableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #toggleCollapseOrShow.
		button
			color: self color;
			icon: ((aMorph imageForm: 32) scalePreservingAspectRatioTo: `128@128`);
			setBalloonText: aMorph label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

