'From Cuis7.5 [latest update: #7659] on 15 October 2025 at 4:04:32 pm'!

!ScrollBar methodsFor: 'options' stamp: 'jmv 10/15/2025 16:02:35'!
sliderSlackFactor

	^ self valueOfProperty: #sliderSlackFactor ifAbsent: [ `1.0 / 6.0` ]! !

!ScrollBar methodsFor: 'options' stamp: 'jmv 10/15/2025 16:03:04'!
sliderSlackFactor: aNumber

	self setProperty: #sliderSlackFactor toValue: aNumber.
	self hideOrShowButtons.! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/15/2025 15:51:33'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Must layout submorphs again."

	self updateScrollBarsBounds
	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt := false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller extentInOwner ifFalse: [
				theScrollerExtent := scroller extentInOwner.
				doIt := true ]].
		scrollerLocation := scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale := scrollerLocation scale.
			doIt := true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians := scrollerLocation radians.
			doIt := true ].
		doIt ifTrue: [ self setScrollDeltas ]].! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 10/15/2025 16:03:13'!
sliderSlack
	^self thickness * self sliderSlackFactor! !

