'From Cuis 5.0 [latest update: #4982] on 27 November 2021 at 9:22:52 am'!

!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 11/27/2021 09:20:49'!
extentForComposing

	^ extent x - (4 * borderWidth) @ 9999999! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 11/27/2021 09:20:26'!
extentForComposing

	^wrapFlag
		ifTrue: [ extent x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !


!TextParagraphMorph methodsFor: 'initialization' stamp: 'jmv 11/27/2021 09:22:04'!
initialize
	super initialize.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: self extentForComposing;
		composeAll.! !

!TextParagraphMorph methodsFor: 'drawing' stamp: 'hlsf 11/26/2021 10:59:52'!
drawOn: aCanvas
	super drawOn: aCanvas.
	aCanvas
		textComposition: textComposition
		bounds: (self morphLocalBounds insetBy: borderWidth * 2)
		color: Theme current text
		selectionColor: `Color red`.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 11/27/2021 09:21:10'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
	textComposition
		initialize;
		extentForComposing: self extentForComposing;
		composeAll.
	self fit.
	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ]].! !

