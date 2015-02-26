'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 29 January 2015 at 10:19:24.284069 am'!

!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 1/29/2015 10:09'!
disableKeyboard
	self textMorph disableKeyboard! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 1/29/2015 10:10'!
disableKeyboard
	self setProperty: #disableKeyboard toValue: true.
	self stopBlinking! !


!TextComposition methodsFor: 'access' stamp: 'jmv 1/29/2015 09:40'!
usedHeight
	^ lines last bottom - lines first top! !

!TextComposition methodsFor: 'access' stamp: 'jmv 1/29/2015 09:45'!
usedWidth
	^ maxRightX! !


!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 1/29/2015 09:58'!
model: aTextModel

	super model: aTextModel.
	self textMorph model: model wrappedTo: self viewableWidth.
	model refetch.
	self setScrollDeltas! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 1/29/2015 10:07'!
handlesKeyboard

	^self visible and: [ (self hasProperty: #disableKeyboard) not ]! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 1/29/2015 10:10'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self handlesKeyboard ifTrue: [
				self startBlinking ]]
		ifFalse: [ self stopBlinking ].
	"Selection might be shown differently when focused"
	owner
		ifNotNil: [ owner redrawNeeded ]
		ifNil: [ self redrawNeeded ]		"Or at least redraw us"! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/29/2015 09:46'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: AbstractFont default height + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 1/29/2015 10:13'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	textComposition ifNotNil: [
		self handlesKeyboard ifTrue: [
			"Show cursor right now if needed"
			textComposition showTextCursor ifFalse: [
				textComposition showTextCursor: true ]]]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 1/29/2015 09:41'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			0@0 + (0@(d+AbstractFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !


!TextComposition methodsFor: 'access' stamp: 'jmv 1/29/2015 09:40'!
usedExtent
	^ maxRightX @ self usedHeight! !

!methodRemoval: TextComposition #extent!
TextComposition removeSelector: #extent!
