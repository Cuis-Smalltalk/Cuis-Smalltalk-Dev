'From Cuis7.5 [latest update: #7534] on 9 September 2025 at 12:01:26 pm'!

!Transcripter methodsFor: 'accessing' stamp: 'jmv 9/9/2025 12:00:53'!
clear
	DisplayScreen deferredUpdatingDisplay fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	self on: (String new: 100); endEntry.! !

!Transcripter methodsFor: 'accessing' stamp: 'jmv 9/9/2025 11:59:52'!
endEntry
	| c d cb |
	c := self contents.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame := frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d := textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb := textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit := position := collection size.
		^ self endEntry].
	DisplayScreen deferredUpdatingDisplay fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	DisplayScreen deferredUpdatingDisplay getCanvas
		drawTextComposition: textComposition
		at: `4@4` + frame topLeft
		extent: Display extent
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen forceDeferredToScreen: frame.! !

