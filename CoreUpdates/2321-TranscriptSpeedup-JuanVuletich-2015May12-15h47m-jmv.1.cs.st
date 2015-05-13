'From Cuis 4.2 of 25 July 2013 [latest update: #2320] on 12 May 2015 at 4:05:19.507899 pm'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay innerRectangle lastDisplayTime lastDisplayPosition '!

!AbstractFont methodsFor: 'as yet unclassified' stamp: 'jmv 5/12/2015 14:49'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	| char fallbackFont x y answer |
	fallbackFont _ (AbstractFont familyName: 'DejaVu' aroundPointSize: self pointSize*0.9) emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!StrikeFont methodsFor: 'as yet unclassified' stamp: 'jmv 5/12/2015 14:49'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	^aGrafPort
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!GrafPort methodsFor: 'text' stamp: 'jmv 5/12/2015 14:52'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: font baseKern.
	^ destX@(destY+font height)! !


!String methodsFor: 'displaying' stamp: 'jmv 5/12/2015 15:52'!
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: Color black
	"
	'Display' displayOn: Display at: 10@10
	"! !

!String methodsFor: 'displaying' stamp: 'jmv 5/12/2015 14:51'!
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	^aDisplayMedium getCanvas drawString: self at: aPoint font: nil color: aColor! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/12/2015 14:50'!
drawString: s at: pt font: fontOrNil color: aColor

	^self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/12/2015 14:50'!
drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/12/2015 14:50'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + (0@2)
			font: fontOrNil
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + (0@1)
		font: fontOrNil
		color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/12/2015 15:49'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ (currentTransformation transform: aPoint) rounded.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font on: port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/12/2015 14:43'!
clear
	| stream |
	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		
		logToFile ifTrue: [
			stream _ FileStream forceNewFileNamed: self filename.
			[
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine
			] ensure: [ stream close ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/12/2015 14:43'!
clearInternal

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		lastDisplayPosition _ 0 ].
	self display! !

!Transcript class methodsFor: 'private' stamp: 'jmv 5/12/2015 14:44'!
finishEntry
	| newEntry |
	newEntry _ unfinishedEntry contents.
	unfinishedEntry reset.
	lastDisplayPosition _ 0.
	self addEntry: newEntry.
	self display! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 5/12/2015 15:59'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display ].
	self changed: #redraw	"So any morph in front of us is repaired when Morphic cycles"! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 5/12/2015 15:58'!
displayUnfinishedEntry
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOn: Display ]! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 5/12/2015 16:02'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	lastDisplayPosition < innerRectangle right ifTrue: [
		font _ AbstractFont default.
		fh _ font height.
		count _ innerRectangle height // fh-1.
		x _ innerRectangle left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
		canvas _ aForm getCanvas.
		canvas newClipRect: (innerRectangle left: lastDisplayPosition).
		lastDisplayPosition _ (canvas drawString: string at: x@y font: font color: Color veryDarkGray) x ]! !

!Transcript class methodsFor: 'class initialization' stamp: 'jmv 5/12/2015 14:43'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	innerRectangle _ 20@20 extent: 300@500.
	logToFile _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String new writeStream.
	lastDisplayPosition _ 0.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay innerRectangle lastDisplayPosition'!

Transcript clear!
