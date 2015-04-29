'From Cuis 4.2 of 25 July 2013 [latest update: #2278] on 28 April 2015 at 10:54:30.328009 am'!

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 4/27/2015 15:38'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd		:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (100.0 * fullGCTime / upTime) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (1.0 * fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (100.0 * incrGCTime / upTime) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (1.0 * incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (1.0 * incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows printStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !


!Time methodsFor: 'printing' stamp: 'jmv 4/27/2015 15:31'!
print24: hr24 showSeconds: showSeconds on: aStream 
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h := self hour. m := self minute. s := self second.
	hr24
		ifTrue: 
			[ h < 10 ifTrue: [ aStream nextPutAll: '0' ].
			h printOn: aStream ]
		ifFalse:
			[ h > 12
				ifTrue: [h - 12 printOn: aStream]
				ifFalse: 
					[h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ]]].

	aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
	m printOn: aStream.

	showSeconds ifTrue:
		[ aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
		self nanoSecond = 0
			ifTrue: [s asInteger printOn: aStream]
			ifFalse: [(s + (1.0 * self nanoSecond / Time nanosInSecond)) printOn: aStream]].

	hr24 ifFalse:
		[ aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ].
! !


!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 4/27/2015 15:31'!
magnifiedIcon
	| factor magnifiedExtent |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		factor _ 1.0 * extent x / icon width min: 1.0 * extent y / icon height.
		(factor < 1 or: [ factor > 1.7 and: [self isRoundButton]]) ifTrue: [
			magnifiedExtent _ (icon extent * factor) rounded.
			magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
	^magnifiedIcon! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/28/2015 10:54'!
hSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta w |
	
	delta _ self scrollDeltaWidth * 1.0.		"avoid Fraction arithmetic"
	range _ self hLeftoverScrollRange.
	range = 0 ifTrue: [
		^hScrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; setValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	w _ self viewableWidth * 1.0.		"avoid Fraction arithmetic"
	hScrollBar scrollDelta: delta / range pageDelta: w - delta / range.
	hScrollBar interval: w / self hTotalScrollRange.
	hScrollBar setValue: hScrollBar value! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/27/2015 15:29'!
vSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta h |
	
	delta _ self scrollDeltaHeight * 1.0.	"avoid Fraction arithmetic"
	range _ self vLeftoverScrollRange.
	range = 0 ifTrue: [
		^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; setValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	h _ self viewableHeight * 1.0. 		"avoid Fraction arithmetic"
	scrollBar scrollDelta: delta / range pageDelta: h - delta / range.
	scrollBar interval: h / self vTotalScrollRange.
	scrollBar setValue: scrollBar value! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 4/27/2015 15:32'!
drawLabelOn: aCanvas

	| e x0 y0 f w availableW l |
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2.
	f _ Preferences windowTitleFont.
	w _ f widthOfString: labelString.
	availableW _ extent x - x0 - 10.
	availableW >= w
		ifTrue: [
			l _ labelString ]
		ifFalse: [
			l _ labelString squeezedTo: (1.0 * labelString size * availableW / w) rounded ].
	Theme current embossedTitles
		ifFalse: [
			aCanvas
				drawString: l
				at: x0@y0
				font: f
				color: Theme current windowLabel ]
		ifTrue: [
			aCanvas
				drawStringEmbossed: l
				at: x0@y0
				font: f
				color: Theme current windowLabel ]! !


!FillInTheBlankMorph class methodsFor: 'default constants' stamp: 'jmv 4/27/2015 17:33'!
defaultAnswerExtent
	^  (200@60 * (Preferences standardMenuFont height / 12.0)) rounded! !

