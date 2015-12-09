'From Cuis 4.2 of 25 July 2013 [latest update: #2621] on 4 December 2015 at 4:54:36.262323 pm'!

!TextComposition methodsFor: 'access' stamp: 'jmv 12/4/2015 15:35'!
lastTextCursorRect
	"Warning: Could be not updated, it is the one for when the text cursor was last drawn."
	^lastTextCursorRect! !


!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 12/4/2015 15:35'!
onBlinkCursor
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textComposition lastTextCursorRect ifNotNil: [ :r | self invalidateLocalRect: r].! !


!TextComposition methodsFor: 'display' stamp: 'jmv 12/4/2015 15:36'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc
	"textTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx textCursorFont t b textCursorAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show text cursor on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showTextCursor ifTrue: [
				leftX _ textTopLeft x + startBlock left.
				idx _ startBlock stringIndex.
				textCursorAttributes _ editor ifNotNil: [ editor currentAttributes ].
				textCursorFont _ textCursorAttributes
					ifNil: [ model actualContents fontAt: idx ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes ].
				b _ textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t _ textTopLeft y + line top + line baseline - textCursorFont ascent.
				lastTextCursorRect _ nil.
				self
					displayTextCursorAtX: leftX
					top: t
					bottom: b
					emphasis: textCursorFont emphasis
					on: aCanvas
					textLeft: textTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastTextCursorRect _ nil.
			leftX _  textTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  textTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  textTopLeft y) corner: rightX @ (line bottom +  textTopLeft y))
				color: sc ].	"Selection begins on line below"! !

!methodRemoval: TextComposition #getAndCleanLastTextCursorRect!
TextComposition removeSelector: #getAndCleanLastTextCursorRect!
