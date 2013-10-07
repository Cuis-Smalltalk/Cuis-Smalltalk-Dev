'From Cuis 4.2 of 25 July 2013 [latest update: #1843] on 3 October 2013 at 10:01:21.590285 pm'!

!TextEditor methodsFor: 'typing support' stamp: 'jmv 10/3/2013 21:55'!
setEmphasisHereFromTextForward: f

	| i forward delta prevIsLineSeparator nextIsLineSeparator prevIsSeparator nextIsSeparator |
	i _ self pointIndex.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsLineSeparator _  i > 1 and: [ (model actualContents at: i-1) isLineSeparator ].
	nextIsLineSeparator _ i <= model textSize and: [ (model actualContents at: i) isLineSeparator ].
	prevIsSeparator _  i > 1 and: [ (model actualContents at: i-1) isSeparator ].
	nextIsSeparator _ i <= model textSize and: [ (model actualContents at: i) isSeparator ].
	prevIsLineSeparator & nextIsLineSeparator
		ifTrue: [
			"Empty paragraph: take emphasis from the newLine character"
			forward _ false ]
		ifFalse: [
			prevIsSeparator == nextIsSeparator
				ifTrue: [
					"Space at both sides, or non-space at both sides, take emphasis used where the cursor comes from"
					forward _ f ]
				ifFalse: [
					"Space at one side and non-space at the other, take emphasis from the non-space character"
					forward _ nextIsSeparator ]].
	delta _ forward ifTrue: [ 1 ] ifFalse: [ 0 ].
	emphasisHere _ (model actualContents attributesAt: (i - delta max: 1))
					select: [:att | att mayBeExtended].
	morph possiblyChanged! !


!Paragraph methodsFor: 'display' stamp: 'jmv 10/3/2013 22:00'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc
	"paragraphTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx caretFont t b caretAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show caret on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showCaret ifTrue: [
				leftX _ paragraphTopLeft x + startBlock left.
				idx _ startBlock stringIndex.
				caretAttributes _ editor ifNotNil: [ editor currentAttributes ].
				caretFont _ caretAttributes
					ifNil: [ model actualContents fontAt: idx ]
					ifNotNil: [ model actualContents fontIfApplying: caretAttributes ].
				b _ paragraphTopLeft y + line top + line baseline + caretFont descent-1.
				t _ paragraphTopLeft y + line top + line baseline - caretFont ascent.
				self
					displayInsertionMarkAtX: leftX
					top: t
					bottom: b
					emphasis: caretFont emphasis
					on: aCanvas
					paragraphLeft: paragraphTopLeft x ]]
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
			lastCaretRect _ nil.
			leftX _  paragraphTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  paragraphTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  paragraphTopLeft y) corner: rightX @ (line bottom +  paragraphTopLeft y))
				color: sc ].	"Selection begins on line below"! !

