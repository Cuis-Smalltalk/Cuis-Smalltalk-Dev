'From Cuis 6.0 [latest update: #5306] on 23 June 2022 at 4:08:03 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/23/2022 16:07:18'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection.
	This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart size inStream outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	size _  stop + 1 - start.
	inStream _ self privateCurrentString readStreamFrom: start to: stop.
	outStream _ self stringClassToUse writeStream.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: outStream contents.
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !

