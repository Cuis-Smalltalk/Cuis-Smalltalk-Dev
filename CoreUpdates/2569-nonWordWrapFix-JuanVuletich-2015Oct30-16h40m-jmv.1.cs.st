'From Cuis 4.2 of 25 July 2013 [latest update: #2568] on 30 October 2015 at 4:47:00.620005 pm'!

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/30/2015 16:44'!
composeAll
	"Full recomposition computes actual right border of text,
	only limited by longest line and (extentForComposing x)"
	maxRightX _ self
		composeLinesFrom: 1
		to: model textSize
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: 0! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/30/2015 16:44'!
composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY 
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult composer |
	composer _ TextComposer new.
	editor ifNotNil: [
		composer editor: editor ].
	newResult _ composer
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				text: model actualContents
				extentForComposing: extentForComposing.
	lines _ newResult first asArray.
	^newResult second "right border of recomposed lines (bounded by extentForComposing x)"! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/30/2015 16:46'!
recomposeFrom: startArg to: stopArg delta: delta
	"Recompose this text.  The altered portion is between start and stop.
	Recomposition may continue to the end of the text, due to a ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed.
	Expand the requested interval to include whole paragraphs because there could be paragraph attributes."
	| intervalToFix start stop startLine newLines partialMaxRightX |
	"Have to recompose line above in case a word-break was affected."
	intervalToFix _ model actualContents encompassParagraph: (startArg to: stopArg).
	start _ intervalToFix first.
	stop _ intervalToFix last.
	startLine _ (self lineIndexFor: start) - 1 max: 1.
	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]
		whileTrue: [startLine _ startLine - 1].  "Find leftmost of line pieces"
	newLines _ OrderedCollection new: lines size + 1.
	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].
	partialMaxRightX _ self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top.
	"Partial recomposition computes actual right border only of recompsed text,
	so, it could make maxRightX larger but not smaller.
	This means that if the longest line gets shortened, we won't know, and maxRightX will be erroneously large"
	maxRightX _ maxRightX max: partialMaxRightX! !

