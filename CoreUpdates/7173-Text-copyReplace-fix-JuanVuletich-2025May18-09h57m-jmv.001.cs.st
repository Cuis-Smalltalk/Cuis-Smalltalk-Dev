'From Cuis7.3 [latest update: #7170] on 18 May 2025 at 9:59:22 am'!

!Text methodsFor: 'copying' stamp: 'jmv 5/18/2025 09:58:49'!
copyReplaceFrom: start to: stop with: replacement
	| rep newRuns newString answer |
	rep := replacement asText.	"might be a string"
	newString := string copyReplaceFrom: start to: stop with: rep string.
	newRuns := runs copyReplaceFrom: start to: stop with: rep runs.
	answer := self class string: newString runs: newRuns.
	answer fixParagraphAttributesFrom: start to: start + replacement size - 1.
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	^ answer! !

