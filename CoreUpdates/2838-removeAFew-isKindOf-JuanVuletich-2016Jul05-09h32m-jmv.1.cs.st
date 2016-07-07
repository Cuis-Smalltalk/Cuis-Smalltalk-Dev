'From Cuis 4.2 of 25 July 2013 [latest update: #2837] on 5 July 2016 at 9:37:51.848832 am'!

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:29'!
lineDiffButtonHelp
	^'Show code differences between the file-based method and the in-memory version, line by line.'! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/4/2016 22:08'!
prettyLineDiffButtonHelp
	^'Show pretty-printed code differences between the file-based method and the in-memory version, line by line.'! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/4/2016 22:09'!
prettyWordDiffButtonHelp
	^'Show pretty-printed code differences between the file-based method and the in-memory version, word by word.'! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:29'!
wordDiffButtonHelp
	^'Show code differences between the file-based method and the in-memory version, word by word.'! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:28'!
lineDiffButtonHelp
	^'Show code differences from the previous version, line by line.'! !

!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/4/2016 22:08'!
prettyLineDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, line by line.'! !

!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/4/2016 22:09'!
prettyWordDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, word by word.'! !

!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:29'!
wordDiffButtonHelp
	^'Show code differences from the previous version, word by word.'! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:30'!
lineDiffButton
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingLineDiffs
		action: #toggleLineDiffing)
			label: 'lineDiffs';
			setBalloonText: self lineDiffButtonHelp! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:36'!
prettyLineDiffButton
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyLineDiffs
		action: #togglePrettyLineDiffing)
			label: 'linePrettyDiffs';
			setBalloonText: self prettyLineDiffButtonHelp! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:37'!
prettyWordDiffButton
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyWordDiffs
		action: #togglePrettyWordDiffing)
			label: 'wordPrettyDiffs';
			setBalloonText: self prettyWordDiffButtonHelp! !

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 7/5/2016 09:30'!
wordDiffButton
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingWordDiffs
		action: #toggleWordDiffing)
			label: 'wordDiffs';
			setBalloonText: self wordDiffButtonHelp! !

