'From Cuis7.3 [latest update: #7055] on 28 February 2025 at 10:36:35 am'!

!Text methodsFor: 'copying' stamp: 'jmv 2/28/2025 10:36:31'!
copyReplaceFrom: start to: stop with: aText
	| newRuns newString |
	newString := string copyReplaceFrom: start to: stop with: aText string.
	newRuns := runs copyReplaceFrom: start to: stop with: aText runs.
	^self class string: newString runs: newRuns.! !

