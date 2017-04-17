'From Cuis 5.0 of 7 November 2016 [latest update: #3068] on 16 April 2017 at 7:53:58 pm'!

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 4/16/2017 19:53:14'!
compareToClipboard
	"Check to see if whether the receiver's text is the same as the text currently on the clipboard, and inform the user."
	| s1 s2 |
	s1 _ self clipboardStringOrText string.
	s2 _ self selection ifEmpty: [self privateCurrentString].
	s1 = s2 ifTrue: [^ self inform: 'Exact match'].

	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: s1 to: s2 tryWords: true))
			openLabel: 'Comparison to Clipboard Text'! !

