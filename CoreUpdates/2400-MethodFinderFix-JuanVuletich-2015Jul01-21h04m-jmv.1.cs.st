'From Cuis 4.2 of 25 July 2013 [latest update: #2399] on 1 July 2015 at 9:06:29.99458 pm'!

!MessageNames methodsFor: 'user interface support' stamp: 'jmv 7/1/2015 21:05'!
editorClassFor: textGetter
	textGetter = #searchString ifTrue: [
		^SmalltalkEditor ].
	^super editorClassFor: textGetter! !


!MessageNames reorganize!
('search' computeSelectorListFromSearchString searchString searchString:)
('selector list' messageList selectedMessageName selectorList selectorListIndex selectorListIndex:)
('initialization' contentsSelection initialize labelString)
('user interface support' editorClassFor:)
!

