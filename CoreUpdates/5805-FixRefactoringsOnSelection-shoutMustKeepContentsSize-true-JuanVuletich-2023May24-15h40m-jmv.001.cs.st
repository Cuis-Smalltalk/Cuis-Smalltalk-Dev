'From Cuis 6.0 [latest update: #5803] on 24 May 2023 at 3:52:43 pm'!

!TextProvider methodsFor: 'user interface support' stamp: 'jmv 5/24/2023 15:51:19'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents.
	True for refactorings based in selected text to work correctly in Browsers."

	^true! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 5/24/2023 15:51:24'!
shoutMustKeepContentsSize
	"Answer true if we require the styler not to modify the size of our contents.
	True in Debugger for the auto selection of context to work correctly"

	^true! !

