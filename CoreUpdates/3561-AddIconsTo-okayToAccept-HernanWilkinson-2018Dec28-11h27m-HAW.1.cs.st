'From Cuis 5.0 of 7 November 2016 [latest update: #3560] on 28 December 2018 at 11:29:52 am'!

!CodeProvider methodsFor: 'misc' stamp: 'HAW 12/28/2018 09:33:38'!
okayToAccept
	"Answer whether it is okay to accept the receiver's input"

	self showingByteCodes ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingDocumentation ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingAnyKindOfDiffs ifTrue: [
		^ SelectionMenu confirm: 
'Caution!!  You are "showing diffs" here, so 
there is a danger that some of the text in the
code pane is contaminated by the "diff" display'
		trueChoice: 'accept anyway -- I''ll take my chances' falseChoice: 'um, let me reconsider' icons: #(acceptIcon cancelIcon)
	].

	^ true! !

