'From Cuis 4.0 of 21 April 2012 [latest update: #1262] on 25 April 2012 at 10:52:28 am'!

!TextProvider methodsFor: 'testing' stamp: 'jmv 4/25/2012 10:50'!
refusesToAccept
	"This method must answer false if we usually accept text from a PluggableTextModel (i.e. we understand its textSetter), but are not accepting text right now."

	^ false! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 4/25/2012 10:52'!
wantsFrameAdornments

	^textSetter notNil! !


!TextProvider reorganize!
('contents' acceptedContents acceptedContentsChanged acceptedStringOrText)
('accessing' contentsSelection)
('testing' refusesToAccept)
!

