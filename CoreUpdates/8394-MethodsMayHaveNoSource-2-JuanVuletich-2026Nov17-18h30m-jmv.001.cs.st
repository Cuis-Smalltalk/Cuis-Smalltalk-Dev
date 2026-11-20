'From Cuis7.9 [latest update: #8384] on 17 November 2026 at 6:33:06 pm'!

!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 17/Nov/2026, 9:32:21 pm (UTC)'!
createCodePaneMorph
	^ (TextModelMorph textProvider: model textGetter: #acceptedContents)
		emptyTextDisplayMessage: 'Nothing to show'.! !


!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 17/Nov/2026, 9:32:47 pm (UTC)'!
createCodePaneMorph
	^ (TextModelMorph textProvider: model textGetter: #acceptedContents)
		emptyTextDisplayMessage: 'Nothing to show'.! !

