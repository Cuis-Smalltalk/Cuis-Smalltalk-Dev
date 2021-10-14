'From Cuis 5.0 [latest update: #4923] on 14 October 2021 at 12:28:03 pm'!

!FileListWindow methodsFor: 'GUI building' stamp: 'hlsf 10/14/2021 12:27:46'!
morphicPatternPane

	^ (TextModelMorph
		textProvider: model
		textGetter: #pattern
		textSetter: #pattern:)
		acceptOnCR: true;
		yourself.! !

