'From Cuis7.5 [latest update: #7716] on 26 November 2025 at 10:04:06 am'!

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 11/26/2025 10:02:31'!
fontPreferenceChanged
	
	super fontPreferenceChanged.
	self rescaleButtons.
	self morphExtent: extent. "To honor #minimumExtent"
	layoutMorph ifNotNil: [ 	"Title and border may have changed. So #interiorOrigin and #interiorExtent may have changed too."
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].
	model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.! !

