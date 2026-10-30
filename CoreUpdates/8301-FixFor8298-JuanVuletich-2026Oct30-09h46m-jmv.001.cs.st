'From Cuis7.9 [latest update: #8300] on 30 October 2026 at 9:49:12 am'!

!TextModel methodsFor: 'misc' stamp: 'jmv 27/Aug/2024, 1:39:17 pm (UTC)'!
refetch
	"Nothing here. Answer true if actualContents was actually fetched."
	self actualContents: self actualContents.
	self changed: #refetched.! !


!PluggableTextModel methodsFor: 'misc' stamp: 'jmv 27/Aug/2024, 1:39:29 pm (UTC)'!
refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ]]
		ifNotNil: [
			self actualContents: (Text
				initialFont: (Preferences at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched ]! !

