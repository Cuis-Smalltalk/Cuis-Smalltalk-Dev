'From Cuis7.1 [latest update: #6637] on 27 August 2024 at 10:43:08 am'!

!TextModel methodsFor: 'misc' stamp: 'jmv 8/27/2024 10:39:17'!
refetch
	"Nothing here. Answer true if actualContents was actually fetched."
	self actualContents: self actualContents.
	self changed: #refetched.! !


!PluggableTextModel methodsFor: 'misc' stamp: 'jmv 8/27/2024 10:39:29'!
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

