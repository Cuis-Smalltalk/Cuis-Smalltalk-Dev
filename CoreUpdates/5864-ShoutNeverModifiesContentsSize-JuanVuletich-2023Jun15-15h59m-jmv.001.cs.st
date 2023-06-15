'From Cuis 6.0 [latest update: #5861] on 15 June 2023 at 4:01:00 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:00:22'!
privateConvertAssignments
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

	self parseText.
	self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

!methodRemoval: ObjectExplorer #shoutMustKeepContentsSize stamp: 'jmv 6/15/2023 16:00:33'!
ObjectExplorer removeSelector: #shoutMustKeepContentsSize!
!methodRemoval: Debugger #shoutMustKeepContentsSize stamp: 'jmv 6/15/2023 16:00:32'!
Debugger removeSelector: #shoutMustKeepContentsSize!
!methodRemoval: TextProvider #shoutMustKeepContentsSize stamp: 'jmv 6/15/2023 16:00:31'!
TextProvider removeSelector: #shoutMustKeepContentsSize!
!methodRemoval: PluggableTextModel #shoutMustKeepContentsSize stamp: 'jmv 6/15/2023 16:00:27'!
PluggableTextModel removeSelector: #shoutMustKeepContentsSize!
!methodRemoval: TextModel #shoutMustKeepContentsSize stamp: 'jmv 6/15/2023 16:00:31'!
TextModel removeSelector: #shoutMustKeepContentsSize!
