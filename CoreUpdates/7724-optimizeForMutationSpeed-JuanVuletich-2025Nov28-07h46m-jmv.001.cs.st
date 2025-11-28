'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 7:46:41 am'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 28/Nov/2025 07:46:30'!
optimizeForMutationSpeed: rangesSize

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	| newFormattedText |
	"Not if we are obsoleted because #actualContents changed."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ rangesSize < 50 ]) ifFalse: [
			"What folows could take a little while"
			newFormattedText := formattedText optimizedForMutationSpeed.
			"if already #optimizedForMutationSpeed, do nothing"
			formattedText == newFormattedText ifFalse: [
				"We may be running in a background process... So check again, just in case actualContents changed in the mean time."
				formattedText == textModel actualContents ifTrue: [
					formattedText := newFormattedText.
					textModel basicActualContents: newFormattedText ]]]].

! !

