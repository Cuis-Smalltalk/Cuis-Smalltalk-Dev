'From Cuis6.3 [latest update: #6352] on 10 May 2024 at 5:03:05 pm'!

!TestCase methodsFor: 'assertions' stamp: 'bp 5/10/2024 17:02:43'!
assert: actual equals: expected
	"First parameter is actual result from evaluating some expression.
	Second parameter is the value expected by the designer of the test.
	This differs from Squeak, but likely agrees with most other dialects and existing tests.
	See Martin's comments at
	https://lists.cuis.st/mailman/archives/cuis-dev/2023-July/007865.html
	Some tests may assume the previous behavior. Please correct them as you find them.
	"
	"
	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: expected to: actual tryWords: true))
			openLabel: 'Expected (red) versus actual (green)'
	"
	self
		assert: expected = actual
		description: [ self comparingStringBetween: expected and: actual ]
! !

