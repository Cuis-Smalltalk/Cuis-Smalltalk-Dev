'From Cuis7.1 [latest update: #6794] on 28 October 2024 at 9:09:48 pm'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 10/28/2024 21:07:36'!
assert: actual equals: expected description: aStringOrBlock
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
		description: aStringOrBlock.! !

