'From Cuis 6.0 [latest update: #5195] on 25 May 2022 at 5:39:54 pm'!

!Scanner methodsFor: 'public access' stamp: 'jmv 5/23/2022 17:15:50'!
scanTokenPositionsIn: textOrString into: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source _ textOrString asPlainString readStream.
	buffer _ source collectionSpecies writeStream.
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]'"! !

!Scanner methodsFor: 'initialization' stamp: 'jmv 5/23/2022 17:11:54'!
scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source _ inputStream.
	buffer _ source collectionSpecies writeStream.
	self step.
	self step.
	self scanToken! !

