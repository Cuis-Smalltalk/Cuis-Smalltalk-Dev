'From Cuis6.3 [latest update: #6353] on 10 May 2024 at 4:42:36 pm'!

!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 5/10/2024 16:41:57'!
scanFrom: aStream
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."
	| methodSource |
	[
		methodSource := aStream nextChunk withCuisLineEndings.
		methodSource size > 0 ] whileTrue: [
			class compile: methodSource classified: category
				withStamp: changeStamp
				notifying: nil ]! !

!methodRemoval: ClassCategoryReader #theClass stamp: 'jmv 5/10/2024 16:39:45'!
ClassCategoryReader removeSelector: #theClass!
!methodRemoval: ClassCategoryReader #changeStamp stamp: 'jmv 5/10/2024 16:40:50'!
ClassCategoryReader removeSelector: #changeStamp!

!ClassCategoryReader reorganize!
('fileIn/Out' scanFrom:)
('private' setClass:category: setClass:category:changeStamp:)
!

