'From Cuis 6.0 [latest update: #5377] on 7 July 2022 at 9:27:58 pm'!

!FileList methodsFor: 'private' stamp: 'jmv 7/7/2022 21:25:09'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first part characters. Don't create a file if it doesn't already exist."
	| fileSize firstPart firstPartSize |
firstPartSize _ 1e6.
directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read' halt].
	(brevityFlag not or: [(fileSize _ f size) <= (1.2*firstPartSize) ]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState _ #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	firstPart := f next: 1e6.
].
	acceptedContentsCache _
'File ''{1}'' is {2} characters long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. fileSize. firstPartSize. firstPart}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !

