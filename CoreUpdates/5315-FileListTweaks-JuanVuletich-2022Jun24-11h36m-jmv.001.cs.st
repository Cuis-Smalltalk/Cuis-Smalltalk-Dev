'From Cuis 6.0 [latest update: #5311] on 24 June 2022 at 11:48:18 am'!

!FileList methodsFor: 'own services' stamp: 'jmv 6/24/2022 11:36:43'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"

	| aString aName |
	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream upToEnd.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName! !

!FileList methodsFor: 'private' stamp: 'jmv 6/24/2022 11:43:58'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first part characters. Don't create a file if it doesn't already exist."
	| fileSize firstPart firstPartSize |
firstPartSize _ 1e6.
directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read' halt].
	(brevityFlag not or: [(fileSize _ f size) <= (10*firstPartSize) ]) ifTrue: [
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

!FileList methodsFor: 'private' stamp: 'jmv 6/24/2022 11:46:02'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData firstPartSize |
	firstPartSize _ 1e6.
	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		((size _ stream size)) > (10*firstPartSize) & brevity
			ifTrue: [ data _ stream next: firstPartSize. brevityState := #briefHex ]
			ifFalse: [ data _ stream next: size. brevityState := #fullHex ]].

	hexData _ String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			loc printOn: s base: 16 length: 8 padded: true.
			s
				space;
				nextPut: $(.
			loc printOn: s base: 10 length: 10 padded: true.
			s
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (data at: i) hex; space ].
			s newLine ]].

	^ acceptedContentsCache _ ((size > (10*firstPartSize)) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. size. firstPartSize. hexData}]
		ifFalse: [hexData])! !

