'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 13 January 2016 at 11:09:17.068393 am'!

!FileList methodsFor: 'private' stamp: 'jmv 1/13/2016 11:07'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first50000 |

directory // fileName readStream: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first50000 := f next: 50000.
].
	acceptedContentsCache _
'File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 50000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 50000 characters.' format: {fileName. fileSize. first50000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 1/13/2016 11:07'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData |

	directory // fileName readStream: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		((size _ stream size)) > 2000000 & brevity
			ifTrue: [ data _ stream next: 10000. brevityState := #briefHex ]
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

	^ acceptedContentsCache _ ((size > 2000000) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 10000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 10000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !

