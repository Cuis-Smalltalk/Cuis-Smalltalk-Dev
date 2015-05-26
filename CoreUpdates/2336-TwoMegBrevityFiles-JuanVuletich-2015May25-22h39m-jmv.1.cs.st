'From Cuis 4.2 of 25 July 2013 [latest update: #2330] on 25 May 2015 at 10:40:02.796674 pm'!

!FileList methodsFor: 'private' stamp: 'jmv 5/25/2015 22:39'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| f fileSize first5000 |

	f := directory oldFileOrNoneNamed: self fullName.
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first5000 := f next: 5000.
	f close.
	acceptedContentsCache _ ('File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. fileSize. first5000}).
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 5/25/2015 22:39'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| f size data hexData |

	f _ directory oldFileOrNoneNamed: self fullName. 
	f ifNil: [^ 'For some reason, this file cannot be read'].
	((size _ f size)) > 2000000 & brevity
		ifTrue: [ data _ f next: 10000. f close. brevityState := #briefHex ]
		ifFalse: [ data _ f contentsOfEntireFile. brevityState := #fullHex ].

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

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !

