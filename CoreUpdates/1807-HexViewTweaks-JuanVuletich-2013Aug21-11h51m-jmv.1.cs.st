'From Cuis 4.2 of 25 July 2013 [latest update: #1806] on 21 August 2013 at 11:57:04.652426 am'!

!FileList methodsFor: 'private' stamp: 'jmv 8/21/2013 11:57'!
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| f size data hexData |

	f _ directory oldFileOrNoneNamed: self fullName. 
	f ifNil: [^ 'For some reason, this file cannot be read'].
	((size _ f size)) > 5000 & brevity
		ifTrue: [ data _ f next: 10000. f close. brevityState := #briefHex ]
		ifFalse: [ data _ f contentsOfEntireFile. brevityState := #fullHex ].

	hexData _ String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			s
				nextPutAll: (loc printStringBase: 16 length: 8 padded: true);
				space;
				nextPut: $(;
				nextPutAll: (loc printStringBase: 10 length: 10 padded: true);
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (data at: i) hex; space ].
			s newLine ]].

	^ acceptedContentsCache _ ((size > 5000) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])! !

