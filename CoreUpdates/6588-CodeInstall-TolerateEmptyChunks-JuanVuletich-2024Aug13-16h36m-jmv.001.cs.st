'From Cuis7.1 [latest update: #6587] on 13 August 2024 at 4:37:42 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'Install-toton 8/13/2024 16:34:26'!
compileNextChunkWhenStartsWithExclamationMark

	| chunk |
	
	chunk := self nextChunk.
	
	"These are the ones that should do nothing, 
	because next line is a doit that does the stuff
	(or because it is handled elsewhere)"
	
	(chunk isEmpty or: [(chunk beginsWith: 'description:  ') 
		or: [ ChangeList knownFileInPreambles anySatisfy: [ :aPreamble | chunk beginsWith: aPreamble ]]])
			ifFalse: [
				self evaluate: [ (Compiler evaluate: chunk logged: false) scanFrom: self ] printingErrorWith: chunk ].! !

