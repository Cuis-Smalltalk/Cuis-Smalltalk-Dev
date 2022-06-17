'From Cuis 6.0 [latest update: #5302] on 17 June 2022 at 10:11:35 am'!

!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('testing' atEnd isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark copyMethodChunkFrom: evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('filein/out' copyPreamble:oldPreamble:)
('gui' untilEnd:displayingProgress:)
!

