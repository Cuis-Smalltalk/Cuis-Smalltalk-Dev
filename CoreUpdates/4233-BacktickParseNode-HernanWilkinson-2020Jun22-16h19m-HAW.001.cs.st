'From Cuis 5.0 [latest update: #4230] on 22 June 2020 at 4:43:45 pm'!
!methodRemoval: Scanner #canNotBeReferencedInsideBacktick: stamp: 'HAW 6/22/2020 16:40:39'!
Scanner removeSelector: #canNotBeReferencedInsideBacktick:!
!methodRemoval: Scanner #assertNodesIn:canBeReferencedInsideBacktickUsing: stamp: 'HAW 6/22/2020 16:40:31'!
Scanner removeSelector: #assertNodesIn:canBeReferencedInsideBacktickUsing:!

!Scanner reorganize!
('expression types - private' skipDelimiters)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('multi-character scans' isAt: readUpToNext:ifNotFound: typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' initScanner initScannerForTokenization scan:)
!

