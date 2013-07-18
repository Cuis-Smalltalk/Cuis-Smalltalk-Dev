'From Cuis 4.1 of 12 December 2012 [latest update: #1691] on 28 April 2013 at 10:20:47.518214 pm'!
!methodRemoval: StandardFileStream class #privateCheckForBrowserPrimitives!
StandardFileStream class removeSelector: #privateCheckForBrowserPrimitives!

!StandardFileStream class reorganize!
('file creation' crc16OfFileNamed: fileNamed: forceNewFileNamed: isAFileNamed: newFileNamed: oldFileNamed: readOnlyFileNamed:)
('registry' register: registry retryWithGC:until:forFileNamed: unregister:)
('error handling' fileDoesNotExistUserHandling: fileExistsUserHandling: readOnlyFileDoesNotExistUserHandling:)
!

!methodRemoval: StandardFileStream #defaultBrowserReadyWait!
StandardFileStream removeSelector: #defaultBrowserReadyWait!
!methodRemoval: StandardFileStream #post:target:url:ifError:!
StandardFileStream removeSelector: #post:target:url:ifError:!
!methodRemoval: StandardFileStream #post:url:ifError:!
StandardFileStream removeSelector: #post:url:ifError:!
!methodRemoval: StandardFileStream #primBrowserReady!
StandardFileStream removeSelector: #primBrowserReady!
!methodRemoval: StandardFileStream #primURLPost:data:semaIndex:!
StandardFileStream removeSelector: #primURLPost:data:semaIndex:!
!methodRemoval: StandardFileStream #primURLPost:target:data:semaIndex:!
StandardFileStream removeSelector: #primURLPost:target:data:semaIndex:!
!methodRemoval: StandardFileStream #primURLRequest:semaIndex:!
StandardFileStream removeSelector: #primURLRequest:semaIndex:!
!methodRemoval: StandardFileStream #primURLRequest:target:semaIndex:!
StandardFileStream removeSelector: #primURLRequest:target:semaIndex:!
!methodRemoval: StandardFileStream #primURLRequestDestroy:!
StandardFileStream removeSelector: #primURLRequestDestroy:!
!methodRemoval: StandardFileStream #primURLRequestFileHandle:!
StandardFileStream removeSelector: #primURLRequestFileHandle:!
!methodRemoval: StandardFileStream #primURLRequestState:!
StandardFileStream removeSelector: #primURLRequestState:!
!methodRemoval: StandardFileStream #requestURL:target:!
StandardFileStream removeSelector: #requestURL:target:!
!methodRemoval: StandardFileStream #requestURL:target:ifError:!
StandardFileStream removeSelector: #requestURL:target:ifError:!
!methodRemoval: StandardFileStream #requestURLStream:!
StandardFileStream removeSelector: #requestURLStream:!
!methodRemoval: StandardFileStream #requestURLStream:ifError:!
StandardFileStream removeSelector: #requestURLStream:ifError:!
!methodRemoval: StandardFileStream #waitBrowserReadyFor:ifFail:!
StandardFileStream removeSelector: #waitBrowserReadyFor:ifFail:!

!StandardFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' ascii binary getFileType isBinary isReadOnly readOnly readWrite setFileTypeToObject)
('access' directory file fullName isDirectory localName name peekFor: printOn: reset size)
('read, write, position' atEnd basicNext compressFile crc16 findString: findStringFromEnd: flush next next: next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextWordsInto: padToEndWith: peek peekLast position position: readInto:startingAt:count: readOnlyCopy setToEnd skip: truncate truncate: upTo: upToAny: upToEnd)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' actAsExecutor finalize)
('dnd requests' primDropRequestFileHandle: primDropRequestFileName: requestDropStream:)
('private' collectionSpecies disableReadBuffering enableReadBuffering flushReadBuffer)
!

!methodRemoval: FileStream class #post:target:url:ifError:!
FileStream class removeSelector: #post:target:url:ifError:!
!methodRemoval: FileStream class #post:url:ifError:!
FileStream class removeSelector: #post:url:ifError:!
!methodRemoval: FileStream class #requestURL:target:!
FileStream class removeSelector: #requestURL:target:!
!methodRemoval: FileStream class #requestURLStream:!
FileStream class removeSelector: #requestURLStream:!
!methodRemoval: FileStream class #requestURLStream:ifError:!
FileStream class removeSelector: #requestURLStream:ifError:!

!FileStream class reorganize!
('instance creation' fileNamed: forceNewFileNamed: fullName: isAFileNamed: new newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileNamed:)
('concrete classes' concreteStream)
('dnd requests' requestDropStream:)
('class initialization' unload)
('directory services' creationAndModificationTimeOf:)
!

