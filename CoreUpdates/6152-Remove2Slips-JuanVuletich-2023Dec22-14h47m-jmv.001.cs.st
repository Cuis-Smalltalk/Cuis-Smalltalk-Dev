'From Cuis6.0 [latest update: #6151] on 22 December 2023 at 2:50:11 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 12/22/2023 12:48:05'!
fileOutCurrentVersionsOfSelections

	self request: 'Enter file name' initialAnswer: 'CurrentVersions.st' do: [ :filename |
		DirectoryEntry fileOutsDirectory // filename writeStreamDo: [ :stream |
			stream timeStamp.
			self currentVersionsOfSelections do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: stream
					moveSource: false
					toFile: 0 ]]].! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 12/22/2023 14:49:20'!
open: model label: aString message: messageString
	|  window |

	window := self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString.
	aString ifNotNil: [ window setLabel: aString ].

	window openInWorld.! !

