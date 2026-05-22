[
| suite result exitCode |

Utilities classPool at: #AuthorName put: 'TonelTests'.
Utilities classPool at: #AuthorInitials put: 'TT'.

CodePackageFile installPackage: DirectoryEntry currentDirectory // 'Packages/Features/Tonel.pck.st'.
CodePackageFile installPackage: DirectoryEntry currentDirectory // 'Packages/Features/Tests-Tonel.pck.st'.

ChangeSet fileIn: DirectoryEntry currentDirectory // '.github/scripts/TestResultConsolePrinter.st'.

suite := TestSuite new.
(Smalltalk classNamed: #TonelWriterTest) addToSuiteFromSelectors: suite.
(Smalltalk classNamed: #TonelReaderTest) addToSuiteFromSelectors: suite.
(Smalltalk classNamed: #TonelImporterTest) addToSuiteFromSelectors: suite.
(Smalltalk classNamed: #TonelImageExporterTest) addToSuiteFromSelectors: suite.

result := suite run.
result printReport.

exitCode := (result hasFailures or: [ result hasErrors ]) ifTrue: [ 1 ] ifFalse: [ 0 ].
Smalltalk quitPrimitive: exitCode
] on: Error do: [ :ex |
	StdIOWriteStream stdout
		nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine;
		nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine;
		flush.
	Smalltalk quitPrimitive: 1
]
